/*
 * Jazr. A tool to parse, analyse and transform javascript modules
 * Copyright (C) 2016  Sebastiaan Koppe
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
module es6.transforms.ifstatements;

import es6.nodes;
import es6.scopes;
import es6.transforms.expressions;
import es6.transforms.conditionals;
import option;
import es6.analyse;
import es6.eval;
import std.range : retro;
import std.algorithm : until, each;
import std.array : array;

version(tracing)
{
	import es6.transformer;
	import std.datetime : StopWatch;
	import es6.bench;
}
version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}

bool combineNestedIfs(IfStatementNode parent, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (parent.hasElsePath)
		return false;

	IfStatementNode child;
	if (parent.truthPath.node.type == NodeType.IfStatementNode)
		child = parent.truthPath.node.as!IfStatementNode;
	else if (parent.truthPath.node.type == NodeType.BlockStatementNode)
	{
		if (parent.truthPath.node.children.length != 1)
			return false;
		if (parent.truthPath.node.children[0].type != NodeType.IfStatementNode)
			return false;
		child = parent.truthPath.node.children[0].as!IfStatementNode;
	} else
		return false;

	if (child.hasElsePath)
		return false;
	
	parent.truthPath.node.branch.hints |= child.truthPath.node.branch.hints;

	child.truthPath.node.branch.remove();

	combineExpressions(parent.condition, child.condition);

	child.truthPath.node.parent = null;
	parent.truthPath.node.replaceWith(child.truthPath.node);

	return true;
}

@("combineNestedIfs")
unittest
{
	void assertCombineNestedIfs(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		got.assertTreeInternals(file,line);
		got.runTransform!(combineNestedIfs);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
	assertCombineNestedIfs(
		"function a() { if (a) if (c && d) return bla; }",
		"function a() { if (a && c && d) return ook; }"
	).shouldThrow();
	assertCombineNestedIfs(
		"function a() { if (a) if (c && d) return bla; }",
		"function a() { if (a && c && d) return bla; }"
	);
	assertCombineNestedIfs(
		"function a() { if (a) { c = d; if (c && d) return bla; } }",
		"function a() { if (a) { c = d; if (c && d) return bla; } }"
	);
	/// ditto
	assertCombineNestedIfs(
		"function a() { if (a) { if (c) { return bla; } } }",
		"function a() { if (a && c) { return bla } }"
	);
	/// ditto
	assertCombineNestedIfs(
		"function a() { if (a || b) { if (c) { return bla; } } }",
		"function a() { if ((a || b) && c) { return bla } }"
	);
	/// ditto
	assertCombineNestedIfs(
		"function a() { if (a) { if (b || c) { return bla; } } }",
		"function a() { if (a && (b || c)) { return bla } }"
	);
	/// ditto
	assertCombineNestedIfs(
		"function a() { if (a || b) { if (c || d) { return bla; } } }",
		"function a() { if ((a || b) && (c || d)) { return bla } }"
	);
	/// ditto
	assertCombineNestedIfs(
		"function a() { if (a && b) { if (c || d) { return bla; } } }",
		"function a() { if (a && b && (c || d)) { return bla } }"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (b()) if (c()) d()`,
		`if (b() && c()) d()`
	);
	/// ditto
	assertCombineNestedIfs(
		`if (b()) if (c()) d(); else e()`,
		`if (b()) if (c()) d(); else e()`
	);
	/// ditto
	assertCombineNestedIfs(
		`if (b()) { if (c()) d() } else e()`,
		`if (b()) { if (c()) d() } else e()`
	);
}

bool convertIfsToExpressionStatements(IfStatementNode ifStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (ifStmt.hasElsePath)
		return false;

	if (ifStmt.truthPath.hints.has(Hint.NonExpression))
		return false; 

	if (!ifStmt.truthPath.hasStatements)
		return false;

	ifStmt.truthPath.branch.remove();
	Node result;
	if (ifStmt.truthPath.isBlockStatement)
	{
		Node expr;
		if (ifStmt.truthPath.children.length == 1)
			expr = ifStmt.truthPath.children[0];
		else {
			expr = new ExpressionNode([]);
			ifStmt.truthPath.node.children.each!((c){
				if (c.type == NodeType.ExpressionNode)
					expr.addChildren(c.children);
				else
					expr.addChild(c);
			});
			expr.reanalyseHints();
		}
		expr.parent = null;
		result = combineExpressions(ifStmt.condition,expr);
	} else
	{
		ifStmt.truthPath.node.parent = null;
		result = combineExpressions(ifStmt.condition,ifStmt.truthPath.node);
	}
	replacedWith = ifStmt.replaceWith(result);

	result.assignBranch(ifStmt.branch);

	result.reanalyseHints();

	return true;
}
@("convertIfsToExpressionStatements")
unittest
{
	void assertConvertIfsToExpressionStatements(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		got.assertTreeInternals(file,line);
		got.runTransform!(convertIfsToExpressionStatements)(file,line);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
	assertConvertIfsToExpressionStatements(
		"if (a) d = 5;",
		"a && (d = 5)"
	);
	assertConvertIfsToExpressionStatements(
		"if (a) d = 5;",
		"b && (d = 5)"
	).shouldThrow();
	assertConvertIfsToExpressionStatements(
		"function handly() { if (!a) { b = 4; d = 6;}}",
		"function handly() { !a && (b=4, d=6) }"
	);
	assertConvertIfsToExpressionStatements(
		`function handly() { if (!a) { b=4, d=6 } }`,
		`function handly() { !a && (b=4, d=6) }`
	);
	assertConvertIfsToExpressionStatements(
		"if (a) d = 5; if (b) g = 5;",
		"a && (d = 5); b && (g = 5)"
	);
	assertConvertIfsToExpressionStatements(
		"if (a) {d = 5; e = 5; if (b) g = 5;}",
		"a && (d = 5,e = 5,b && (g = 5))"
	);
	assertConvertIfsToExpressionStatements(
		"if (a) {d = 5, e = 5; if (b) g = 5;}",
		"a && (d = 5,e = 5,b && (g = 5))"
	);
	assertConvertIfsToExpressionStatements(
		`if (g) { a=5; if (e) d = 5; b = 5;}`,
		"g && (a = 5,e && (d = 5),b = 5)"
	);
	assertConvertIfsToExpressionStatements(
		`if (g) { (a = 5,e) && (d = 5); b = 5 }`,
		"g && ((a = 5,e) && (d = 5),b = 5)"
	);
	assertConvertIfsToExpressionStatements(
		`if (g) { if (a=6, e) d = 6; b = 6;}`,
		"g && ((a = 6,e) && (d = 6),b = 6)"
	);
	assertConvertIfsToExpressionStatements(
		`if (a) { b = 7; for(;;) ; }`,
		`if (a) { b = 7; for(;;) ; }`
	);
	assertConvertIfsToExpressionStatements(
		`if (a) { b = 7; } else { }`,
		`if (a) { b = 7; } else { }`
	);
	assertConvertIfsToExpressionStatements(
		`if (a) { b = 7; }`,
		`a && (b = 7)`
	);
	assertConvertIfsToExpressionStatements(
		`if ("use strict", b.can() && top === bottom) doThing();`,
		`("use strict",b.can() && top === bottom) && doThing()`
	);
}

/* combo

/// ditto
	assertCombineNestedIfs(
		`if (g) if (a=5, e) d = 5;`,
		"g && (a = 5,e) && (d = 5)"
	);
	assertCombineNestedIfs(
		`(t = 7,g && (a = 5,e)) && (d = 5)`,
		"t = 7,g && (a = 5,e) && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (t = 7, g) if (a=5, e) d = 5;`,
		"t = 7,g && (a = 5,e) && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (g || t) if (a=5, e) d = 5;`,
		"(g || t) && (a = 5,e) && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if ((a)) if ((b)) d = 5;`,
		"a && b && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (!(a)) if ((!b)) d = 5;`,
		"!a && !b && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (!a) if (!b) d = 5;`,
		"!a && !b && (d = 5)"
	);
	/// ditto
	assertCombineNestedIfs(
		`if (a=5) if (b) d = 5;`,
		"(a = 5) && b && (d = 5)"
	);
*/

bool removeEmptyIfPaths(IfStatementNode ifStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (ifStmt.truthPath.hasStatements)
	{
		if (!ifStmt.hasElsePath)
			return false;

		if (ifStmt.elsePath.hasStatements)
			return false;

		// truth path has statements, else path doesn't
		ifStmt.removeElsePath();
		return true;
	} else if (!ifStmt.hasElsePath)
	{
		// truth path has no statement, there is no else path
		auto cond = ifStmt.condition;
		ifStmt.truthPath.branch.remove();
		ifStmt.replaceWith(cond);
		cond.parent.reanalyseHints();
		return true;
	} else if (ifStmt.elsePath.hasStatements)
	{
		// truth path has no statement, but else path does
		// negate condition, replace truth with else path, remove (new) else path
		ifStmt.negateCondition();
		ifStmt.swapPaths();
		ifStmt.removeElsePath();
		return true;
	}
	// neither truth not else path has any statements
	// replace ifStmt with condition
	auto cond = ifStmt.condition;
	ifStmt.truthPath.branch.remove();
	ifStmt.elsePath.branch.remove();
	ifStmt.replaceWith(cond);
	cond.parent.reanalyseHints();
	return true;
}
@("removeEmptyIfPaths")
unittest
{
	alias assertRemoveEmptyPaths = assertTransformations!(removeEmptyIfPaths);
	assertRemoveEmptyPaths(
		`if (a) b(); else c();`,
		`if (a) b(); else c();`
	);
	assertRemoveEmptyPaths(
		`if (a) { } else { }`,
		`a`
	);
	assertRemoveEmptyPaths(
		`if (a) ; else ;`,
		`a`
	);
	assertRemoveEmptyPaths(
		`if (a) { } else b = 6;`,
		`if (!a) b = 6;`
	);
	assertRemoveEmptyPaths(
		`if (a) ; else b = 6;`,
		`if (!a) b = 6;`
	);
	assertRemoveEmptyPaths(
		`if (a) b = 6; else { }`,
		`if (a) b = 6;`
	);
	assertRemoveEmptyPaths(
		`if (a) b = 6; else ;`,
		`if (a) b = 6;`
	);
	assertRemoveEmptyPaths(
		`if (a) { }`,
		`a`
	);
	assertRemoveEmptyPaths(
		`if (a) ;`,
		`a`
	);
}
bool simplifyStaticIfStatement(IfStatementNode ifStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	auto value = ifStmt.condition.coerceToTernary;

	if (value == Ternary.None)
		return false;

	if (ifStmt.condition.type == NodeType.ExpressionNode)
	{
		auto walk = ifStmt.condition.children[$-1];
		while ((walk.type == NodeType.ExpressionNode ||
				walk.type == NodeType.ParenthesisNode))
			walk = walk.children[$-1];
		walk.detach();
		ifStmt.insertBefore(ifStmt.condition);
	}

	ifStmt.truthPath.branch.remove();
	Node parent = ifStmt.parent;
	if (value == Ternary.True)
	{
		if (ifStmt.hasElsePath)
			ifStmt.removeElsePath();
		replacedWith = ifStmt.replaceWith(ifStmt.truthPath.node);
		parent.reanalyseHints();
		return true;
	}
	if (ifStmt.hasElsePath)
	{
		ifStmt.elsePath.branch.remove();
		replacedWith = ifStmt.replaceWith(ifStmt.elsePath.node);
		parent.reanalyseHints();
		return true;
	}
	ifStmt.detach();
	return true;
}
@("simplifyStaticIfStatement")
unittest
{
	alias assertSimplifyStaticIf = assertTransformations!(simplifyStaticIfStatement);
	assertSimplifyStaticIf(
		`if (true) a`,
		`a`
	);
	assertSimplifyStaticIf(
		`if (true) a; else b`,
		`a`
	);
	assertSimplifyStaticIf(
		`if (!true) a; else b`,
		`b`
	);
	assertSimplifyStaticIf(
		`if (0 < 66) a; else b`,
		`a`
	);
	assertSimplifyStaticIf(
		`if (false) a`,
		``
	);
	assertSimplifyStaticIf(
		`if (false) a; else b`,
		`b`
	);
	assertSimplifyStaticIf(
		`if (a, true) b; else c`,
		`a; b`
	);
	assertSimplifyStaticIf(
		`if (a, false) b; else c`,
		`a; c`
	);
	assertSimplifyStaticIf(
		`if (a) { if (true) { d=5,p=6 } else { g=5 } }`,
		`if (a) { { d=5,p=6 } }`
	);
	assertSimplifyStaticIf(
		`if (a) { if (false) { d=5,p=6 } else { g=5 } }`,
		`if (a) { { g=5 } }`
	);
	assertSimplifyStaticIf(
		`if (0) a();`,
		``
	);
	assertSimplifyStaticIf(
		`if (bla(),0) a();`,
		`bla()`
	);
	assertSimplifyStaticIf(
		`if ("0") a();`,
		`a();`
	);
	assertSimplifyStaticIf(
		`if (bla(),"0") a();`,
		`bla();a();`
	);
	assertSimplifyStaticIf(
		`if (bla(),(hup(),"0")) a();`,
		`bla(),(hup());a();`
	);
}

bool moveExpressionsIntoIfCond(IfStatementNode ifStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (ifStmt.parent.type != NodeType.BlockStatementNode &&
		ifStmt.parent.type != NodeType.ModuleNode &&
		ifStmt.parent.type != NodeType.FunctionBodyNode)
		return false;
	auto idx = ifStmt.parent.getIndexOfChild(ifStmt);
	if (idx == 0)
		return false;
	auto candidates = ifStmt.parent.children[0..idx];
	auto exprs = candidates.retro.until!(c => c.hints.has(Hint.NonExpression)).array;
	if (exprs.length == 0)
		return false;
	exprs.each!(expr => expr.detach());
	auto cond = ifStmt.condition;
	if (cond.type != NodeType.ExpressionNode)
		cond.replaceWith(new ExpressionNode([])).addChild(cond);
	cond = ifStmt.condition;
	foreach(expr; exprs)
		if (expr.type == NodeType.ExpressionNode)
			cond.prependChildren(expr.children);
		else
			cond.prependChildren([expr]);
	cond.reanalyseHints();
	ifStmt.parent.reanalyseHints();
	return true;
}

@("moveExpressionsIntoIfCond")
unittest
{
	alias assertMoveIntoIf = assertTransformations!(moveExpressionsIntoIfCond);
	assertMoveIntoIf(
		`bla(); if(a) foo();`,
		`if(bla(), a) foo();`
	);
	assertMoveIntoIf(
		`b = 6; if(a) foo();`,
		`if(b = 6, a) foo();`
	);
	assertMoveIntoIf(
		`bla(), hup(); if(a) foo();`,
		`if(bla(), hup(), a) foo();`
	);
	assertMoveIntoIf(
		`bla(); hup(); if(a) foo();`,
		`if(bla(), hup(), a) foo();`
	);
	assertMoveIntoIf(
		`bla(), bar(); hup(); if(a) foo();`,
		`if(bla(), bar(), hup(), a) foo();`
	);
	assertMoveIntoIf(
		`if(b) kaz(); bla(), bar(); hup(); if(a) foo();`,
		`if(b) kaz(); if(bla(), bar(), hup(), a) foo();`
	);
	assertMoveIntoIf(
		`if(b) kaz(); if(a) foo();`,
		`if(b) kaz(); if(a) foo();`
	);
	assertMoveIntoIf(
		`bla(), hup(); if(a) foo(); bla(), hup(); if(a) foo();`,
		`if(bla(), hup(), a) foo(); if(bla(), hup(), a) foo();`
	);
	assertMoveIntoIf(
		`bla(); hup(); if(a, b) foo();`,
		`if(bla(), hup(), a, b) foo();`
	);
	assertMoveIntoIf(
		`bla(), hup(); if(a, b) foo();`,
		`if(bla(), hup(), a, b) foo();`
	);
}



