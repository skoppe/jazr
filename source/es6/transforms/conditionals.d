/**
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
module es6.transforms.conditionals;

import es6.nodes;
import es6.scopes;
import es6.analyse;
import es6.eval;
import es6.transforms.expressions;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;

	void assertIfElseAssignmentToConditional(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		auto trunkA = got.analyseNode().trunk;
		auto trunkB = expected.analyseNode().trunk;
		got.runTransform!(convertIfElseAssignmentToConditionalExpression)(file,line);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}

	void assertNegateCondition(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto got = parseNode!("parseIfStatement",IfStatementNode)(input);
		auto expected = parseNode!("parseIfStatement",IfStatementNode)(output);
		auto trunkA = got.analyseNode().trunk;
		auto trunkB = expected.analyseNode().trunk;
		got.negateCondition();
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
}

bool convertIfElseAssignmentToConditionalExpression(IfStatementNode ifStmt, out Node replacedWith)
{
	if (!ifStmt.hasElsePath)
		return false;

	if (ifStmt.truthPath.hints.has(Hint.NonExpression | Hint.Return | Hint.ReturnValue))
		return false;
	if (ifStmt.elsePath.hints.has(Hint.NonExpression | Hint.Return | Hint.ReturnValue))
		return false;

	if (!ifStmt.truthPath.hasStatements ||
		!ifStmt.elsePath.hasStatements)
		return false;

	auto truthAssignExpr = ifStmt.truthPath.getLastStatement();
	auto falseAssignExpr = ifStmt.elsePath.getLastStatement();

	if (truthAssignExpr.type == NodeType.ExpressionNode)
		truthAssignExpr = truthAssignExpr.children[$-1];
	if (falseAssignExpr.type == NodeType.ExpressionNode)
		falseAssignExpr = falseAssignExpr.children[$-1];

	if (truthAssignExpr.type != NodeType.AssignmentExpressionNode ||
		falseAssignExpr.type != NodeType.AssignmentExpressionNode)
		return false;

	auto truthAssign = truthAssignExpr.children[0];
	auto falseAssign = falseAssignExpr.children[0];

	if (truthAssign.type != NodeType.IdentifierNode ||
		truthAssign.diff(falseAssign) != Diff.No)
		return false;

	ifStmt.truthPath.branch.remove();
	ifStmt.elsePath.branch.remove();

	auto condition = ifStmt.condition();

	truthAssignExpr.as!(AssignmentExpressionNode).removeFirstAssignment();
	falseAssignExpr.as!(AssignmentExpressionNode).removeFirstAssignment();

	Node oper = new AssignmentOperatorNode(Assignment.Assignment);
	auto cond = new ConditionalExpressionNode(
			condition,
			ifStmt.truthPath.convertToAssignmentExpression,
			ifStmt.elsePath.convertToAssignmentExpression
		);
	Node r = new AssignmentExpressionNode([truthAssign,
		oper,
		cond
	]);
	cond.reanalyseHints();
	oper.reanalyseHints();
	r.reanalyseHints();

	if (condition.type == NodeType.ExpressionNode)
	{
		auto preExprs = condition.children[0..$-1];
		condition.replaceWith(condition.children[$-1]);
		preExprs ~= r;
		r = new ExpressionNode(preExprs);
		r.reanalyseHints();
	}

	r.assignBranch(ifStmt.branch);
	replacedWith = ifStmt.replaceWith(r);
	return true;
}
@("convertIfElseAssignmentToConditionalExpression")
unittest
{
	assertIfElseAssignmentToConditional(
		`if (a) b = 6;`,
		`something_that_should_fail`
	).shouldThrow();
	assertIfElseAssignmentToConditional(
		`if (a) b = 6; else b = 7;`,
		`b = a ? 6 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if (a) c(),b = 6; else d(),b = 7;`,
		`b=a?(c(),6):(d(),7);`
	);
	assertIfElseAssignmentToConditional(
		`if (a) c(); else b = 7;`,
		`if (a) c(); else b = 7;`
	);
	assertIfElseAssignmentToConditional(
		`if (a) b = 7; else c();`,
		`if (a) b = 7; else c();`
	);
	assertIfElseAssignmentToConditional(
		`if (a) { b = 6; } else { b = 7; }`,
		`b = a ? 6 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if (a) b = 6; else { b = 7; }`,
		`b = a ? 6 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if (a) { b = 6; } else b = 7;`,
		`b = a ? 6 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if (a) { d = 5; b = 6; } else {g = 12; b = 7};`,
		`b = a ? (d = 5,6) : (g = 12,7)`
	);
	assertIfElseAssignmentToConditional(
		`if (a) b = 7; else b = 8; if (c) d = 6; else d = 9;`,
		`b = a ? 7 : 8; d = c ? 6 : 9`
	);
	assertIfElseAssignmentToConditional(
		`if (c) d = 6; else g = 9;`,
		`if (c) d = 6; else g = 9;`
	);
	assertIfElseAssignmentToConditional(
		`if (c) { for(;;) ; d = 6; } else d = 9;`,
		`if (c) { for(;;) ; d = 6; } else d = 9;`
	);
	assertIfElseAssignmentToConditional(
		`if (c) d = 9; else { for(;;) ; d = 6; }`,
		`if (c) d = 9; else { for(;;) ; d = 6; }`
	);
	assertIfElseAssignmentToConditional(
		`if (a) c = b = 6; else c = b = 7;`,
		`c = a ? b = 6 : b = 7`
	);
	assertIfElseAssignmentToConditional(
		`if (doFun(), a) { b = 6; } else { b = 7; }`,
		`doFun(),b = a ? 6 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if(y) b=2; else if(k) b=5; else b=7;`,
		`b = y ? 2 : k ? 5 : 7`
	);
	assertIfElseAssignmentToConditional(
		`if(y) { } else if(k) b=5; else b=7;`,
		`if(y) { } else b = k ? 5 : 7;`
	);
	assertIfElseAssignmentToConditional(
		`if(y) ; else if(k) b=5; else b=7;`,
		`if(y) ; else b = k ? 5 : 7;`
	);
}
void negateBinaryExpression(BinaryExpressionNode node)
{
	auto paren = new ParenthesisNode();
	node.replaceWith(new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Negation)],paren)).withBranch(node.branch);
	paren.addChild(node);
}
void negateUnaryExpression(UnaryExpressionNode node)
{
	if (node.prefixs.length > 0 && node.prefixs[0].type == NodeType.PrefixExpressionNode)
	{
		auto prefix = node.prefixs[0].as!PrefixExpressionNode;
		if (prefix.prefix == Prefix.Negation)
		{
			if (node.prefixs.length == 1)
			{
				node.replaceWith(node.children[0]);
				return;
			}
			node.prefixs = node.prefixs[1..$];
			return;
		}
	}
}
void negateNode(Node node)
{
	if (node.hints.has(Hint.HasAssignment))
		node = node.parenthesizeExpression();
	auto unary = new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Negation)]).withBranch(node.branch);
	node.replaceWith(unary);
	unary.addChild(node);
}
void negateCondition(IfStatementNode node)
{
	auto bin = node.condition().opt!(BinaryExpressionNode);
	if (bin.isDefined)
	{
		negateBinaryExpression(bin.get);
		return;
	}
	auto unary = node.condition().opt!(UnaryExpressionNode);
	if (unary.isDefined)
	{
		negateUnaryExpression(unary.get);
		return;
	}
	negateNode(node.condition());
	return;
}
@("negateCondition")
unittest
{
	assertNegateCondition(
		`if (a) b = 6;`,
		`if (c) something_that_should_fail`
	).shouldThrow();
	assertNegateCondition(`if (45 > 46) b;`,`if (!(45 > 46)) b;`);
	assertNegateCondition(`if (b > 6) b = 5;`,`if (!(b > 6)) b = 5;`);
	assertNegateCondition(`if (c) b = 5;`,`if (!c) b = 5;`);
	assertNegateCondition(`if (!!a) b = 5;`,`if (!a) b = 5;`);
	assertNegateCondition(`if (!(a && b)) b = 5;`,`if ((a && b)) b = 5;`);
	assertNegateCondition(`if (a && b) b = 5;`,`if (!(a && b)) b = 5;`);
	assertNegateCondition(`if (a = 5) b = 5;`,`if (!(a = 5)) b = 5;`);
}

bool convertIfElseToConditionalExpression(IfStatementNode ifStmt, out Node replacedWith)
{
	if (!ifStmt.hasElsePath)
		return false;

	if (ifStmt.truthPath.hints.has(Hint.NonExpression) ||
		ifStmt.elsePath.hints.has(Hint.NonExpression))
		return false;

	if (!ifStmt.truthPath.hasStatements ||
		!ifStmt.elsePath.hasStatements)
		return false;

	auto condition = ifStmt.condition;
	if (condition.hints.has(Hint.HasAssignment))
		condition = condition.parenthesizeExpression();

	auto left = new ExpressionNode([]);
	auto right = new ExpressionNode([]);

	ifStmt.truthPath.branch.remove();
	ifStmt.elsePath.branch.remove();

	void copyChildrenToExpression(Node node, ExpressionNode expr)
	{
		if (node.type == NodeType.BlockStatementNode)
			node.children.each!(c => expr.addExpression(c));
		else
			expr.addExpression(node);
	}

	copyChildrenToExpression(ifStmt.truthPath.node, left);
	copyChildrenToExpression(ifStmt.elsePath.node, right);

	left.reanalyseHints();
	right.reanalyseHints();

	Node l = left.children.length == 1 ? left.children[0] : left.parenthesizeExpression();
	Node r = right.children.length == 1 ? right.children[0] : right.parenthesizeExpression();


	auto cond = new ConditionalExpressionNode(condition, l, r);
	replacedWith = ifStmt.replaceWith(cond);

	cond.reanalyseHints();

	return true;
}

@("convertIfElseToConditionalExpression")
unittest
{
	alias assertConvertIfElseToConditional = assertTransformations!(convertIfElseToConditionalExpression);

	/// Issue #86
	assertConvertIfElseToConditional(
		`if (a) b = 6; else { for(;;)break; b = 7 }`,
		`if (a) b = 6; else { for(;;)break; b = 7 }`
	);
	/// ditto
	assertConvertIfElseToConditional(
		`function doo() { if (a) b = 6; else { if (d) return 7; b = 7; } }`,
		`function doo() { if (a) b = 6; else { if (d) return 7; b = 7 } }`
	);

	assertConvertIfElseToConditional(
		`if (bla) doB(); else { goP() }`,
		`bla ? doB() : goP()`
	);
	assertConvertIfElseToConditional(
		`if (bla) { } else { goP() }`,
		`if (bla) { } else { goP() }`
	);
	assertConvertIfElseToConditional(
		`if (bla) ; else { goP() }`,
		`if (bla) ; else { goP() }`
	);
	assertConvertIfElseToConditional(
		`if (bla) { goP() } else { }`,
		`if (bla) { goP() } else { }`
	);
	assertConvertIfElseToConditional(
		`if (bla) { goP() } else ;`,
		`if (bla) { goP() } else ;`
	);
	assertConvertIfElseToConditional(
		`if (a) { g = 6; } else k = 7;`,
		"a ? g = 6 : k = 7"
	);
	assertConvertIfElseToConditional(
		`if (a || b) { g = 6; } else { k = 7; }`,
		"a || b ? g = 6 : k = 7"
	);
	assertConvertIfElseToConditional(
		`if (a) g = 6; else k = 7;`,
		"a ? g = 6 : k = 7"
	);
	assertConvertIfElseToConditional(
		`if (a) g = 6; else if (b) k = 7; else i = 9;`,
		"a ? g = 6 : b ? k = 7 : i = 9;"
	);
	assertConvertIfElseToConditional(
		`if (a = 6) doB(); else { goP() }`,
		`(a = 6) ? doB() : goP()`
	);
	/// ditto
	assertConvertIfElseToConditional(
		`if (a) { e(); g = 6; } else { f(); k = 7; }`,
		"a ? (e(),g = 6) : (f(),k = 7)"
	);
	/// ditto
	assertConvertIfElseToConditional(
		`if (a) e(), g = 6; else f(), k = 7;`,
		"a ? (e(),g = 6) : (f(),k = 7)"
	);
	/// ditto
	assertConvertIfElseToConditional(
		`if (a) { g = 6; } else { k = 7; }`,
		"a ? g = 6 : k = 7"
	);
	// TODO: there is also precedence situation with yield operator
}

bool simplifyStaticConditional(ConditionalExpressionNode cond, out Node replacedWith)
{
	auto value = cond.condition.coerceToTernary;

	if (value == Ternary.None)
		return false;

	if (cond.condition.type == NodeType.ExpressionNode || cond.condition.type == NodeType.ParenthesisNode)
	{
		auto walk = cond.condition.children[$-1];
		while ((walk.type == NodeType.ExpressionNode ||
				walk.type == NodeType.ParenthesisNode))
			walk = walk.children[$-1];
		walk.detach();
		cond.insertBefore(cond.condition);
	}

	auto parent = cond.parent;
	Node node;
	if (value == Ternary.True)
		node = cond.truthPath;
	else
		node = cond.elsePath;

	replacedWith = cond.replaceWith(node);
	return true;
}

@("simplifyStaticConditional")
unittest
{
	alias assertSimplifyStaticConditional = assertTransformations!(simplifyStaticConditional);

	assertSimplifyStaticConditional(
		`a ? b : c`,
		`a ? b : c`
	);
	assertSimplifyStaticConditional(
		`true ? b : c`,
		`b`
	);
	assertSimplifyStaticConditional(
		`false ? b : c`,
		`c`
	);
	assertSimplifyStaticConditional(
		`!false ? b : c`,
		`b`
	);
	assertSimplifyStaticConditional(
		`66 > 9 ? b : c`,
		`b`
	);
	assertSimplifyStaticConditional(
		`true ? (b,c) : (d,e)`,
		`(b,c)`
	);
	assertSimplifyStaticConditional(
		`false ? (b,c) : (d,e)`,
		`(d,e)`
	);
	assertSimplifyStaticConditional(
		`0 ? (b,c) : (d,e)`,
		`(d,e)`
	);
	assertSimplifyStaticConditional(
		`"0" ? (b,c) : (d,e)`,
		`(b,c)`
	);
	assertSimplifyStaticConditional(
		`bla(),false ? (b,c) : (d,e)`,
		`bla(),(d,e)`
	);
	assertSimplifyStaticConditional(
		`(bla(),false) ? (b,c) : (d,e)`,
		`(bla());(d,e)`
	);
}
