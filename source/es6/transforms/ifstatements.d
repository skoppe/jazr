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
import option;

version(unittest)
{
	import es6.parser;
	import es6.analyse;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
	Node parseModule(string input)
	{
		auto parser = parser(input);
		parser.scanToken();
		return parser.parseModule();
	}
}

// TODO: we can detect ifs faster by walking branches
bool combineNestedIfs(Node node)
{
	if (node.type != NodeType.IfStatementNode)
		return false;

	if (node.parent is null)
		return false;

	auto parent = node.as!IfStatementNode;

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

	combineExpressions(parent.condition, child.condition);

	child.truthPath.node.branch.remove();

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
		emit(got).shouldEqual(emit(expected));
	}
	assertCombineNestedIfs(
		"function a() { if (a) if (c && d) return bla; }",
		"function a() { if (a && c && d) return bla; }"
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
	assertCombineNestedIfs(
		`if (b()) if (c()) d()`,
		`if (b() && c()) d()`
	);
	// TODO: test cases where combining is not possible
}

// TODO: we can detect ifs faster by walking branches
bool convertIfsToExpressionStatements(Node node)
{
	if (node.type != NodeType.IfStatementNode)
		return false;
	auto ifStmt = node.as!IfStatementNode;

	if (ifStmt.hasElsePath)
		return false;

	if (ifStmt.truthPath.hints.has(Hint.NonExpression))
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
			expr.addChildren(ifStmt.truthPath.node.children);
		}
		expr.parent = null;
		result = combineExpressions(ifStmt.condition,expr);
	} else
	{
		ifStmt.truthPath.node.parent = null;
		result = combineExpressions(ifStmt.condition,ifStmt.truthPath.node);
	}
	ifStmt.replaceWith(result);

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
		got.runTransform!(convertIfsToExpressionStatements);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		import std.stdio;
		writeln(diff.getDiffMessage());
		emit(got).shouldEqual(emit(expected),file,line);
	}
	assertConvertIfsToExpressionStatements(
		"if (a) d = 5;",
		"a && (d = 5)"
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