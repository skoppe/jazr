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
module es6.transforms.assignmentexpressions;

import es6.nodes;
import std.algorithm : map, each;
import std.array : array;

version (unittest)
{
	import unit_threaded;
	import es6.parser;
	import es6.emitter;
	import es6.transformer;
	import es6.analyse;
	import std.stdio;
}

bool simplifyRedundantAssignmentExpressions(ExpressionNode expr, out Node replacedWith)
{
	if (expr.isSingleExpression) {
		import std.stdio;
		writeln(expr.parent);
	}
	assert(!expr.isSingleExpression);

	if (expr.children[$-2].type != NodeType.AssignmentExpressionNode)
		return false;

	auto assignExpr = expr.children[$-2].as!AssignmentExpressionNode;
	Node lhs = assignExpr.children[0];

	if (expr.children[$-1].type == NodeType.IdentifierReferenceNode)
	{
		if (lhs.type != NodeType.IdentifierReferenceNode)
			return false;

		if (lhs.as!IdentifierReferenceNode.identifier != expr.children[$-1].as!IdentifierReferenceNode.identifier)
			return false;
	} else if (expr.children[$-1].type == NodeType.CallExpressionNode)
	{
		if (lhs.type != NodeType.CallExpressionNode)
			return false;

		if (diffTree(lhs,expr.children[$-1]).type != Diff.No)
			return false;
	} else
		return false;

	expr.children = expr.children[0..$-1];

	if (expr.children.length == 1)
		replacedWith = expr.replaceWith(expr.children[0]);
	return true;
}

@("simplifyRedundantAssignmentExpressions")
unittest
{
	void assertTransformation(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		got.runTransform!(simplifyRedundantAssignmentExpressions);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
	assertTransformation
	(
		`if (a = 5) doBla();`,
		`if (a = 5) doBla();`
	);
	assertTransformation
	(
		`if (a = 5, a) doBla();`,
		`if (a = 5) doBla();`
	);
	assertTransformation
	(
		`if (a.bla = 5, a.bla) doBla();`,
		`if (a.bla = 5) doBla();`
	);
	assertTransformation
	(
		`if (a[4] = 5, a[4]) doBla();`,
		`if (a[ 4 ] = 5) doBla();`
	);
	assertTransformation
	(
		`if (a.bla[4].tar = 5, a.bla[4].tar) doBla();`,
		`if (a.bla[ 4 ].tar = 5) doBla();`
	);

	assertTransformation
	(
		`if (a = 5, b) doBla();`,
		`if (a = 5,b) doBla();`
	);
	assertTransformation
	(
		`if (a = 5, a.bla) doBla();`,
		`if (a = 5,a.bla) doBla();`
	);
	assertTransformation
	(
		`if (a.bla = 5, a.ops) doBla();`,
		`if (a.bla = 5,a.ops) doBla();`
	);
	assertTransformation
	(
		`if (a[4] = 5, a[5]) doBla();`,
		`if (a[ 4 ] = 5,a[ 5 ]) doBla();`
	);

	assertTransformation
	(
		`function a() { return b = 7, b }`,
		`function a() { return b = 7 }`
	);
	assertTransformation
	(
		`function a() { return b = 7, b ? ab() : ko() }`,
		`function a() { return b = 7,b ? ab() : ko() }`
	);
	assertTransformation
	(
		`if (a.bla() = 4,a) ;`,
		`if (a.bla() = 4,a) ;`
	);
	assertTransformation
	(
		`if (a = 4,a()) ;`,
		`if (a = 4,a()) ;`
	);
	assertTransformation
	(
		`if (super.a = 4,a) ;`,
		`if (a = 4,super.a) ;`
	).shouldThrow();
	assertTransformation
	(
		`if (123 = 4,123) ;`,
		`if (123 = 4,123) ;`
	);
	assertTransformation
	(
		`if (b,c=56,d(),g=77,g) ;`,
		`if (b,c=56,d(),g=77) ;`
	);
}