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

version(unittest)
{
	import es6.parser;
	import es6.analyse;
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
		got.runTransform!(convertIfElseAssignmentToConditionalExpression);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected));
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
		emit(got).shouldEqual(emit(expected));
	}
}

// TODO: we can detect ifs faster by walking branches
bool convertIfElseAssignmentToConditionalExpression(Node node)
{
	if (node.type != NodeType.IfStatementNode)
		return false;

	IfStatementNode ifStmt = node.as!IfStatementNode;

	if (!ifStmt.hasElsePath)
		return false;

	if (ifStmt.truthPath.hints.has(Hint.NonExpression | Hint.Return | Hint.ReturnValue))
		return false;
	if (ifStmt.elsePath.hints.has(Hint.NonExpression | Hint.Return | Hint.ReturnValue))
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

	Node r = new AssignmentExpressionNode([truthAssign,
		new AssignmentOperatorNode(Assignment.Assignment),
		new ConditionalExpressionNode(
			condition,
			ifStmt.truthPath.convertToAssignmentExpression,
			ifStmt.elsePath.convertToAssignmentExpression
		)
	]);

	if (condition.type == NodeType.ExpressionNode)
	{
		auto preExprs = condition.children[0..$-1];
		condition.replace(condition.children[$-1]);
		preExprs ~= r;
		r = new ExpressionNode(preExprs);
	}

	r.assignBranch(ifStmt.branch);
	ifStmt.replace(r);
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
	assertNegateCondition(`if (45 > 46) b;`,`if (!(45 > 46)) b;`);
	assertNegateCondition(`if (b > 6) b = 5;`,`if (!(b > 6)) b = 5;`);
	assertNegateCondition(`if (c) b = 5;`,`if (!c) b = 5;`);
	assertNegateCondition(`if (!!a) b = 5;`,`if (!a) b = 5;`);
	assertNegateCondition(`if (!(a && b)) b = 5;`,`if ((a && b)) b = 5;`);
	assertNegateCondition(`if (a && b) b = 5;`,`if (!(a && b)) b = 5;`);
}


