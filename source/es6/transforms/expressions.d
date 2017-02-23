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
module es6.transforms.expressions;

import es6.nodes;
import es6.scopes;
import es6.transforms.conditionals;
import option;
import es6.analyse;
import es6.eval;
import std.range : retro, enumerate;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}

Node parenthesizeExpression(Node a)
{
	auto paren = new ParenthesisNode();
	a.replaceWith(paren);
	paren.addChild(a);
	reanalyseHints(paren);
	return paren;
}

Node combineExpressions(Node a, Node b)
{
	if (a.hints.has(Hint.Or) || a.type == NodeType.ConditionalExpressionNode || !a.opt!(ExpressionNode).isSingleExpression.getOrElse(true) ||
		a.type == NodeType.AssignmentExpressionNode)
		a = parenthesizeExpression(a);

	if (b.hints.has(Hint.Or) || b.type == NodeType.ConditionalExpressionNode || !b.opt!(ExpressionNode).isSingleExpression.getOrElse(true) ||
		b.type == NodeType.AssignmentExpressionNode)
		b = parenthesizeExpression(b);

	if (a.type == NodeType.BinaryExpressionNode)
	{
		if (b.type == NodeType.BinaryExpressionNode)
		{
			a.addChild(new ExpressionOperatorNode(ExpressionOperator.LogicalAnd));
			a.addChildren(b.children);
		} else
		{
			b.assignBranch(a.branch);
			a.addChildren([new ExpressionOperatorNode(ExpressionOperator.LogicalAnd), b]);
		}
		a.reanalyseHints();
		return a;
	} else if (b.type == NodeType.BinaryExpressionNode)
	{
		b.parent = null;
		b.assignBranch(a.branch);
		a.replaceWith(b).prependChildren([a, new ExpressionOperatorNode(ExpressionOperator.LogicalAnd)]);
		b.reanalyseHints();
		return b;
	}
	else
	{
		auto bin = new BinaryExpressionNode([]);
		b.assignBranch(a.branch);
		a.replaceWith(bin).addChildren([a, new ExpressionOperatorNode(ExpressionOperator.LogicalAnd), b]);
		bin.reanalyseHints();
		return bin;
	}
}

@("combineExpressions")
unittest {
	void assertCombineExpressions(string a, string b, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node astA = parseModule(a);
		Node astB = parseModule(b);
		Node expected = parseModule(output);
		astA.analyseNode();
		astB.analyseNode();

		Node got = combineExpressions(astA.children[0], astB.children[0]);
		expected.analyseNode();
		auto diff = diffTree(astA,expected);
		got.assertTreeInternals(file,line);

		if (diff.type == Diff.No)
			return;

		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
	assertCombineExpressions(`a`,`b`,`a or b`).shouldThrow();
	assertCombineExpressions(`a`,`b`,`a && b`);
	assertCombineExpressions(`a && b`,`c`,`a && b && c`);
	assertCombineExpressions(`a`,`b && c`,`a && b && c`);
	assertCombineExpressions(`a && b`,`c && d`,`a && b && c && d`);
	assertCombineExpressions(`a || b`,`c`,`(a || b) && c`);
	assertCombineExpressions(`a`,`b || c`,`a && (b || c)`);
	assertCombineExpressions(`a ? 1 : 2`,`b`,`(a ? 1 : 2) && b`);
	assertCombineExpressions(`a`,`b ? 1 : 2`,`a && (b ? 1 : 2)`);
	assertCombineExpressions(`a, b`,`c`,`(a, b) && c`);
	assertCombineExpressions(`a`,`b, c`,`a && (b, c)`);
	assertCombineExpressions(`+a`,`-b`,`+a && -b`);
	assertCombineExpressions(`!a`,`~b`,`!a && ~b`);
	assertCombineExpressions(`a`,`b = 6`,`a && (b = 6)`);

}

bool combineBlockStatementIntoExpression(BlockStatementNode node, out Node replacedWith)
{
	return combineStatementsIntoExpression(node,replacedWith);
}
bool combineFunctionBodyIntoExpression(FunctionBodyNode node, out Node replacedWith)
{
	return combineStatementsIntoExpression(node,replacedWith);
}
bool combineModuleIntoExpression(ModuleNode node, out Node replacedWith)
{
	return combineStatementsIntoExpression(node,replacedWith);
}

private bool combineStatementsIntoExpression(Node node, out Node replacedWith)
{
	assert(node.type == NodeType.BlockStatementNode || node.type == NodeType.FunctionBodyNode || node.type == NodeType.ModuleNode);

	auto length = node.children.length;
	if (length < 2)
		return false;

	bool didWork = false;
	for (size_t idx = 0; idx < length-1;)
	{
		auto a = node.children[idx];
		auto b = node.children[idx+1];

		if (a.hints.has(Hint.NonExpression) || a.type == NodeType.EmptyStatementNode)
		{
			idx += 1;
			continue;
		}
		if (b.hints.has(Hint.NonExpression) || b.type == NodeType.EmptyStatementNode)
		{	
			idx += 2;
			continue;
		}

		didWork = true;
		length--;
		b.detach();
		auto parent = a.parent;
		if (a.type == NodeType.ExpressionNode)
		{
			if (b.type == NodeType.ExpressionNode)
				a.addChildren(b.children);
			else
				a.addChild(b);
			a.reanalyseHints();
		} else if (b.type == NodeType.ExpressionNode)
		{
			parent.replaceChild(a, b);
			b.prependChildren([a]);
			b.reanalyseHints();
		} else
		{
			auto expr = new ExpressionNode([a,b]);
			parent.replaceChild(a, expr);
			expr.reanalyseHints();
		}
	}

	return didWork;
}

@("combineStatementsIntoExpression")
unittest
{
	alias assertCombineStatements = assertTransformations!(combineBlockStatementIntoExpression,combineFunctionBodyIntoExpression,combineModuleIntoExpression);
	assertCombineStatements(
		`a();b();`,
		`a(),b()`
	);
	assertCombineStatements(
		`;;;`,
		`;;;`
	);
	assertCombineStatements(
		`a = 6; b = 7, c = 8;`,
		`a = 6, b = 7, c = 8`
	);
	assertCombineStatements(
		`a ? b() : c(); e && f && (d = 6); g = {a: 4}; h = [0,1]; (i || j) && g(); a = /^(?:webkit|moz|o)[A-Z]/.test("");`,
		`a ? b() : c(), e && f && (d = 6), g = {a: 4}, h = [0,1], (i || j) && g(), a = /^(?:webkit|moz|o)[A-Z]/.test("");`
	);
}
bool simplifyUnaryExpressions(UnaryExpressionNode node, out Node replacedWith)
{
	auto raw = node.getRawValue();
	switch (raw.type)
	{
		case ValueType.Undefined:
			if (node.prefixs.length > 0 && node.prefixs[0].type == NodeType.PrefixExpressionNode && node.prefixs[0].as!(PrefixExpressionNode).prefix == Prefix.Void)
				return false;
			goto case ValueType.String;
		case ValueType.NaN:
		case ValueType.Bool:
		case ValueType.String:
			replacedWith = node.replaceWith(raw.toUnaryExpression());
			return true;
		default: return false;
	}
}
@("simplifyUnaryExpressions")
unittest
{
	alias assertSimplifyUnaryExpr = assertTransformations!(simplifyUnaryExpressions);
	assertSimplifyUnaryExpr(
		`!false`,
		`true`
	);
	assertSimplifyUnaryExpr(
		`-Infinity`,
		`-Infinity`
	);
	assertSimplifyUnaryExpr(
		`void 0`,
		`void 0`
	);
	assertSimplifyUnaryExpr(
		`+Infinity`,
		`+Infinity`
	);
}
bool simplifyBinaryExpressions(BinaryExpressionNode node, out Node replacedWith)
{
	auto raw = node.resolveBinaryExpression();
	if (raw.type == ValueType.NotKnownAtCompileTime)
		return false;
	replacedWith = node.replaceWith(raw.toUnaryExpression());
	replacedWith.reanalyseHints();
	return true;
}

@("simplifyBinaryExpressions")
unittest
{
	alias assertSimplifyBinaryExpr = assertTransformations!(simplifyBinaryExpressions);
	assertSimplifyBinaryExpr(
		`var a = 123 + 123`,
		`var a = 246`
	);
	assertSimplifyBinaryExpr(
		`var a = "some" + "stuff" + "concatenated" +
		"together";`,
		`var a = "somestuffconcatenatedtogether"`
	);
	assertSimplifyBinaryExpr(
		`if (123 < 456) a();`,
		`if (true) a();`
	);
	assertSimplifyBinaryExpr(
		`if (123 > 456) a();`,
		`if (false) a();`
	);
}
bool shortenBooleanNodes(BooleanNode node, out Node replacedWith)
{
	if (node.parent.type == NodeType.UnaryExpressionNode)
	{
		node.parent.as!(UnaryExpressionNode).prefixs ~= new PrefixExpressionNode(Prefix.Negation);
		replacedWith = node.replaceWith(new DecimalLiteralNode(cast(const(ubyte)[])(node.value ? "0" : "1")));
	} else
	{
		replacedWith = node.replaceWith(
			new UnaryExpressionNode(
				[new PrefixExpressionNode(Prefix.Negation)],
				new DecimalLiteralNode(cast(const(ubyte)[])(node.value ? "0" : "1"))
			)
		);
	}
	return true;
}

@("shortenBooleanNodes")
unittest
{
	alias assertShortenBooleanNodes = assertTransformations!(shortenBooleanNodes);
	assertShortenBooleanNodes(
		`true`,
		`!0`
	);
	assertShortenBooleanNodes(
		`false`,
		`!1`
	);
	assertShortenBooleanNodes(
		`var b = false`,
		`var b = !1`
	);
	assertShortenBooleanNodes(
		`var b = true`,
		`var b = !0`
	);
	assertShortenBooleanNodes(
		`var b = !true`,
		`var b = !!0`
	);
}

void moveStringComparisonToLeftOperand(BinaryExpressionNode binExpr, out Node replacedWith)
{
	auto ops = binExpr.getOperators();
	foreach(idx, op; ops.enumerate)
	{
		if (op.operator != ExpressionOperator.StrictEqual && op.operator != ExpressionOperator.StrictNotEqual)
			continue;

		auto leftOperand = binExpr.children[idx*2];
		auto rightOperand = binExpr.children[(idx+1)*2];

		if (leftOperand.type != NodeType.StringLiteralNode &&
			rightOperand.type != NodeType.StringLiteralNode)
			continue;

		auto leftPrecedence = idx > 0 ? ops[idx-1].operator.getExprOperatorPrecedence() : 0;
		auto rightPrecedence = idx+1 < ops.length ? ops[idx+1].operator.getExprOperatorPrecedence() : 0;

		auto precedence = op.operator.getExprOperatorPrecedence();
		if (precedence <= leftPrecedence || precedence < rightPrecedence)
			continue;

		if (leftOperand.type != NodeType.StringLiteralNode)
		{
			binExpr.children[idx*2] = rightOperand;
			binExpr.children[(idx+1)*2] = leftOperand;
		}
		
		if (op.operator == ExpressionOperator.StrictEqual)
			op.operator = ExpressionOperator.Equal;
		else
			op.operator = ExpressionOperator.NotEqual;
	}
}

@("moveStringComparisonToLeftOperand")
unittest
{
	alias assertMoveStringComparison = assertTransformations!(moveStringComparisonToLeftOperand);
	assertMoveStringComparison(
		`a === "a"`,
		`"a" == a`
	);
	assertMoveStringComparison(
		`a !== "a"`,
		`"a" != a`
	);
	assertMoveStringComparison(
		`b === a !== "a"`,
		`b === a !== "a"`
	);
	assertMoveStringComparison(
		`a !== "a" === b`,
		`"a" != a === b`
	);
	assertMoveStringComparison(
		`false === "false" < 1`,
		`false === "false" < 1`
	);
	assertMoveStringComparison(
		`"abc" < "def" === true`,
		`"abc" < "def" === true`
	);
}




