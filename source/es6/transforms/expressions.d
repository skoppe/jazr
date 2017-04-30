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
import es6.transforms.returns : createVoid0Node;
import option;
import es6.analyse;
import es6.eval;
import std.range : retro, enumerate;
import std.algorithm : all, each, map, reduce, sum, any;

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
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

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

		if (diff.type == Diff.No || diff.type == Diff.IdentifiersCount)
			return;

		emit(got.getRoot()).shouldEqual(emit(expected), file, line); throw new UnitTestException([diff.getDiffMessage()], file, line);
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
	version(tracing) mixin(traceTransformer!(__FUNCTION__));
	
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
		`a();`,
		`a();`
	);
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
		`a = 6; for (;;); b = 7, c = 8;`,
		`a = 6; for (;;); b = 7, c = 8`
	);
	assertCombineStatements(
		`a ? b() : c(); e && f && (d = 6); g = {a: 4}; h = [0,1]; (i || j) && g(); a = /^(?:webkit|moz|o)[A-Z]/.test("");`,
		`a ? b() : c(), e && f && (d = 6), g = {a: 4}, h = [0,1], (i || j) && g(), a = /^(?:webkit|moz|o)[A-Z]/.test("");`
	);
	assertCombineStatements(
		`a(),b();c(),d();`,
		`a(),b(),c(),d();`
	);
}
bool simplifyUnaryExpressions(UnaryExpressionNode node, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	auto raw = node.getRawValue();
	switch (raw.type)
	{
		case ValueType.Undefined:
			assert(node.prefixs.length > 0 && node.prefixs[0].type == NodeType.PrefixExpressionNode && node.prefixs[0].as!(PrefixExpressionNode).prefix == Prefix.Void);
			return false;
		case ValueType.NaN:
		case ValueType.Bool:
		case ValueType.String:
			if (node.children[0].type == NodeType.IdentifierReferenceNode)
				node.children[0].shread();
			auto expr = raw.toUnaryExpression();
			if (expr.type == NodeType.IdentifierReferenceNode)
				node.branch.scp.addIdentifier(Identifier(expr.as!IdentifierNode));
			replacedWith = node.replaceWith(expr);
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
		`!NaN`,
		`true`
	);
	assertSimplifyUnaryExpr(
		`+"abc"`,
		`NaN`
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
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

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
	assertSimplifyBinaryExpr(
		`if (123 > b) a();`,
		`if (123 > b) a();`
	);
}
bool shortenBooleanNodes(BooleanNode node, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (node.parent.type == NodeType.CallExpressionNode)
		return false;
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
	assertShortenBooleanNodes(
		`true.should.be.false;`,
		`true.should.be.false;`
	);
}

void moveStringComparisonToLeftOperand(BinaryExpressionNode binExpr, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

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

void convertHexToDecimalLiterals(HexLiteralNode node, out Node replacedWith)
{
	import std.conv : to;
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (node.value.length > 8)
		return;
	uint dec = (cast(const(char)[])node.value).to!uint(16);
	replacedWith = node.replaceWith(new DecimalLiteralNode(cast(const(ubyte)[])dec.to!string));
}

@("convertHexToDecimalLiterals")
unittest
{
	alias assertHexDecLiterals = assertTransformations!(convertHexToDecimalLiterals);
	assertHexDecLiterals(
		`0x1;`,
		`1;`
	);
	assertHexDecLiterals(
		`0xFf;`,
		`255;`
	);
	assertHexDecLiterals(
		`0xffffffff;`,
		`4294967295;`
	);
	assertHexDecLiterals(
		`0xfffffffff;`,
		`0xfffffffff;`
	);
}

// TODO: this is an unfortunate name. Better something like NonNestedExpression
bool isExpressionStatement(Node node)
{
	switch (node.parent.type)
	{
		case NodeType.BlockStatementNode:
		case NodeType.ModuleNode:
		case NodeType.FunctionBodyNode:
		case NodeType.CaseBodyNode:
		case NodeType.WithStatementNode:
		case NodeType.ReturnStatementNode:
		case NodeType.ExpressionNode:
			return true;
		case NodeType.IfStatementNode:
			return node.parent.as!IfStatementNode.condition !is node;
		case NodeType.DoWhileStatementNode:
			return node.parent.getIndexOfChild(node) == 0;
		case NodeType.WhileStatementNode:
		case NodeType.ForStatementNode:
			return node.parent.children[$-1] is node;
		default:
			return false;
	}
}

void invertBinaryExpressions(BinaryExpressionNode node, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	auto opNode = node.children[1].as!(ExpressionOperatorNode);
	if (opNode.operator != ExpressionOperator.LogicalAnd && 
		opNode.operator != ExpressionOperator.LogicalOr)
		return;

	if (!node.isExpressionStatement)
		return;

	if (node.children[0].type == NodeType.UnaryExpressionNode)
	{
		auto unary = node.children[0].as!UnaryExpressionNode;
		if (unary.prefixs.length != 1)
			return;
		if (unary.prefixs[0].as!(PrefixExpressionNode).prefix != Prefix.Negation)
			return;

		if (unary.children[0].type == NodeType.ParenthesisNode)
		{
			auto paren = unary.children[0].as!ParenthesisNode;
			assert(paren.children.length == 1); // its possible for a parenthesisNode to have more children, but only as argumentlist for arrow function
			if (paren.children[0].type != NodeType.BinaryExpressionNode)
				return;
			auto innerBin = paren.children[0].as!BinaryExpressionNode;

			auto heuristic = innerBin.nodes.map!((n){
				if (n.type != NodeType.UnaryExpressionNode)
					return -1;
				auto un = n.as!UnaryExpressionNode;
				if (un.prefixs.length != 1)
					return -1;
				if (un.prefixs[0].as!(PrefixExpressionNode).prefix != Prefix.Negation)
					return -1;
				return 1;
			});
			auto hasAnd = innerBin.getOperators.any!(o => o.operator == ExpressionOperator.LogicalAnd);
			auto hasOr = innerBin.getOperators.any!(o => o.operator == ExpressionOperator.LogicalOr);

			if (reduce!"a+b"(heuristic) >= 0 && !(hasAnd && hasOr))
			{
				foreach(n; innerBin.nodes)
				{
					if (n.type != NodeType.UnaryExpressionNode)
						n.negateNode();
					else
					{
						auto un = n.as!UnaryExpressionNode;
						if (un.prefixs.length == 1)
						{
							auto pref = un.prefixs[0].as!PrefixExpressionNode;
							if (pref.prefix == Prefix.Negation)
							{
								un.replaceWith(un.children[0]);
							} else
								un.prefixs = [cast(Node)new PrefixExpressionNode(Prefix.Negation)] ~ un.prefixs;
						}
					}
				}
				innerBin.getOperators.each!((o){ o.operator = o.operator == ExpressionOperator.LogicalAnd ? ExpressionOperator.LogicalOr : ExpressionOperator.LogicalAnd; o.reanalyseHints(); });
				node.replaceChildWith(unary,innerBin);
				opNode.reanalyseHints();
			} else
			{
				node.replaceChildWith(unary,innerBin);
				opNode.operator = opNode.operator == ExpressionOperator.LogicalAnd ? ExpressionOperator.LogicalOr : ExpressionOperator.LogicalAnd;
				opNode.reanalyseHints();
			}
			replacedWith = node;
		} else
		{
			unary.replaceWith(unary.children[0]);
			opNode.operator = opNode.operator == ExpressionOperator.LogicalAnd ? ExpressionOperator.LogicalOr : ExpressionOperator.LogicalAnd;
			opNode.reanalyseHints();
		}
	} else if (node.children[0].type == NodeType.ParenthesisNode)
	{
		auto paren = node.children[0].as!ParenthesisNode;
		assert(paren.children.length == 1); // its possible for a parenthesisNode to have more children, but only as argumentlist for arrow function
		if (paren.children[0].type != NodeType.BinaryExpressionNode)
			return;
		auto innerBin = paren.children[0].as!BinaryExpressionNode;

		auto hasAnd = innerBin.getOperators.any!(o => o.operator == ExpressionOperator.LogicalAnd);
		auto hasOr = innerBin.getOperators.any!(o => o.operator == ExpressionOperator.LogicalOr);
		if (hasAnd && hasOr)
			return;

		foreach(n; innerBin.nodes)
		{
			if (n.type != NodeType.UnaryExpressionNode)
				return;
			auto un = n.as!UnaryExpressionNode;
			if (un.prefixs.length != 1)
				return;
			if (un.prefixs[0].as!(PrefixExpressionNode).prefix != Prefix.Negation)
				return;
		}
		innerBin.nodes.each!(n => n.replaceWith(n.children[0]));
		innerBin.getOperators.each!((o){ o.operator = o.operator == ExpressionOperator.LogicalAnd ? ExpressionOperator.LogicalOr : ExpressionOperator.LogicalAnd; o.reanalyseHints(); });
		node.replaceChildWith(paren,innerBin);
		opNode.operator = opNode.operator == ExpressionOperator.LogicalAnd ? ExpressionOperator.LogicalOr : ExpressionOperator.LogicalAnd;
		opNode.reanalyseHints();
	}
}

@("invertBinaryExpressions")
unittest
{
	alias assertInvertBinExpr = assertTransformations!(invertBinaryExpressions);

	assertInvertBinExpr(
		`!a > b()`,
		`!a > b()`
	);
	assertInvertBinExpr(
		`!a && b()`,
		`a || b()`
	);
	assertInvertBinExpr(
		`~!(a) && b()`,
		`~!(a) && b()`
	);
	assertInvertBinExpr(
		`~(a) && b()`,
		`~(a) && b()`
	);
	assertInvertBinExpr(
		`!(!!a && b) && b()`,
		`!!a && b || b();`	// TODO: actually the !! from 'a' can be removed
	);
	assertInvertBinExpr(
		`!(~a && b) && b()`,
		`~a && b || b()`
	);
	assertInvertBinExpr(
		`!(~a || !b) && b()`,
		`!~a && b && b();`
	);
	assertInvertBinExpr(
		`!((a)) && b()`,
		`!((a)) && b()`
	);
	assertInvertBinExpr(
		`(a) && b()`,
		`(a) && b()`
	);
	assertInvertBinExpr(
		`(!!a && c) && b()`,
		`(!!a && c) && b()`
	);
	assertInvertBinExpr(
		`(a && c) && b()`,
		`(a && c) && b()`
	);
	assertInvertBinExpr(
		`(~a && c) && b()`,
		`(~a && c) && b()`
	);
	assertInvertBinExpr(
		`d && !a && b()`,
		`d && !a && b()`
	);
	assertInvertBinExpr(
		`d || !a && b()`,
		`d || !a && b()`
	);
	assertInvertBinExpr(
		`!a && b() && d`,
		`a || b() && d`
	);
	assertInvertBinExpr(
		`(!a || !b) && c()`,
		`a && b || c()`
	);
	assertInvertBinExpr(
		`(!a && !b) && c()`,
		`a || b || c()`
	);
	assertInvertBinExpr(
		`!(a && b) && c()`,
		`a && b || c()`
	);
	assertInvertBinExpr(
		`!(a && b) && c()`,
		`a && b || c()`
	);
	assertInvertBinExpr(
		`!(a || b) && c()`,
		`a || b || c()`
	);
	assertInvertBinExpr(
		`!(!a || b) && c()`,
		`a && !b && c()`
	);
	assertInvertBinExpr(
		`!(!a || b || d || e) && c()`,
		`!a || b || d || e || c()`
	);
	assertInvertBinExpr(
		`!(a || b || d) && c()`,
		`a || b || d || c()`
	);
	assertInvertBinExpr(
		`(!a && !b && !d) && c()`,
		`a || b || d || c()`
	);
	assertInvertBinExpr(
		`(!a || !b || !d) && c()`,
		`a && b && d || c()`
	);
	assertInvertBinExpr(
		`!(!a || !b || c) && b()`,
		`a && b && !c && b()`
	);
	assertInvertBinExpr(
		`!(!a || b || c) && b()`,
		`!a || b || c || b()`
	);
	assertInvertBinExpr(
		`!(!a && !e) && c()`,
		`a || e && c()`
	);
	assertInvertBinExpr(
		`!(!a && !e && !f) && c()`,
		`a || e || f && c()`
	);
	assertInvertBinExpr(
		`!(!a || !b || !d && !e) && c()`,
		`!a || !b || !d && !e || c()` // TODO: this can be `a && b && (d || e) && c()`
	);
	assertInvertBinExpr(
		`(!a || !b && !d) && c()`,
		`(!a || !b && !d) && c()` // TODO: this can be `a && (b || d) || c()`
	);
	assertInvertBinExpr(
		`(!a && !b || !d) && c()`,
		`(!a && !b || !d) && c()` // TODO: this can be `(a || b) && d || c()`
	);
	assertInvertBinExpr(
		`!(!a || !b && !c) && b()`,
		`!a || !b && !c || b()` // TODO: this can be `a && (b || c) && b()`
	);
	assertInvertBinExpr(
		`(c(), !a && !g) && b()`,
		`(c(), a || !g) && b()`
	);
	assertInvertBinExpr(
		`if(!a && b) bla();`,
		`if(!a && b) bla();`
	);
	assertInvertBinExpr(
		`!a && b ? 5 : 6;`,
		`!a && b ? 5 : 6;`
	);
	assertInvertBinExpr(
		`c && (!a && b);`,
		`c && (!a && b);`
	);
	assertInvertBinExpr(
		`while (!a && b) doBla();`,
		`while (!a && b) doBla();`
	);
	assertInvertBinExpr(
		`a = !b && c;`,
		`a = !b && c;`
	);
	assertInvertBinExpr(
		`fun(!b && c);`,
		`fun(!b && c);`
	);
	assertInvertBinExpr(
		`for(var b;!b && c;d++);`,
		`for(var b;!b && c;d++);`
	);
	assertInvertBinExpr(
		`do a && b(); while(c);`,
		`do a && b(); while(c);`
	);
	assertInvertBinExpr(
		`a=6, !b && d();`,
		`a=6, b || d();`
	);
}


// TODO: create minifier for inverting equality binary expressions:
// 	assertInvertEqualityBinExpr(
	//	`!(a == 9)`,
	//	`a != 9`
	//);
	//assertInvertEqualityBinExpr(
	//	`d(), !(a == 9)`,
	//	`d(), a != 9`
	//);
	//assertInvertEqualityBinExpr(
		//`(t.type !== a.Token.Keyword || t.value !== e) && this.throwUnexpectedToken(t)`,
		//`t.type === a.Token.Keyword && t.value === e || this.throwUnexpectedToken(t)`
	//);

bool convertUndefinedToVoid0(Scope scp)
{
	import std.algorithm : remove;
	size_t off = 0;
	foreach(idx, iden; scp.identifiers.dup)
	{
		if (iden.definition !is null)
			continue;
		if (iden.node.identifier != "undefined")
			continue;

		iden.node.replaceWith(createVoid0Node());

		scp.identifiers = scp.identifiers.remove(idx-off);
		off++;
	}
	return off != 0;
}

@("convertUndefinedToVoid0")
unittest
{
	alias assertToVoid0 = assertTransformations!(convertUndefinedToVoid0);

	assertToVoid0(
		`undefined;`,
		`void 0;`
	);
	assertToVoid0(
		`var a = undefined;`,
		`var a = void 0;`
	);
	assertToVoid0(
		`if (undefined) b = undefined;`,
		`if (void 0) b = void 0;`
	);
	assertToVoid0(
		`a = {undefined: 6};`,
		`a = {undefined: 6};`
	);
	assertToVoid0(
		`var undefined; a = undefined;`,
		`var undefined; a = undefined;`
	);
}

bool convertInfinityTo1div0(Scope scp)
{
	import std.algorithm : remove;
	size_t off = 0;
	foreach(idx, iden; scp.identifiers.dup)
	{
		if (iden.definition !is null)
			continue;
		if (iden.node.identifier != "Infinity")
			continue;

		iden.node.replaceWith(
			new BinaryExpressionNode([new DecimalLiteralNode(cast(const(ubyte)[])"1"),new ExpressionOperatorNode(ExpressionOperator.Division),new DecimalLiteralNode(cast(const(ubyte)[])"0")])
		);

		scp.identifiers = scp.identifiers.remove(idx-off);
		off++;
	}
	return off != 0;
}

@("convertInfinityTo1div0")
unittest
{
	alias assertTo1div0 = assertTransformations!(convertInfinityTo1div0);

	assertTo1div0(
		`Infinity;`,
		`1/0;`
	);
	assertTo1div0(
		`var a = Infinity;`,
		`var a = 1/0;`
	);
	assertTo1div0(
		`if (Infinity) b = Infinity;`,
		`if (1/0) b = 1/0;`
	);
	assertTo1div0(
		`a = {Infinity: 6};`,
		`a = {Infinity: 6};`
	);
	assertTo1div0(
		`var Infinity; a = Infinity;`,
		`var Infinity; a = Infinity;`
	);
}
