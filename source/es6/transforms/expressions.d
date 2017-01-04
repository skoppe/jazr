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