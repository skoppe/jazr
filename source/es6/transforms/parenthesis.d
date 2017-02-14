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
module es6.transforms.parenthesis;

import es6.nodes;
import es6.scopes;
import es6.transforms.expressions;
import es6.transforms.conditionals;
import option;
import es6.analyse;
import es6.eval;
import std.algorithm : map, min, max, any;
import std.range : drop;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}
/*

	assertConvertIfsToExpressionStatements(
		`("use strict",b.can() && top === bottom) && doThing()`,
		`"use strict",b.can() && top === bottom && doThing()`
	);
 */

bool removeUnnecessaryParenthesis(ParenthesisNode node, out Node replacedWith)
{
	if (node.children.length == 0)
	{
		node.detach();
		return true;
	}
	bool isExpression = node.children[0].type == NodeType.ExpressionNode;
	bool hasAssignment = node.children[0].hints.has(Hint.HasAssignment);
	bool isPartOfConditionalExpr = node.parent.type == NodeType.ConditionalExpressionNode;
	bool isPartOfBinaryExpr = node.parent.type == NodeType.BinaryExpressionNode;
	bool isLeftChild = node.parent.getIndexOfChild(node) == 0;
	bool isBinaryExpr = node.children[0].type == NodeType.BinaryExpressionNode;
	bool isPartOfUnaryExpr = node.parent.type == NodeType.UnaryExpressionNode;
	bool isPartOfNewExpr = node.parent.type == NodeType.NewExpressionNode;
	bool isPartOfCallExpr = node.parent.type == NodeType.CallExpressionNode;
	bool isConditionalNode = node.children[0].type == NodeType.ConditionalExpressionNode;
	bool isPartOfExpression = node.parent.type == NodeType.ExpressionNode;
	bool isFunctionExpr = node.children[0].type == NodeType.FunctionExpressionNode;
	bool isCallExpr = node.children[0].type == NodeType.CallExpressionNode;
	bool isPartOfVariableDecl = node.parent.type == NodeType.VariableDeclarationNode;

	if (isPartOfVariableDecl)
	{
		if (isExpression)
			return false;
	}

	if (isPartOfConditionalExpr && isConditionalNode)
		return false;

	if (isPartOfNewExpr || isPartOfCallExpr)
		return false;

	if (isFunctionExpr)
		return false;

	if (isCallExpr || isExpression)
	{
		if (node.children[0].children.any!(c =>
			c.type == NodeType.FunctionExpressionNode ||
			c.type == NodeType.GeneratorExpressionNode ||
			(c.type == NodeType.ClassDeclarationNode && c.as!(ClassDeclarationNode).name is null)))
			return false;
	}
	auto parent = node.parent;
	if (isPartOfBinaryExpr)
	{
		if (isBinaryExpr)
		{
			auto innerBinaryExpr = node.children[0].as!(BinaryExpressionNode);
			auto outerBinaryExpr = node.parent.as!(BinaryExpressionNode);
			auto innerOperators = innerBinaryExpr.getOperators();
			auto outerOperators = outerBinaryExpr.getOperatorsSurrounding(node);
			if (!isLeftChild && outerOperators.take(1).any!(o => o.operator.isNonCommutative))
				return false;
			if (outerOperators.drop(isLeftChild ? 0 : 1).take(1).any!(o => o.operator == ExpressionOperator.In))
				return false;
			auto innerLowestPrecedence = innerOperators.map!(o => o.operator.getExprOperatorPrecedence()).reduce!(min);
			auto outerHighestPrecedence = outerOperators.map!(o => o.operator.getExprOperatorPrecedence()).reduce!(max);
			if (innerLowestPrecedence < outerHighestPrecedence)
				return false;
			outerBinaryExpr.replaceChildWith(node, innerBinaryExpr);
			replacedWith = innerBinaryExpr.children[0];
		}
		else if (parent.parent.type == NodeType.ConditionalExpressionNode)
			return false;
		else if (isExpression)
		{
			if (!isLeftChild)
				return false;
			auto last = node.children[0].children[$-1];
			if (last.hints.has(Hint.HasAssignment))
				return false;
			auto transfers = node.children[0].children[0..$-1];
			auto binExpr = node.parent;
			if (last.type == NodeType.BinaryExpressionNode)
			{
				node.children = [last];
				last.parent = node;
			} else {
				node.replaceWith(last);
				replacedWith = last;
			}
			if (transfers.length > 0)
			{
				if (binExpr.parent.type == NodeType.ExpressionNode)
				{
					binExpr.insertBefore(transfers);
					replacedWith = transfers[0];
				} else
				{
					auto exprNode = binExpr.replaceWith(new ExpressionNode(transfers));
					exprNode.addChild(binExpr);
					replacedWith = binExpr;
				}
			}
		} else if (hasAssignment)
			return false;
		else if (isConditionalNode)
			return false;
		else
		{
			replacedWith = node.replaceWith(node.children[0]);
		}
	} else if (isPartOfUnaryExpr)
	{
		if (!isExpression)
			return false;
		auto transfers = node.children[0].children[0..$-1];
		auto last = node.children[0].children[$-1];
		auto unary = node.parent;
		if (parent.parent.type == NodeType.BinaryExpressionNode)
		{
			auto binExpr = parent.parent;
			replacedWith = node.replaceWith(last);
			if (binExpr.parent.type == NodeType.ExpressionNode)
			{
				binExpr.insertBefore(transfers);
			} else
			{
				auto expr = new ExpressionNode(transfers);
				binExpr.replaceWith(expr);
				expr.addChild(binExpr);
				replacedWith = expr;
			}
		} else
		{
			node.children[0].replaceWith(last);
			if (unary.parent.type == NodeType.ExpressionNode)
			{
				unary.insertBefore(transfers);
			} else
			{
				auto exprNode = new ExpressionNode(transfers);
				unary.replaceWith(exprNode);
				exprNode.addChild(unary);
				replacedWith = exprNode;
			}
		}
	} else if (isExpression && isPartOfConditionalExpr)
	{
		if (hasAssignment)
			return false;
		auto condExpr = node.parent.as!(ConditionalExpressionNode);
		if (condExpr.condition !is node)
			return false;
		auto transfers = node.children[0].children[0..$-1];
		auto cond = node.children[0].children[$-1];
		if (cond.type == NodeType.ConditionalExpressionNode)
		{
			auto exprNode = condExpr.replaceWith(new ExpressionNode(transfers));
			node.children = [];
			node.addChild(cond);
			exprNode.addChild(condExpr);
			return true;
		}
		auto exprNode = condExpr.replaceWith(new ExpressionNode(transfers));
		cond.parent = null;
		condExpr.condition.replaceWith(cond);
		exprNode.addChild(condExpr);
	} else if (isExpression && isPartOfExpression)
	{
		node.insertBefore(node.children[0].children);
		node.detach();
	} else if (isPartOfConditionalExpr && !isLeftChild && hasAssignment)
	{
		return false;
	} else if (node.children.length == 1)
	{
		replacedWith = node.replaceWith(node.children[0]);
	} else
	{
		node.insertBefore(node.children);
		node.detach();
	}
	parent.reanalyseHints();
	return true;
}

unittest
{
	alias assertRemoveParens = assertTransformations!(removeUnnecessaryParenthesis);

	assertRemoveParens(
		`+(a + b)`,
		`+(a + b)`
	);
	assertRemoveParens(
		`+(a ? c : d)`,
		`+(a ? c : d)`
	);
	/// Issue #88
	assertRemoveParens(
		`if ( !( events = elemData.events ) ) { events = elemData.events = {}; }`,
		`if (!(events = elemData.events)) { events = elemData.events = {} }`
	);
	/// Issue #82
	assertRemoveParens(
		`(freeModule.exports = Rx).Rx = Rx`,
		`(freeModule.exports = Rx).Rx = Rx`
	);
	/// ditto
	assertRemoveParens(
		`(gekko(),freeModule.exports = Rx).Rx = Rx`,
		`(gekko(),freeModule.exports = Rx).Rx = Rx`
	);
	/// Issue #74
	assertRemoveParens(
		`function checkGlobal(value) { return (value && value.Object === Object) ? value : null }`,
		`function checkGlobal(value) { return value && value.Object === Object ? value : null }`
	);
	assertRemoveParens(
		`function checkGlobal(value) { return (a && bla(), value && value.Object === Object) ? value : null }`,
		`function checkGlobal(value) { return a && bla(),value && value.Object === Object ? value : null }`
	);
	/// cannot remove parenthesis when it contains a ConditionalExpression
	assertRemoveParens(
		`function checkGlobal(value) { return (a ? "bla" : g) ? value : null }`,
		`function checkGlobal(value) { return (a ? "bla" : g) ? value : null }`
	);
	/// ditto
	assertRemoveParens(
		`function checkGlobal(value) { return (doBla(),a ? "bla" : g) ? value : null }`,
		`function checkGlobal(value) { return doBla(),(a ? "bla" : g) ? value : null }`
	);
	assertRemoveParens(
		`(a, b) && 2`,
		`a, b && 2`
	);
	assertRemoveParens(
		`d && (a, b) && 2`,
		`d && (a, b) && 2`
	);
	assertRemoveParens(
		`if ((a && b)) doBla();`,
		`if (a && b) doBla();`
	);
	assertRemoveParens(
		`if (((a || b))) doBla();`,
		`if (a || b) doBla();`
	);
	assertRemoveParens(
		`d = 6, (b = 6, a),u = 7`,
		`d = 6,b = 6,a,u = 7`
	);
	assertRemoveParens(
		`d = 6, (b = 6),u = 7`,
		`d = 6,b = 6,u = 7`
	);
	assertRemoveParens(
		`c ? (b = 6, a) : 6`,
		`c ? (b = 6,a) : 6`
	);
	assertRemoveParens(
		`if ((b=6,(a))) doBla();`,
		`if (b = 6,a) doBla();`
	);
	assertRemoveParens(
		`if (dp(), (b=6,(a))) doBla();`,
		`if (dp(), b = 6,a) doBla();`
	);
	assertRemoveParens(
		`if ((b=6, (a) || b)) doBla();`,
		`if (b = 6,a || b) doBla();`
	);
	assertRemoveParens(
		`if ((b=6, (a) && b)) doBla();`,
		`if (b = 6,a && b) doBla();`
	);
	assertRemoveParens(
		`!(b=6, a && b)`,
		`b=6, !(a && b)`
	);
	assertRemoveParens(
		`if (!(b=6, (a) && b)) doBla();`,
		`if (b = 6,!(a && b)) doBla();`
	);
	assertRemoveParens(
		`if (da(), !(b=6, (a) && b)) doBla();`,
		`if (da(), b = 6,!(a && b)) doBla();`
	);
	assertRemoveParens(
		`b = a ? (d = 5,6) : (g = 12,7)`,
		`b = a ? (d = 5,6) : (g = 12,7)`
	);
	assertRemoveParens(
		`b = a ? (d = 5,6) && g : (g = 12,7) || h`,
		`b = a ? (d = 5,6) && g : (g = 12,7) || h`
	);
	assertRemoveParens(
		`b = a ? g && (d = 5,6) : h || (g = 12,7)`,
		`b = a ? g && (d = 5,6) : h || (g = 12,7)`
	);
	assertRemoveParens(
		`b = a ? (g && (d = 5,6)) : (h || (g = 12,7))`,
		`b = a ? g && (d = 5,6) : h || (g = 12,7)`
	);
	assertRemoveParens(
		`if ((b=6, (a)) || b) doBla();`,
		`if (b = 6,a || b) doBla();`
	);
	assertRemoveParens(
		`if ((a || (b), b=6) || b) doBla();`,
		`if ((a || b, b=6) || b) doBla();`,
	);
	assertRemoveParens(
		`(b = 6, a)`,
		`b = 6,a`
	);
	assertRemoveParens(
		`(b = 6)`,
		`b = 6`
	);
	assertRemoveParens(
		`inst.refs === emptyObject ? (inst.refs = {}) : inst.refs`,
		"inst.refs === emptyObject ? (inst.refs = {}) : inst.refs"
	);
	assertRemoveParens(
		`a = (b && g in window)`,
		"a = b && g in window"
	);
	assertRemoveParens(
		`(b = a ? 6 : 7)`,
		`b = a ? 6 : 7`
	);
	assertRemoveParens(
		`a && (b = a ? 6 : 7)`,
		`a && (b = a ? 6 : 7)`
	);
	assertRemoveParens(
		`a && (b = a)`,
		`a && (b = a)`
	);
	writeln("yep!");
  	assertRemoveParens(
  		`var a = (a * b * c) + 6`,
        `var a = a * b * c + 6`
  	);
  	assertRemoveParens(
  		`var a = ("a " + propName + " b ") + ("c.");`,
        `var a = "a " + propName + " b " + "c.";`
  	);
  	assertRemoveParens(
  		`;(function (factory) { doSomething() });`,
  		`; (function (factory){ doSomething() })`
  	);
  	assertRemoveParens(
  		`;(function (factory) { doSomething() }.call(this, function (root, exp, Rx, undefined) { return undefined }));`,
  		`; (function (factory){ doSomething() }.call(this, function (root, exp, Rx, undefined){ return undefined }))`
  	);
  	/// Issue 83
  	assertRemoveParens(
  		`()`,
  		``
  	);
  	assertRemoveParens(
  		`bla()`,
  		`bla()`
  	);
  	// TODO this belongs in the argumentlist optimizer
  	/*assertRemoveParens(
  		`new bla()`,
  		`new bla`
  	);*/
  	assertRemoveParens(
  		`if (typeof new root.Set()[ '@@iterator' ] === 'function') a = 6;`,
  		`if (typeof new root.Set()[ '@@iterator' ] === 'function') a = 6;`
  	);
  	assertRemoveParens(
  		`(a ? b : c)(b)`,
  		`(a ? b : c)(b)`
  	);
  	assertRemoveParens(
  		`(d = 5,a ? b : c)`,
  		`d = 5,a ? b : c`
  	);
  	assertRemoveParens(
  		`(a ? b : c)`,
  		`a ? b : c`
  	);
  	assertRemoveParens(
  		`(a && b ? c : d) && e`,
  		`(a && b ? c : d) && e`
  	);
  	assertRemoveParens(
		`function b() { return a ? 6 : (void (d = 5)) }`,
  		`function b() { return a ? 6 : void (d = 5) }`
  	);
	assertRemoveParens(
		`if (((a && b) && c) && d) doBla();`,
		`if (a && b && c && d) doBla();`
	);
	assertRemoveParens(
		`if (((a || b) && c) && (d || e)) doBla();`,
		`if ((a || b) && c && (d || e)) doBla();`
	);
	assertRemoveParens(
		`g&&((a=5,e)&&(d=5),b=5);`,
		`g&&(a=5,e&&(d=5),b=5);`
	);
	//assertRemoveParens(
	//	`(1)&&(true)?d=5:g=5;`,
	//	`1&&true?d=5:g=5;`
	//);
	//assertRemoveParens(
	//	`if (!(a)) if ((!b)) d = 5;`,
	//	"!a && !b && (d = 5)"
	//);
	assertRemoveParens(
		`if (!(a = bla())) bar();`,
		`if (!(a = bla())) bar();`
	);	
	assertRemoveParens(
		`var E = n(0), R = (u(E), n(1017)), G = (9);`,
		`var E = n(0), R = (u(E), n(1017)), G = 9;`
	);
	assertRemoveParens(
		`( length - 1 ) in obj`,
		`(length-1)in obj;`
	);
	assertRemoveParens(
		`a && ( length - 1 ) in obj`,
		`a&&(length-1)in obj;`
	);
	//assertRemoveParens(
	//	`(f ? (c ? '+' : '') : '-')`,
	//	`f ? c ? '+' : '' : '-'`
	//);

	assertRemoveParens(`(1 - 2) - 3 + 4`,	`1 - 2 - 3 + 4`);
	assertRemoveParens(`(1 - 2) + 3 + 4`,	`1 - 2 + 3 + 4`);
	assertRemoveParens(`(1 + 2) - 3 + 4`,	`1 + 2 - 3 + 4`);
	assertRemoveParens(`(1 + 2) + 3 + 4`,	`1 + 2 + 3 + 4`);
	assertRemoveParens(`1 - 2 - (3 - 4)`,	`1 - 2 - (3 - 4)`);
	assertRemoveParens(`1 - 2 - (3 + 4)`,	`1 - 2 - (3 + 4)`);
	assertRemoveParens(`1 - 2 + (3 - 4)`,	`1 - 2 + 3 - 4`);
	assertRemoveParens(`1 - 2 + (3 + 4)`,	`1 - 2 + 3 + 4`);

	assertRemoveParens(`(1 - 2) * 3 + 4`,	`(1 - 2) * 3 + 4`);
	assertRemoveParens(`(1 - 2) / 3 + 4`,	`(1 - 2) / 3 + 4`);
	assertRemoveParens(`(1 + 2) * 3 + 4`,	`(1 + 2) * 3 + 4`);
	assertRemoveParens(`(1 + 2) / 3 + 4`,	`(1 + 2) / 3 + 4`);
	assertRemoveParens(`1 - 2 * (3 - 4)`,	`1 - 2 * (3 - 4)`);
	assertRemoveParens(`1 - 2 * (3 + 4)`,	`1 - 2 * (3 + 4)`);
	assertRemoveParens(`1 - 2 / (3 - 4)`,	`1 - 2 / (3 - 4)`);
	assertRemoveParens(`1 - 2 / (3 + 4)`,	`1 - 2 / (3 + 4)`);

	assertRemoveParens(`(1 * 2) * 3 + 4`,	`1 * 2 * 3 + 4`);
	assertRemoveParens(`(1 * 2) / 3 + 4`,	`1 * 2 / 3 + 4`);
	assertRemoveParens(`(1 / 2) * 3 + 4`,	`1 / 2 * 3 + 4`);
	assertRemoveParens(`(1 / 2) / 3 + 4`,	`1 / 2 / 3 + 4`);
	assertRemoveParens(`1 - 2 * (3 * 4)`,	`1 - 2 * 3 * 4`);
	assertRemoveParens(`1 - 2 / (3 * 4)`,	`1 - 2 / (3 * 4)`);
	assertRemoveParens(`1 - 2 * (3 / 4)`,	`1 - 2 * 3 / 4`);
	assertRemoveParens(`1 - 2 / (3 / 4)`,	`1 - 2 / (3 / 4)`);

	assertRemoveParens(`!(1 - 2,b) - 3 + 4`, `1 - 2, !b - 3 + 4`);
	assertRemoveParens(`a(), !(1 - 2,b) - 3 + 4`, `a(), 1 - 2, !b - 3 + 4`);

}