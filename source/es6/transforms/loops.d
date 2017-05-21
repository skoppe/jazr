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
module es6.transforms.loops;

import es6.lexer;
import es6.tokens;
import es6.nodes;
import es6.keywords;
import es6.transforms.conditionals;
import es6.transformer;
import es6.analyse;
import std.array : appender;
import es6.bench;
import es6.eval;

version (unittest)
{
	import std.stdio;
}
IfStatementNode getSurroundingIfStatement(Node node)
{
	auto parent = node.parent;
	if (parent is null)
		return null;
	if (parent.type == NodeType.BlockStatementNode)
		return node.parent.getSurroundingIfStatement();
	else if (parent.type == NodeType.IfStatementNode)
		return parent.as!IfStatementNode;
	return null;
}
ForStatementNode getSurroundingForStatement(Node node)
{
	auto parent = node.parent;
	if (parent is null)
		return null;
	if (parent.type == NodeType.BlockStatementNode)
		return node.parent.getSurroundingForStatement();
	else if (parent.type == NodeType.ForStatementNode)
		return parent.as!ForStatementNode;
	return null;
}

bool negateIfContinue(ContinueStatementNode cont, out Node replacedWith)
{
	if (cont.label !is null)
		return false;

	IfStatementNode ifStmt = cont.getSurroundingIfStatement();
	if (ifStmt is null)
		return false;

	Node outer = ifStmt.parent;

	// Ensure there are no statements after the ifStmt.parent inside the for loop
	Node walk = outer;
	do {
		if (walk is null)
			return false;
		if (walk.type == NodeType.ForStatementNode)
			break;
		if (walk.type == NodeType.WhileStatementNode ||
			walk.type == NodeType.DoWhileStatementNode)
			return false;
		if (walk.parent is null)
			return false;
		if (walk.parent.type == NodeType.BlockStatementNode)
			if (walk.parent.children[$-1] !is walk)
				return false;
		if (walk.parent.type == NodeType.ForStatementNode)
		{
			if (walk.parent.children[$-1] !is walk)
				return false;
			break;
		}
		walk = walk.parent;
	} while(true);

	Node[] transfers;
	if (outer.type == NodeType.BlockStatementNode)
		transfers = outer.detachStatementsAfter(ifStmt);

	if (ifStmt.truthPath.isBlockStatement)
	{
		ifStmt.truthPath.as!(BlockStatementNode).dropAllFrom(NodeType.ContinueStatementNode);
		ifStmt.truthPath.reanalyseHints();
		if (ifStmt.truthPath.hasStatements())
		{
			if (transfers.length == 0)
			{
				return true;
			}
			ifStmt.forceElsePath();
			ifStmt.elsePath.addStatements(transfers);
			replacedWith = ifStmt.truthPath;
			return true;
		}
		if (ifStmt.hasElsePath)
		{
			ifStmt.swapPaths();
			ifStmt.removeElsePath();
			ifStmt.negateCondition();
			ifStmt.truthPath.addStatements(transfers);
			return true;
		}
		if (transfers.length == 0)
		{
			if (ifStmt.condition.canHaveSideEffects())
			{
				ifStmt.truthPath.branch.remove();
				if (ifStmt.hasElsePath)
					ifStmt.elsePath.branch.remove();
				ifStmt.replaceWith(ifStmt.condition);
			}
			else
				ifStmt.detach();
			outer.reanalyseHints();
			return true;
		}
	}
	else
	{
		if (ifStmt.hasElsePath) {
			ifStmt.swapPaths();
			ifStmt.removeElsePath();
		} else if (transfers.length == 0)
		{
			if (ifStmt.condition.canHaveSideEffects())
			{
				ifStmt.truthPath.branch.remove();
				if (ifStmt.hasElsePath)
					ifStmt.elsePath.branch.remove();
				replacedWith = ifStmt.condition;
				ifStmt.replaceWith(ifStmt.condition).reanalyseHints();
			}
			else
				ifStmt.detach();
			outer.reanalyseHints();
			return true;
		} else
			ifStmt.truthPath.clearStatements();
	}

	ifStmt.negateCondition();
	ifStmt.truthPath.addStatements(transfers);
	ifStmt.truthPath.reanalyseHints();
	ifStmt.parent.reanalyseHints();
	replacedWith = ifStmt;
	return true;
}

@("negateIfContinue")
unittest
{

	alias assertNegateIfContinue = assertTransformations!(negateIfContinue);
	
	assertNegateIfContinue(
		`if(condition=='timeout'){continue;console.log('\'waitFor()\' timeout');phantom.exit(1)}`,
		`if(condition=='timeout'){continue;console.log('\'waitFor()\' timeout');phantom.exit(1)}`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) continue; b() }`,
		`for(;;) { if (!a) { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) { continue; } b() }`,
		`for(;;) { if (!a) { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) continue; b(); e() }`,
		`for(;;) { if (!a) { b(); e() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) continue; else b(); }`,
		`for(;;) { if (!a) { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) continue; else b(); e() }`,
		`for(;;) { if (!a) { b(); e() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) continue; else { b(); } e() }`,
		`for(;;) { if (!a) { b(); e() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) { c(); continue; } b() }`,
		`for(;;) { if (a) { c() } else { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) { c(); continue; } else f(); b() }`,
		`for(;;) { if (a) { c() } else { f(); b() } }`
	);
	//assertNegateIfContinue( // TODO: See canHaveSideEffects
	//	`for(;;) { if (a) continue; }`,
	//	`for(;;) { }`
	//);
	//assertNegateIfContinue(
	//	`for(;;) { if (a) { continue; } }`,
	//	`for(;;) { }`
	//);
	assertNegateIfContinue(
		`for(;;) { if (a.b) continue; }`,
		`for(;;) { a.b }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) { b(); continue; } }`,
		`for(;;) { if (a) { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { d(); if (a) continue; b() }`,
		`for(;;) { d(); if (!a) { b() } }`
	);
	assertNegateIfContinue(
		`for(;;) { if (a) { if (b) continue; c() } b() }`,
		`for(;;) { if (a) { if (b) continue; c() } b() }`
	);
	//assertNegateIfContinue( // TODO: advanced
	//	`for(;;) { if (a) { if (b) continue; c() } }`,
	//	`for(;;) { if (a) { if (!b) c() } }`
	//);
	//assertNegateIfContinue(
	//	`function b(){ for(;;) { if (a) continue; b() } }`,
	//	`function b(){ for(;;) { if (!a) { b() } } }`
	//);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) { continue; } b() } }`,
		`function b(){ for(;;) { if (!a) { b() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) continue; b(); e() } }`,
		`function b(){ for(;;) { if (!a) { b(); e() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) continue; else b(); } }`,
		`function b(){ for(;;) { if (!a) { b() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) continue; else b(); e() } }`,
		`function b(){ for(;;) { if (!a) { b(); e() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) continue; else { b(); } e() } }`,
		`function b(){ for(;;) { if (!a) { b(); e() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) { c(); continue; } b() } }`,
		`function b(){ for(;;) { if (a) { c() } else { b() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) { c(); continue; } else f(); b() } }`,
		`function b(){ for(;;) { if (a) { c() } else { f(); b() } } }`
	);
	assertNegateIfContinue(
		`function b(){ for(;;) { if (a) { c(); continue; } if (e) { continue; } b() } }`,
		`function b(){ for(;;) { if (a) { c() } else { if (!e) { b() } } } }`
	);
}

bool convertWhileToForLoop(WhileStatementNode node, out Node replacedWith)
{
	auto value = node.children[0].coerceToTernary;

	if (value == Ternary.False)
	{
		node.detach();
		return true;
	}

	ForStatementNode forLoop;

	if (value == Ternary.None)
		forLoop = new ForStatementNode(ForLoop.ExprCStyle, [new SemicolonNode(), node.children[0], new SemicolonNode(), node.children[1]]);
	else {
		forLoop = new ForStatementNode(ForLoop.ExprCStyle, [new SemicolonNode(), new SemicolonNode(), node.children[1]]);
	}

	node.replaceWith(forLoop);
	replacedWith = forLoop;
	forLoop.reanalyseHints();

	return true;
}

@("convertWhileToForLoop")
unittest
{
	alias assertConvertToFor = assertTransformations!(convertWhileToForLoop);

	assertConvertToFor(
		`while(a);`,
		`for(;a;);`
	);

	assertConvertToFor(
		`while(!0);`,
		`for(;;);`
	);

	assertConvertToFor(
		`while(1);`,
		`for(;;);`
	);

	assertConvertToFor(
		`while(true);`,
		`for(;;);`
	);

	assertConvertToFor(
		`while(true) a = 6;`,
		`for(;;) a = 6;`
	);

	assertConvertToFor(
		`while(true) { a = 6; }`,
		`for(;;) { a = 6; }`
	);
}

// NOTE: this needs to run before mergeVariableDeclarationStatements
bool moveVariableDeclarationsIntoForLoops(ForStatementNode stmt, out Node replacedWith)
{
	bool didWork = false;
	while(true)
	{
		auto idx = stmt.getIndex();

		if (idx == 0)
			return didWork;

		if (stmt.parent.children[idx-1].type != NodeType.VariableStatementNode)
			return didWork;

		auto varDecl = stmt.parent.children[idx-1].as!VariableStatementNode;

		switch (stmt.loopType)
		{
			case ForLoop.VarCStyle:
				stmt.children[0].prependChildren(varDecl.children);
				varDecl.detach();
				break;
			case ForLoop.ExprCStyle:
				if (stmt.children[0].type != NodeType.SemicolonNode)
					return didWork;
				varDecl.detach();
				stmt.prependChildren([varDecl]);
				stmt.loopType = ForLoop.VarCStyle;
				break;
			default: return didWork;
		}
		didWork = true;
	}
}

@("moveVariableDeclarationsIntoForLoops")
unittest
{
	alias assertMoveIntoFor = assertTransformations!(moveVariableDeclarationsIntoForLoops);

	assertMoveIntoFor(
		`var a; for(var b;;);`,
		`for(var a, b;;);`
	);
	assertMoveIntoFor(
		`var a = 3; for(var b = 5;;);`,
		`for(var a = 3, b = 5;;);`
	);
	assertMoveIntoFor(
		`var a = 3, d = 7; for(var b = 5;;);`,
		`for(var a = 3, d = 7, b = 5;;);`
	);
	assertMoveIntoFor(
		`var p = 1; var a = 3, d = 7; for(var b = 5;;);`,
		`for(var p = 1, a = 3, d = 7, b = 5;;);`
	);
	assertMoveIntoFor(
		`var p = 1, l; var a = 3, d = 7; for(var b = 5;;);`,
		`for(var p = 1, l, a = 3, d = 7, b = 5;;);`
	);
	assertMoveIntoFor(
		`var a; for(;;);`,
		`for(var a;;);`
	);
	assertMoveIntoFor(
		`var a = 6; for(;b;);`,
		`for(var a = 6;b;);`
	);
	assertMoveIntoFor(
		`var a; for(b = 6;;);`,
		`var a; for(b = 6;;);`
	);
	assertMoveIntoFor(
		`var a; for(var b in d);`,
		`var a; for(var b in d);`
	);
	assertMoveIntoFor(
		`var a; for(let b;;);`,
		`var a; for(let b;;);`
	);
	assertMoveIntoFor(
		`var a; for(const b;;);`,
		`var a; for(const b;;);`
	);
	assertMoveIntoFor(
		`var a; for(var b of d);`,
		`var a; for(var b of d);`
	);
}

// needs to run after (combineBlockStatementIntoExpression, combineFunctionBodyIntoExpression, combineModuleIntoExpression)
bool moveExpressionsIntoForLoops(ForStatementNode stmt, out Node replacedWith)
{
	bool hasInExpressionOperator(Node node)
	{
		import std.algorithm : any;
		if (node.type == NodeType.ExpressionNode || node.type == NodeType.ConditionalExpressionNode || node.type == NodeType.UnaryExpressionNode || node.type == NodeType.ParenthesisNode)
		{
			foreach(c; node.children)
				if (hasInExpressionOperator(c))
					return true;
			return false;
		}
		if (node.type == NodeType.BinaryExpressionNode)
			return node.as!(BinaryExpressionNode).getOperators.any!(o => o.operator == ExpressionOperator.In);
		return false;
	}
	final switch (stmt.loopType)
	{
		case ForLoop.ExprIn: 
		case ForLoop.ExprOf: 
		case ForLoop.VarIn: 
		case ForLoop.VarOf: 
		case ForLoop.ConstIn: 
		case ForLoop.ConstOf: 
		case ForLoop.LetIn: 
		case ForLoop.LetOf: 
		case ForLoop.VarCStyle:
		case ForLoop.ConstCStyle:
		case ForLoop.LetCStyle:
			return false;
		case ForLoop.ExprCStyle:
			break;
	}

	bool didWork = false;
	while(true)
	{
		auto idx = stmt.getIndex();

		if (idx == 0)
			return didWork;

		if (stmt.parent.type != NodeType.BlockStatementNode && stmt.parent.type != NodeType.FunctionBodyNode && stmt.parent.type != NodeType.ModuleNode)
			return didWork;

		auto prev = stmt.parent.children[idx-1];
		if (prev.hints.has(Hint.NonExpression))
			return didWork;

		if (hasInExpressionOperator(prev))
			return didWork;

		prev.detach();

		if (stmt.children[0].type == NodeType.SemicolonNode)
		{
			stmt.prependChildren([prev]);
			stmt.reanalyseHints();
		} else 
		{
			if (stmt.children[0].type == NodeType.ExpressionNode)
			{
				if (prev.type == NodeType.ExpressionNode)
				{
					stmt.children[0].prependChildren(prev.children);
				} else
					stmt.children[0].prependChildren([prev]);
			} else
			{
				if (prev.type == NodeType.ExpressionNode)
				{
					prev.addChild(stmt.children[0]);
					stmt.children[0] = prev;
				} else
				{
					auto old = stmt.children[0];
					stmt.children[0].replaceWith(new ExpressionNode([prev])).addChild(old);
				}
			}
			stmt.children[0].reanalyseHints();
		}
		didWork = true;
	}
}

@("moveExpressionsIntoForLoops")
unittest
{
	alias assertMoveIntoFor = assertTransformations!(moveExpressionsIntoForLoops);

	assertMoveIntoFor(
		`a = 6; for(;;);`,
		`for(a = 6;;);`
	);
	assertMoveIntoFor(
		`a = 6; for(b = 6;;);`,
		`for(a = 6, b = 6;;);`
	);
	assertMoveIntoFor(
		`a = 6; for(b = 6, c = 7;;);`,
		`for(a = 6, b = 6, c = 7;;);`
	);
	assertMoveIntoFor(
		`a = 6, b = 8; for(c = 7, d = 9;;);`,
		`for(a = 6, b = 8, c = 7, d = 9;;);`
	);
	assertMoveIntoFor(
		`p = 9; a = 6, b = 8; for(c = 7, d = 9;;);`,
		`for(p = 9, a = 6, b = 8, c = 7, d = 9;;);`
	);
	assertMoveIntoFor(
		`var a = 6; for(;;);`,
		`var a = 6; for(;;);`
	);
	assertMoveIntoFor(
		`a = 6; for(b in k);`,
		`a = 6; for(b in k);`
	);
	assertMoveIntoFor(
		`a = 6; for(b of k);`,
		`a = 6; for(b of k);`
	);
	assertMoveIntoFor(
		`a = 6; for(var b in k);`,
		`a = 6; for(var b in k);`
	);
	assertMoveIntoFor(
		`a = 6; for(var b of k);`,
		`a = 6; for(var b of k);`
	);
	assertMoveIntoFor(
		`a = 6; for(let b in k);`,
		`a = 6; for(let b in k);`
	);
	assertMoveIntoFor(
		`a = 6; for(let b of k);`,
		`a = 6; for(let b of k);`
	);
	assertMoveIntoFor(
		`a = 6; for(const b in k);`,
		`a = 6; for(const b in k);`
	);
	assertMoveIntoFor(
		`a = 6; for(const b of k);`,
		`a = 6; for(const b of k);`
	);
	assertMoveIntoFor(
		`bla in hup ? d() : b(); for(;;);`,
		`bla in hup ? d() : b(); for(;;);`
	);
	assertMoveIntoFor(
		`k(), bla in hup ? d() : b(); for(;;);`,
		`k(), bla in hup ? d() : b(); for(;;);`
	);
}