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
module es6.transforms.returns;

import es6.nodes;
import es6.scopes;
import es6.transforms.conditionals;
import es6.analyse;
import es6.transforms.expressions;
import option;
import std.algorithm : each, until, find, countUntil;
import std.range : retro;
import std.array : array, appender;

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

bool negateReturningIf(Scope s)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	bool modified = false;
	foreach_reverse(branch; s.branch.children)
	{
		if (branch.entry.parent.type != NodeType.IfStatementNode ||
			!branch.hasHint!(Hint.Return))
			continue;

		modified = true;
		auto ifStmt = branch.entry.parent.as!IfStatementNode;
		branch.removeHint!(Hint.Return);

		auto transfers = ifStmt.parent.detachStatementsAfter(ifStmt);
		if (transfers.length == 0)
		{
			if (ifStmt.hasElsePath)
			{
				ifStmt.negateCondition();

				auto elsePath = ifStmt.elsePath.node;
				ifStmt.removeElsePath();
				ifStmt.truthPath.node.replaceWith(elsePath);
				continue;
			}
			if (ifStmt.truthPath.isSingleStatement())
			{
				ifStmt.truthPath.branch.remove();
				ifStmt.truthPath.shread();
				ifStmt.replaceWith(ifStmt.condition).reanalyseHints();
			}
			else {
				ifStmt.truthPath.as!(BlockStatementNode).dropAllFrom(NodeType.ReturnStatementNode);
				ifStmt.truthPath.reanalyseHints();
			}
			continue;
		}
		IfPath target;
		if (ifStmt.truthPath.isSingleStatement())
		{
			ifStmt.negateCondition();
			if (ifStmt.hasElsePath)
			{
				auto elsePath = ifStmt.elsePath.node;
				ifStmt.removeElsePath();
				ifStmt.truthPath.node.replaceWith(elsePath);
			} else
			{
				ifStmt.truthPath.clearStatements();
			}
			target = ifStmt.truthPath;
		} else
		{
			ifStmt.truthPath.as!(BlockStatementNode).dropAllFrom(NodeType.ReturnStatementNode);
			ifStmt.truthPath.reanalyseHints();
			ifStmt.forceElsePath();
			target = ifStmt.elsePath();
		}
		target.addStatements(transfers);

		ifStmt.parent.reanalyseHints();
		branch.reanalyseHints();
		ifStmt.parent.branch.reanalyseHints();
		continue;
	}
	return modified;
}

@("negateReturningIf")
unittest
{
	void assertReturnIfNegation(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		got.runTransform!(negateReturningIf)(file,line);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emitVisitor(got).shouldEqual(emitVisitor(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
	// Note: Need to test that we don't apply this optimisation if the `if (c) return;` is nested in a if-statement
	assertReturnIfNegation(
		`function a() { if (a) return; a = 5; }`,
		`function a() { if (!a) { b = 5; } }`
	).shouldThrow();
	assertReturnIfNegation(
		`function a() { if (b) return; c = 5; }`,
		`function a() { if (!b) { c = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) return; }`,
		`function a() { b }`
	);
	/// ditto
	assertReturnIfNegation(
		"function d() { if (!a) { return; }; }",
		"function d() { !a }"
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (a) return; b = 5; if (c) return; d = 7; }`,
		`function a() { if (!a) { b = 5; if (!c) { d = 7 } } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) { d = 5; return; } e = 5; }`,
		`function a() { if (b) { d = 5; } else { e = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (e) if (b) if (c) return; d = 5; }`,
		`function a() { if (e) if (b) if (c) return; d = 5; }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (e && b && c) return; d = 5; }`,
		`function a() { if (!(e && b && c)) { d = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) return; else d = 4; }`,
		`function a() { if (!b) d = 4; }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) return; else d = 4; f = 5;}`,
		`function a() { if (!b) { d = 4; f = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) { return; } else { d = 4; } f = 5;}`,
		`function a() { if (!b) { d = 4; f = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) { g = 6; return; } else { d = 4; } f = 5;}`,
		`function a() { if (b) { g=6 } else { d=4; f=5 } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) { d = 5; return; } }`,
		`function a() { if (b) { d = 5; } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { if (b) { d = 5; return; } else d = 4; f = 5;}`,
		`function a() { if (b) { d=5 } else { d=4; f=5 } }`
	);
	/// ditto
	assertReturnIfNegation(
		`function a() { for (var k in keys) { if (b) { return; } else d = 4; f = 5; } g = 66; }`,
		`function a() { for (var k in keys) { if (b) { return } else  d = 4; f = 5; } g = 66; }`
	);
	/// ditto
	assertReturnIfNegation(
		`function b(z) { if (z.p !== 'value') return; if (z.q === k) return; k = z.v; }`,
		"function b(z) { if (!(z.p !== 'value')) { if (!(z.q === k)) { k=z.v } } }"
	);
	/// ditto
	assertReturnIfNegation(
		`function b(z) { if (z.p !== 'value') { return; } if (z.q === k) { return; } k = z.v; }`,
		"function b(z) { if (!(z.p !== 'value')) { if (!(z.q === k)) { k=z.v } } }"
	);
	/// ditto
	assertReturnIfNegation(
		`function b() { switch (bla) { case 5: return; }; op() }`,
		`function b() { switch (bla) { case 5: return; }; op() }`
	);
	/// ditto
	assertReturnIfNegation(
		`function b() { if (a) for(;;)return; op() }`,
		`function b() { if (a) for(;;)return; op() }`
	);
	/// ditto
	assertReturnIfNegation(
		`function b() { if (a) { for(;;)return; } op() }`,
		`function b() { if (a) { for(;;)return }; op() }`
	);
	assertReturnIfNegation(
		`function b() { if (d(), a) return; op() }`,
		`function b() { if (d(), !a) { op() } }`
	);
	assertReturnIfNegation(
		`function b() { if (d(), a == 9) return; op() }`,
		`function b() { if (d(), !(a == 9)) { op() } }`
	);
	assertReturnIfNegation(
		`function b() { if (a) return; return 7 }`,
		`function b() { if (!a) { return 7 } }`
	);
	assertReturnIfNegation(
		`function b() { if (c) return undefined; if (k) return 5; return 7;}`,
		`function b() { if (!c) { if (k) return 5; return 7; } }`
	);
	assertReturnIfNegation(
		`function b() { if (c) return void 0; if (k) return 5; return 7;}`,
		`function b() { if (!c) { if (k) return 5; return 7; } }`
	);
}

void removeRedundantElse(IfStatementNode ifStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (!ifStmt.hasElsePath)
		return;

	auto currentParent = ifStmt.parent;
	if (ifStmt.truthPath.hints.has(Hint.Return | Hint.ReturnValue))
	{
		auto transfer = ifStmt.elsePath;
		ifStmt.branch.hints |= transfer.branch.hints;
		ifStmt.removeElsePath();
		ifStmt.insertAfter(transfer);
		ifStmt.reanalyseHints();
	} else if (ifStmt.elsePath.hints.has(Hint.Return | Hint.ReturnValue))
	{
		ifStmt.negateCondition();
		ifStmt.swapPaths();
		auto transfer = ifStmt.elsePath;
		ifStmt.removeElsePath();
		ifStmt.insertAfter(transfer);
		ifStmt.reanalyseHints();
	}
	if (currentParent != ifStmt.parent)
		replacedWith = ifStmt.parent;
}

@("removeRedundantElse")
unittest
{
	alias assertRedundantElse = assertTransformations!removeRedundantElse;

	assertRedundantElse(
		`function cd() { if (a) { return 7; } else d(); }`,
		`function cd() { if (a) { return 7; } d(); }`
	);
	assertRedundantElse(
		`function cd() { if (a) if (b) return 4; else return 5; else return 6; }`,
		`function cd() { if (a) { if (b) return 4; return 5; } return 6; }`
	);
	assertRedundantElse(
		`function cd() { if (a) { return c; } else if (b) return d; }`,
		`function cd() { if (a) { return c; } if (b) return d; }`
	);

	assertRedundantElse(
		`function cd() { if (a) { return 7; } else if (b) return 5; d(); }`,
		`function cd() { if (a) { return 7; } if (b) return 5; d(); }`
	);

	assertRedundantElse(
		`function cd() { if (a) { return k(), 7; } else { return p(), 5; } }`,
		`function cd() { if (a) { return k(), 7; } return p(), 5; }`
	);
	assertRedundantElse(
		`function cd() { if (a) { d() } else return 7; }`,
		`function cd() { if (!a) return 7; d(); }`
	);
	assertRedundantElse(
		`function bla(s) { switch (s) { default: if (c) return a; else return b } }`,
		`function bla(s) { switch (s) { default: if (c) return a; return b}}`
	);
	assertRedundantElse(
		`function bla(s) { switch (s) { case 5: if (c) return a; else return b } }`,
		`function bla(s) { switch (s) { case 5: if (c) return a; return b}}`
	);
	assertRedundantElse(
		`function bla(s) { switch (s) { default: if (c) return a; else if (d) return b; else return e } }`,
		`function bla(s) { switch (s) { default: if (c) return a; if (d) return b; return e}}`
	);
	assertRedundantElse(
		`function bla(s) { switch (s) { case 5: if (c) return a; else if (d) return b; else return e } }`,
		`function bla(s) { switch (s) { case 5: if (c) return a; if (d) return b; return e}}`
	);
}

Node createVoid0Node()
{
	return new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Void)],new DecimalLiteralNode(cast(const(ubyte)[])"0"));
}

bool combineReturnStatements(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	Node parenthesizeIfNecessary(Node n)
	{
		if (n.type != NodeType.ExpressionNode)
			return n;
		if (n.children.length == 1)
			return n.replaceWith(n.children[0]);
		return n.parenthesizeExpression;
	}
	import std.algorithm : any, map, all, fold;
	import std.range : retro, zip, drop, take;
	bool combineReturnStatements(Branch branch)
	{
		foreach(b; branch.children)
			if (b.children.length > 0)
				if (combineReturnStatements(b))
					return true;

		if (branch.children.length == 0)
			return false;

		// the last return of the branch, if any
		auto baseReturn = branch.entry.children.retro.find!(c => c.type == NodeType.ReturnStatementNode);

		auto tailIgnoreCnt = branch.entry.children.retro.countUntil!(c => c.type == NodeType.ReturnStatementNode || c.type == NodeType.IfStatementNode);
		if (tailIgnoreCnt == -1)
			return false;

		if (branch.entry.children.retro.take(tailIgnoreCnt).any!(c => c.hints.has(Hint.NonExpression)))
			return false;

		// the maximum index from the end where each node is either an if or a return 
		auto idx = branch.entry.children.retro.drop(tailIgnoreCnt).countUntil!(c => c.type != NodeType.IfStatementNode && c.type != NodeType.ReturnStatementNode);

		// if there are fewer than 2 return/ifs
		if (idx > -1 && idx < 2)
			return false;

		Node[] validNodes;
		Branch[] validBranches;
		ptrdiff_t startingBranchIdx = 0;

		if (idx == -1)
		{
			// when all nodes in the branch are either ifs or returns its simple
			validBranches = branch.children;
			validNodes = branch.entry.children;
		}
		else {

			validNodes = branch.entry.children[$-idx-tailIgnoreCnt..$-tailIgnoreCnt];
			// else we take only those nodes and branches starting from idx from the back
			if (validNodes[0].type != NodeType.IfStatementNode)
				return false;

			auto startingBranch = validNodes[0].as!(IfStatementNode).truthPath.branch;
			startingBranchIdx = branch.children.countUntil!(c => c is startingBranch);
			if (startingBranchIdx == -1)
				return false;
			validBranches = branch.children[startingBranchIdx..$];
		}

		if (validBranches.length < 1)
			return false;

		// if any sub branch has children
		if (validBranches.any!(c => c.children.length > 0 || c.entry.parent.type != NodeType.IfStatementNode))
			return false;

		auto ifStmts = validBranches.map!(c => c.entry.parent.as!IfStatementNode);

		// if any ifstatement has an elsepath
		if (ifStmts.any!(i => i.hasElsePath))
			return false;
		
		// if any ifstatement has no return
		if (!ifStmts.all!(i => i.truthPath.node.hints.has(Hint.Return | Hint.ReturnValue)))
			return false;

		// if any if statement have statements that are non expressions (besides return values)
		if (!ifStmts.all!(i => i.truthPath.type == NodeType.ReturnStatementNode || i.truthPath.children.all!(c => c.type == NodeType.ReturnStatementNode)))
			return false;

		// if this branch doesn't return
		if (baseReturn.empty)
		{
			// we can still apply the optimisation BUT only if we have at least 2 branches
			if (validBranches.length < 2)
				return false;

			// unless we are in a for or while loop
			if (branch.entry.inLoop)
				return false;

			// and only if there are NO statements after this branch ends
			auto walk = branch.entry;
			while (walk !is null && walk.type != NodeType.FunctionBodyNode) {
				if (!walk.isLastSibling)
					return false;
				walk = walk.parent;
			}
		}

		// map all the return values from all if statements
		auto returnValues = ifStmts.map!((i){
			if (i.truthPath.type == NodeType.BlockStatementNode)
				return i.truthPath.children[0].children[0];
			if (i.truthPath.children.length == 0)
				return createVoid0Node();
			return i.truthPath.children[0];
		}).map!(parenthesizeIfNecessary);

		auto conditions = ifStmts.map!(i => i.condition);

		// get the final value that we need to return
		Node lastValue;
		if (baseReturn.empty || baseReturn.front().children.length == 0)
		{
			if (tailIgnoreCnt == 0)
				lastValue = createVoid0Node();
			else
			{
				auto app = appender!(Node[]);
				app.reserve(tailIgnoreCnt);
				foreach(c; branch.entry.children.retro.take(tailIgnoreCnt).retro)
					if (c.type == NodeType.ExpressionNode)
						app.put(c.children);
					else
						app.put(c);
				if (app.data.length == 1)
				{
					auto paren = new ParenthesisNode(app.data);
					lastValue = new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Void)],paren);
					paren.reanalyseHints();
				}
				else
				{
					auto expr = new ExpressionNode(app.data);
					lastValue = new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Void)],new ParenthesisNode([expr]));
					expr.reanalyseHints();
				}
			}
		} else {
			lastValue = parenthesizeIfNecessary(baseReturn.front().children[0]);
		}

		// zip everything together to a (nested) conditionalstatement
		auto condition = conditions.zip(returnValues).retro.fold!((acc,b){
			return new ConditionalExpressionNode(parenthesizeIfNecessary(b[0]),parenthesizeIfNecessary(b[1]),acc);
		})(lastValue);

		auto returnNode = new ReturnStatementNode(condition);

		// when all statements in this branch were either ifs or returns
		if (idx == -1) {
			// if this branch is a block statement node
			if (branch.entry.type == NodeType.BlockStatementNode)
				branch.entry.replaceWith(returnNode);
			else
			{
				// else we replace all children with the new return node
				branch.entry.children = [returnNode];
				returnNode.parent = branch.entry;
			}
		} else {
			// when only some statements applied for this transformation
			// we need to keep them around
			auto startIdx = branch.entry.children.length - idx - tailIgnoreCnt;
			branch.entry.children = branch.entry.children[0..startIdx+1];
			branch.entry.children[startIdx] = returnNode;
			returnNode.parent = branch.entry;
		}
		returnNode.assignBranch(branch);
		branch.children = branch.children[0..startingBranchIdx];
		returnNode.reanalyseHints();
		branch.hints |= Hint.ReturnValue;

		return true;
	}
	return combineReturnStatements(scp.branch);
}

@("combineReturnStatements")
unittest
{
	alias assertCombineReturn = assertTransformations!combineReturnStatements;

	assertCombineReturn(
		`function cd() { if (a) return 7; return 5; }`,
		`function cd() { return a ? 7 : 5 }`
	);
	assertCombineReturn(
		`function cd() { if (a) { return 7; } return 5; }`,
		`function cd() { return a ? 7 : 5 }`
	);
	assertCombineReturn(
		`function cd() { if (b) { if (a) return 7; return 5; } }`,
		`function cd() { if (b) return a ? 7 : 5 }`
	);
	assertCombineReturn(
		`function cd() { if (b) { if (a) return 7; return 5; } return 6; }`,
		`function cd() { return b ? a ? 7 : 5 : 6 }`
	);
	assertCombineReturn(
		`function cd() { if (a) { return k(), 7; } return p(), 5; }`,
		`function cd() { return a ? (k(),7) : (p(),5) }`
	);
	assertCombineReturn(
		"function a() { if (a) { if (d) return 4; if (c) { return bla; } return 7; } }",
		"function a() { if (a) return d ? 4 : c ? bla : 7 }"
	);
	assertCombineReturn(
		"function a() { if (a) { for(;a < 5;a++)e(); if (d) return 4; if (c) { return bla; } return 7; } }",
		"function a() { if (a) { for(;a < 5;a++)e(); return d ? 4 : c ? bla : 7 } }"
	);
	/// ditto
	assertCombineReturn(
		"function a() { if (a) { if (d) return 4; if (c) { return bla; } } return 7; }",
		"function a() { if (a) { if (d) return 4; if (c) { return bla; } } return 7 }"
	);
	assertCombineReturn(
		`function a() { if (a) { d = 5; return b } }`,
		`function a() { if (a) { d = 5; return b } }`
	);
	assertCombineReturn(
		`function a(){ if (a) return bla; if (b) return alb; }`,
		`function a(){ return a ? bla : b ? alb : void 0}`
	);
	/// ditto
	assertCombineReturn(
		`function cd() { if (a) { return 7; } if (b) return 5; }`,
		`function cd() { return a ? 7 : b ? 5 : void 0 }`
	);
	assertCombineReturn(
		`function cd() { if (b) { if (a) return 7; return 5; } if (c) { if (d) return 7; return 5; } }`,
		`function cd() { return b ? a ? 7 : 5 : c ? d ? 7 : 5 : void 0 }`
	);
	assertCombineReturn(
		"function a() { if (a) { for(;a < 5;a++)e(); if (d) return 4; if (c) { return bla; } } }",
		"function a() { if (a) { for(;a < 5;a++)e(); return d ? 4 : c ? bla : void 0 } }"
	);
	assertCombineReturn(
		`function bla(s) { switch (s) { default: if (c) return a; return b}}`,
		`function bla(s) { switch (s) { default: return c ? a : b } }`,
	);
	assertCombineReturn(
		`function bla(s) { switch (s) { case 5: if (c) return a; return b; case 6: if (d) return 5; return 7; }}`,
		`function bla(s) { switch (s) { case 5: return c ? a : b; case 6: return d ? 5 : 7 } }`,
	);
	assertCombineReturn(
		`function a(){ if (a) { if (c=4,b) return 5; return 6 } return 7 }`,
		`function a(){ return a ? (c=4,b) ? 5 : 6 : 7 }`,
	);
	assertCombineReturn(
		`function a(){
			if ( (elem = context.getElementById( m )) ) {
				if ( elem.id === m ) {
					results.push( elem );
					return results;
				}
			} else {
				return results;
			}
		}`,
		`function a(){
			if ( (elem = context.getElementById( m )) ) {
				if ( elem.id === m ) {
					results.push( elem );
					return results;
				}
			} else {
				return results;
			}
		}`
	);
	assertCombineReturn(
		`function a(b) { if (b instanceof a) return b; if (!(this instanceof a)) return new a(b); this._wrapped = b }`,
        `function a(b){return b instanceof a?b:!(this instanceof a)?new a(b):void(this._wrapped=b)}`
    );
	assertCombineReturn(
		`function a(b) { k = 6; if (b instanceof a) return b; if (!(this instanceof a)) return new a(b); this._wrapped = b; b = 5, e = 6; p = 8;}`,
        `function a(b){k=6;return b instanceof a?b:!(this instanceof a)?new a(b):void(this._wrapped=b,b=5,e=6,p=8)}`
    );
    assertCombineReturn(
    	`function a() { switch(a) { case 5: if (a) return 6; if (b) return 5; } b(); }`,
    	`function a() { switch(a) { case 5: if (a) return 6; if (b) return 5; } b(); }`
    );
    assertCombineReturn(
    	`function a() { switch(a) { case 5: if (a) return 6; if (b) return 5; } }`,
    	`function a() { switch(a) { case 5: return a ? 6 : b ? 5 : void 0 } }`
    );
    assertCombineReturn(
    	`function a() { switch(a) { case 5: if (a) return 6; if (b) return 5; return 7; } }`,
    	`function a() { switch(a) { case 5: return a ? 6 : b ? 5 : 7 } }`
    );
    assertCombineReturn(
    	`function a() { for(;;) { if (a) return 6; if (b) return 5; } }`,
    	`function a() { for(;;) { if (a) return 6; if (b) return 5; } }`
    );
    assertCombineReturn(
    	`function a() { for(;;) { if (a) return 6; if (b) return 5; return 7; } }`,
    	`function a() { for(;;) return a ? 6 : b ? 5 : 7 }`
    );
    assertCombineReturn(
    	`function a() { while(true) { if (a) return 6; if (b) return 5; } }`,
    	`function a() { while(true) { if (a) return 6; if (b) return 5; } }`
    );
    assertCombineReturn(
    	`function a() { while(true) { if (a) return 6; if (b) return 5; return 7; } }`,
    	`function a() { while(true) return a ? 6 : b ? 5 : 7 }`
    );
    assertCombineReturn(
    	`function a() { do { if (a) return 6; if (b) return 5; } while(true); }`,
    	`function a() { do { if (a) return 6; if (b) return 5; } while(true); }`
    );
    assertCombineReturn(
    	`function a() { do { if (a) return 6; if (b) return 5; return 7; } while(true); }`,
    	`function a() { do return a ? 6 : b ? 5 : 7; while(true); }`
    );
    assertCombineReturn(
    	`function a() { if (a && c) return bla; if (b && d) return alb } a();`,
		"function a() { return a && c ? bla : b && d ? alb : void 0 } a();"
    );
	// TODO: this doesn't work since we start at the end looking for the maximum length of ifs/returns statements of at least size 2, and d() fails that
	// to fix it we need to skip any expressions at the end, which we will combine with the void 0 later on
	//assertCombineReturn(
	//	`function cd() { if (a) { return 7; } if (b) return 5; d(); }`,
	//	`function cd() { return a ? 7 : b ? 5 : void(d()) }`
	//);
	//
	//	/// This is more a else return empty optimisation
	//assertTransformation(
	//	`function bla() { if (b) { if (a) { return a } else { return } } }`,
	//	`function bla() { if (b) { if (a) return a } }`
	//);
}

bool moveExpressionsIntoReturn(ReturnStatementNode retStmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (retStmt.parent.type != NodeType.BlockStatementNode &&
		retStmt.parent.type != NodeType.ModuleNode &&
		retStmt.parent.type != NodeType.FunctionBodyNode)
		return false;
	if (retStmt.children.length == 0)
		return false;
	auto idx = retStmt.parent.getIndexOfChild(retStmt);
	if (idx == 0)
		return false;
	auto candidates = retStmt.parent.children[0..idx];
	auto exprs = candidates.retro.until!(c => c.hints.has(Hint.NonExpression)).array;
	if (exprs.length == 0)
		return false;

	exprs.each!(expr => expr.detach());
	auto child = retStmt.children[0];
	if (child.type != NodeType.ExpressionNode)
		child.replaceWith(new ExpressionNode([])).addChild(child);
	child = retStmt.children[0];
	foreach(expr; exprs)
		if (expr.type == NodeType.ExpressionNode)
			child.prependChildren(expr.children);
		else
			child.prependChildren([expr]);
	child.reanalyseHints();
	return true;
}

@("moveExpressionsIntoReturn")
unittest
{
	alias assertMoveIntoReturn = assertTransformations!(moveExpressionsIntoReturn);
	assertMoveIntoReturn(
		`function foo() { bla(); return; }`,
		`function foo() { bla(); return; }`
	);
	assertMoveIntoReturn(
		`function foo() { bla(); return 4; }`,
		`function foo() { return bla(), 4; }`
	);
	assertMoveIntoReturn(
		`function foo() { bla(), hup(); return 4; }`,
		`function foo() { return bla(), hup(), 4; }`,
	);
	assertMoveIntoReturn(
		`function foo() { bla(); hup(); return 4; }`,
		`function foo() { return bla(), hup(), 4; }`
	);
	assertMoveIntoReturn(
		`function foo() { bla(), bar(); hup(); return 4; }`,
		`function foo() { return bla(), bar(), hup(), 4; }`
	);
	assertMoveIntoReturn(
		`function foo() { if(b) kaz(); bla(), bar(); hup(); return 4; }`,
		`function foo() { if(b) kaz(); return bla(), bar(), hup(), 4; }`,
	);
	assertMoveIntoReturn(
		`function foo() { if(b) kaz(); return 4; }`,
		`function foo() { if(b) kaz(); return 4; }`,
	);
	assertMoveIntoReturn(
		`function foo() { bla(), hup(); return foo(), 4; }`,
		`function foo() { return bla(), hup(), foo(), 4; }`,
	);
	assertMoveIntoReturn(
		`function foo() { bla(); hup(); return foo(), 4; }`,
		`function foo() { return bla(), hup(), foo(), 4; }`,
	);
	assertMoveIntoReturn(
		"function d(a) { b = 3; return p; }",
		"function d(a) { return b = 3,p }"
	);
}

bool rewriteReturnUndefined(ReturnStatementNode node, out Node replacedWith)
{
	if (node.children.length == 0)
		return false;

	auto value = node.children[0];
	if (!(value.isVoid0 || value.isUndefined))
		return false;

	value.detach();
	value.shread();

	return true;
}

@("rewriteReturnUndefined")
unittest
{	
	alias assertReturnUndefined = assertTransformations!(rewriteReturnUndefined);

	assertReturnUndefined(
		`function a(){ return; }`,
		`function a(){ return; }`
	);
	assertReturnUndefined(
		`function a(){ return a; }`,
		`function a(){ return a; }`
	);
	assertReturnUndefined(
		`function a(){ return void 0; }`,
		`function a(){ return; }`
	);
	assertReturnUndefined(
		`function a(){ return void(sideEffect()); }`,
		`function a(){ return void(sideEffect()); }`
	);
	assertReturnUndefined(
		`function a(){ return undefined; }`,
		`function a(){ return; }`
	);
}

bool removeUnusedReturn(ReturnStatementNode node, out Node replacedWith)
{
	if (node.children.length > 0)
		return false;

	if (node.inLoop)
		return false;

	if (!node.isLastInFunctionBody)
		return false;

	auto parent = node.parent;
	auto branch = node.branch;
	if (node.parent.type == NodeType.IfStatementNode)
	{
		replacedWith = node.replaceWith(new EmptyStatementNode());
	} else 
	{
		node.detach();
		node.shread();		
	}
	parent.reanalyseHints();
	branch.reanalyseHints();
	return true;
}

@("removeUnusedReturn")
unittest
{
	alias assertRemoveUnused = assertTransformations!(removeUnusedReturn);

	assertRemoveUnused(
		`function a(){ return; }`,
		`function a(){ }`
	);
	assertRemoveUnused(
		`function a(){ return b; }`,
		`function a(){ return b; }`
	);
	assertRemoveUnused(
		`function a(){ if (b) return; d(); }`,
		`function a(){ if (b) return; d(); }`
	);
	assertRemoveUnused(
		`function a(){ d(); if (b) return; }`,
		`function a(){ d(); if (b) ; }`
	);
	assertRemoveUnused(
		`function a(){ while(a++) if (b) return; }`,
		`function a(){ while(a++) if (b) return; }`
	);
	assertRemoveUnused(
		`function a(){ switch(a) {case 5: if (b) return; case 6: return 6; case 7: return; }}`,
		`function a(){ switch(a) {case 5: if (b) return; case 6: return 6; case 7: ; }}`
	);
}





