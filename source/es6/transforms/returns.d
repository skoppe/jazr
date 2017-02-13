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
import option;
import std.algorithm : each;
import es6.analyse;
import es6.transforms.expressions;

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
		//transfers.each!(t => t.assignBranch(target.branch));

		ifStmt.parent.reanalyseHints();
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
		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
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
}

void removeRedundantElse(IfStatementNode ifStmt, out Node replacedWith)
{
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
}

Node createVoid0Node()
{
	return new UnaryExpressionNode([new PrefixExpressionNode(Prefix.Void)],new DecimalLiteralNode("0"));
}

void combineReturnStatements(Scope scp)
{
	Node parenthesizeIfNecessary(Node n)
	{
		if (n.type != NodeType.ExpressionNode)
			return n;
		if (n.children.length == 1)
			return n.replaceWith(n.children[0]);
		return n.parenthesizeExpression;
	}
	import std.algorithm : any, map, all, fold;
	import std.range : retro, zip;
	void combineReturnStatements(Branch branch)
	{
		foreach(b; branch.children)
			if (b.children.length > 0)
				combineReturnStatements(b);

		if (branch.children.length == 0)
			return;

		// the last return of the branch, if any
		auto baseReturn = branch.entry.children.retro.find!(c => c.type == NodeType.ReturnStatementNode);

		// the maximum index from the end where each node is either an if or a return 
		auto idx = branch.entry.children.retro.countUntil!(c => c.type != NodeType.IfStatementNode && c.type != NodeType.ReturnStatementNode);

		// if there are fewer than 2 return/ifs
		if (idx > -1 && idx < 2)
			return;

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
			// else we take only those nodes and branches starting from idx from the back
			if (!branch.entry.children[$-idx].type == NodeType.IfStatementNode)
				return;

			validNodes = branch.entry.children[$-idx..$];

			auto startingBranch = validNodes[0].as!(IfStatementNode).truthPath.branch;
			startingBranchIdx = branch.children.countUntil!(c => c is startingBranch);
			assert(startingBranchIdx != -1);
			validBranches = branch.children[startingBranchIdx..$];
		}

		if (validBranches.length < 1)
			return;

		// if any sub branch has children
		if (validBranches.any!(c => c.children.length > 0 || c.entry.parent.type != NodeType.IfStatementNode))
			return;

		auto ifStmts = validBranches.map!(c => c.entry.parent.as!IfStatementNode);

		// if any ifstatement has an elsepath
		if (ifStmts.any!(i => i.hasElsePath))
			return;
		
		// if any ifstatement has no return
		if (!ifStmts.all!(i => i.truthPath.node.hints.has(Hint.Return | Hint.ReturnValue)))
			return;

		// if any if statement have statements that are non expressions (besides return values)
		if (!ifStmts.all!(i => i.truthPath.type == NodeType.ReturnStatementNode || i.truthPath.children.all!(c => c.type == NodeType.ReturnStatementNode)))
			return;

		// if this branch doesn't return
		if (baseReturn.empty)
		{
			// we can still apply the optimisation BUT only if we have at least 2 branches
			if (validBranches.length < 2)
				return;

			// and only if there are NO statements after this branch ends
			auto walk = branch.entry;
			do {
				if (!walk.isLastSibling)
					return;
				walk = walk.parent;
			} while (walk !is null && walk.type != NodeType.FunctionBodyNode);
		}

		// map all the return values from all if statements
		auto returnValues = ifStmts.map!((i){
			if (i.truthPath.type == NodeType.BlockStatementNode)
				return i.truthPath.children[0].children[0];
			return i.truthPath.children[0];
		}).map!(parenthesizeIfNecessary);

		auto conditions = ifStmts.map!(i => i.condition);

		// get the final value that we need to return
		Node lastValue;
		if (baseReturn.empty || baseReturn.front().children.length == 0)
		{
			lastValue = createVoid0Node();
		} else {
			lastValue = parenthesizeIfNecessary(baseReturn.front().children[0]);
		}

		// zip everything together to a (nested) conditionalstatement
		auto condition = conditions.zip(returnValues).retro.fold!((acc,b){
			return new ConditionalExpressionNode(b[0],b[1],acc);
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
			auto startIdx = branch.entry.children.length - idx;
			branch.entry.children = branch.entry.children[0..startIdx+1];
			branch.entry.children[startIdx] = returnNode;
			returnNode.parent = branch.entry;
		}
		returnNode.assignBranch(branch);
		branch.children = branch.children[0..startingBranchIdx];
		returnNode.reanalyseHints();
		branch.hints |= Hint.ReturnValue;
	}
	combineReturnStatements(scp.branch);
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
		}`);
	// TODO: this doesn't work since we start at the end looking for the maximum length of ifs/returns statements of at least size 2, and d() fails that
	// to fix it we need to skip any expressions at the end, which we will combine with the void 0 later on
	//assertCombineReturn(
	//	`function cd() { if (a) { return 7; } if (b) return 5; d(); }`,
	//	`function cd() { return a ? 7 : b ? 5 : (d(),void 0) }`
	//);
	//
	//	/// This is more a else return empty optimisation
	//assertTransformation(
	//	`function bla() { if (b) { if (a) { return a } else { return } } }`,
	//	`function bla() { if (b) { if (a) return a } }`
	//);
}



