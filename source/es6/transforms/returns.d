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

bool negateReturningIf(Scope s)
{
	foreach(branch; s.branch.children)
	{
		if (branch.entry.parent.type != NodeType.IfStatementNode ||
			!branch.hasHint!(Hint.Return))
			continue;
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
				return true;
			}
			if (ifStmt.truthPath.isSingleStatement())
			{
				ifStmt.replaceWith(ifStmt.condition);
			}
			else {
				ifStmt.truthPath.as!(BlockStatementNode).dropAllAfter(NodeType.ReturnStatementNode);
				return false;
			}
			return true;
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
			ifStmt.truthPath.as!(BlockStatementNode).dropAllAfter(NodeType.ReturnStatementNode);
			ifStmt.forceElsePath();
			target = ifStmt.elsePath();
		}
		target.addStatements(transfers);
	}
	return false;
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
		got.assertTreeInternals(file,line);
		got.runTransform!(negateReturningIf);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected));
	}
	// Note: Need to test that we don't apply this optimisation if the `if (c) return;` is nested in a if-statement
	assertReturnIfNegation(
		`function a() { if (a) return; a = 5; }`,
		`function a() { if (!a) { a = 5; } }`
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
		`function a() { if (a) return; a = 5; if (b) return; a = 7; }`,
		`function a() { if (!a) { a = 5; if (!b) { a = 7 } } }`
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