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
module es6.minifier;

import es6.tokens;
import es6.scopes;
import std.format : formattedWrite, format;
import std.algorithm : each, countUntil;
import std.range : lockstep;
import option;
import es6.utils;
import es6.analyse;
import es6.transformer;
import es6.transforms;
import es6.nodes;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import std.stdio;
}

void minify(Node root, in string file = __FILE__, in size_t line = __LINE__)
{
	debug {
		root.assertTreeInternals();
	}
	root.runTransform!(
		mergeVariableDeclarationStatements,
		moveStringComparisonToLeftOperand,
		hoistFunctions,
		removeRedundantBlockStatements,
		shortenLiteralPropertyNames,
		combineBlockStatementIntoExpression,
		combineFunctionBodyIntoExpression,
		combineModuleIntoExpression,
		simplifyBinaryExpressions,
		simplifyStaticIfStatement,
		simplifyStaticConditional,
		removeUnnecessaryParenthesis,
		removeUnusedParameters,
		shortenBooleanNodes,
		simplifyRedundantAssignmentExpressions,
		moveExpressionsIntoIfCond,
		moveExpressionsIntoReturn,
		negateReturningIf,
		convertIfElseAssignmentToConditionalExpression,
		combineNestedIfs,
		convertIfsToExpressionStatements,
		convertIfElseToConditionalExpression,
		negateReturningIf,
		removeRedundantElse,
		combineReturnStatements
	)(file,line);

	root.branch.scp.shortenVariables();
}

@("minify")
unittest
{
	void assertMinifier(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		got.assertTreeInternals(file,line);
		expected.analyseNode();

		got.minify(file,line);

		auto diff = diffTree(got,expected);
		got.assertTreeInternals(file,line);

		if (diff.type == Diff.No)
			return;

		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}

	//assertMinifier(
	//	`function y(e) { switch (typeof e) { case "object": var t; if (a) 1; } }`,
	//	`function y(b){switch(typeof b){case'object':var c;a&&1}}`
	//);

	//assertMinifier(
	//	`if (46 == a && 5 > b) c = 56`,
	//	`46==a&&5>b&&(c=56)`
	//);
	//assertMinifier(
	//	"function handly(event) { if (!a) { b = 4, d = 6}}",
	//	"function handly(){!a&&(b=4,d=6)}"
	//);
	assertMinifier(
		"function handly(event) { if (a) return; b = 4; d = 6;}",
		"function handly(){!a&&(b=4,d=6)}"
	);
	assertMinifier(
		"function a() { if (a) if (c) return bla; if (b) if (d) return alb; }",
		"function a() { return a && c ? bla : b && d ? alb : void 0 }"
	);

/*	see unittest in returns
	assertMinifier(
		`function cd() { if (a) { return 7; } else if (b) return 5; d(); }`,
		`function cd() { return a ? 7 : b ? 5 : (d(),void 0) }`
	);*/

		/// ditto
		assertMinifier(
			"function z(d) { if (a) b ? d() : e(); }",
			"function z(c) { a && (b ? c() : e()) }"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) g = f(); }",
			"function z() { a && (g = f()) }"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) g&&d&&(k=7),f(); }",
			"function z(b) { a && (g && b && (k = 7),f()) }"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) if (b) d(); else e() };",
			"function z(c) { a && (b ? c() : e()) }"
		);
		assertMinifier(
			`if (doFun(), a) { b = 6; } else { b = 7; }`,
			`doFun(),b = a ? 6 : 7`
		);
		/// shortenOftenUsedStringLiterals
		/*assertMinifier(
			`var abc; if ("prod" !== "dev") { var someLongName = function b() { return 5; }; }`,
			"var abc, someLongName = function b(){ return 5 };"
		);
		/// ditto
		assertMinifier(
			`function a() { var a = {"ThisStringNeedsToStay": "ThisWillBeReplaced"}, b = {"ThisStringNeedsToStay": "ThisWillBeReplaced"}; }`,
			`function a() { var c = "ThisWillBeReplaced", a = { "ThisStringNeedsToStay": c }, b = { "ThisStringNeedsToStay": c } }`
		);
		/// Issue #42
		assertMinifier(
			`var a = ("a " + propName + " b ") + ("c.")`,
			`var a = "a " + propName + " b " + "c.";`
		);
		/// shortCircuitIfStatements*/
		assertMinifier(
			"if (true) d = 5;",
			"d = 5"
		);
		/// ditto
		assertMinifier(
			"if (true) d = 5; else g = 5;",
			"d = 5"
		);
		/// ditto
		assertMinifier(
			"if (a) { if (true) { d = 5; p = 6; } else g = 5; }",
			"a && (d = 5,p = 6)"
		);
		/// ditto
		assertMinifier(
			"if (a) { if (true) { d = 5, p = 6; } else g = 5; }",
			"a && (d = 5,p = 6)"
		);
		/// ditto
		assertMinifier(
			"if (a) if (true) { d = 5; p = 6; } else g = 5;",
			"a && (d = 5,p = 6)"
		);
		/// ditto
		assertMinifier(
			"if (a) if (true) { d = 5, p = 6; } else g = 5;",
			"a && (d = 5,p = 6)"
		);
		/// ditto
		assertMinifier(
			"if (false) d = 5;",
			""
		);
		/// ditto
		assertMinifier(
			"if (false) d = 5; else g = 5;",
			"g = 5"
		);
		/// ditto
		assertMinifier(
			"if (a) if (false) g = 5; else { d = 3; p = 9; }",
			"a && (d = 3,p = 9)"
		);
		/// ditto
		assertMinifier(
			"if (a) { if (false) g = 5; else { d = 4; p = 7; } }",
			"a && (d = 4,p = 7)"
		);
		/// ditto
		assertMinifier(
			"if (!false) d = 5;",
			"d = 5"
		);
		/// ditto
		assertMinifier(
			`if ("abc" !== "def") d = 5;`,
			"d = 5"
		);
		/// ditto
		assertMinifier(
			`"abc" !== "def" ? d = 5 : g = 5;`,
			"d = 5"
		);
		/// ditto
		assertMinifier(
			`"abc" === "def" ? d = 5 : g = 5;`,
			"g = 5"
		);
		/// ditto
		assertMinifier(
			`true && false ? d = 5 : g = 5;`,
			"g = 5"
		);
		/// ditto
		assertMinifier(
			`1 && 0 ? d = 5 : g = 5;`,
			"g = 5"
		);
		/// ditto
		assertMinifier(
			`0 || 1 ? d = 5 : g = 5;`,
			"d = 5"
		);
		/// ditto
		assertMinifier(
			`1 != 2 ? d = 5 : g = 5;`,
			"d = 5"
		);
		/// ditto
		assertMinifier(
			`"abc" === "def" ? d = 5 : g = 5;`,
			"g = 5"
		);
		/// ditto
		//assertMinifier(
		//	`(0 || 1) && (9 == 9) ? d = 5 : g = 5;`,
		//	"d = 5"
		//);
		/// Issue #67
		assertMinifier(
			`if ("use strict", b.can() && top === bottom) doThing();`,
			`"use strict",b.can() && top === bottom && doThing()`
		);
		/// Issue #67
		assertMinifier(
			`if (b.can() && top === bottom && gogo(), true) doThing();`,
			"b.can() && top === bottom && gogo(), doThing()"
		);
		/// Issue #67
		assertMinifier(
			`if (b.can() && top === bottom && gogo(), false) doThing();`,
			"b.can() && top === bottom && gogo()"
		);
		/// Issue #67
		assertMinifier(
			`if (bla(), 6 ? true : false) doThing();`,
			"bla(), doThing()"
		);
		/// Issue #67
		assertMinifier(
			`if (bla(), 0 ? true : false) doThing();`,
			"bla()"
		);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if ("use strict", b.can() && top === bottom) doThing();`,
		//	`a && ("use strict",b.can() && top === bottom) && doThing()`
		//);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if (b.can() && top === bottom && gogo(), true) doThing();`,
		//	"a && (b.can() && top === bottom && gogo(),doThing())"
		//);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if (b.can() && top === bottom && gogo(), false) doThing();`,
		//	"a && b.can() && top === bottom && gogo()"
		//);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if (bla(), 6 ? true : false) doThing();`,
		//	"a && (bla(),doThing())"
		//);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if (bla(), 0 ? true : false) doThing();`,
		//	"a && bla()"
		//);
		/// Issue #67
		//assertMinifier(
		//	`if (a) if (bla(), poi ? kol() : apc()) doThing();`,
		//	"a && (bla(),poi ? kol() : apc()) && doThing()"
		//);
		/// combineExpressionStatementsWithNonEmptyReturnStatements
		assertMinifier(
			"function d(a) { if (a) { if (b) { b = 3; return p; } } }",
			"function d(a) { if (a && b) return b = 3, p }"
		);
		/// ditto
		assertMinifier(
			"function d(a) { if (a) { if (b) { e(); return p; } } }",
			"function d(a) { if (a && b) return e(), p }"
		);
		/// ditto
		assertMinifier(
			"function d(a) { if (a) { if (b) { if (k) e(); return p; } } }",
			"function d(a) { if (a && b) return k && e(), p }"
		);
		/// convertIfsToExpressionStatementsOrConditionals
		assertMinifier(
			"if (a) d = 5;",
			"a && (d = 5)"
		);
		assertMinifier(
			"if (a) d = 5; if (b) g = 5;",
			"a && (d = 5), b && (g = 5)"
		);
		assertMinifier(
			"if (a) {d = 5; e = 5; if (b) g = 5;}",
			"a && (d = 5,e = 5,b && (g = 5))"
		);
		assertMinifier(
			`if (g) { a=5; if (e) d = 5; b = 5;}`,
			"g && (a = 5,e && (d = 5),b = 5)"
		);
		assertMinifier(
			`if (g) { (a = 5,e) && (d = 5); b = 5 }`,
			"g && (a = 5,e && (d = 5),b = 5)"
		);
		assertMinifier(
			`if (g) { if (a=5, e) d = 5; b = 5;}`,
			"g && (a = 5,e && (d = 5),b = 5)"
		);
		/// moveNonValueGeneratingStatementsIntoForIfAndSwitches
		assertMinifier(
			"a = 5; if (d) g = 5;",
			"a = 5, d && (g = 5)"
		);
		/// ditto
		assertMinifier(
			"e(), a = 5; if (d) g = 5;",
			"e(),a = 5, d && (g = 5)"
		);
		/// ditto
		assertMinifier(
			"for (a in k) { d = 5; if (g) k = 3; }",
			"for(a in k) d = 5, g && (k = 3);"
		);
		/// combineNestedIfs
		assertMinifier(
			"function a() { if (a) if (c && d) return bla; }",
			"function a() { if (a && c && d) return bla; }"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a) { if (c) { return bla; } } }",
			"function a() { if (a && c) return bla }"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a || b) { if (c) { return bla; } } }",
			"function a() { if ((a || b) && c) return bla }"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a) { if (b || c) { return bla; } } }",
			"function a() { if (a && (b || c)) return bla }"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a || b) { if (c || d) { return bla; } } }",
			"function a() { if ((a || b) && (c || d)) return bla }"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a && b) { if (c || d) { return bla; } } }",
			"function a() { if (a && b && (c || d)) return bla }"
		);
		/// ditto
		assertMinifier(
			`if (g) if (a=5, e) d = 5;`,
			"g && (a = 5,e && (d = 5))"
		);
		assertMinifier(
			`(t = 7,g && (a = 5,e)) && (d = 5)`,
			"t = 7,g && (a = 5,e) && (d = 5)"
		);
		/// ditto
		assertMinifier(
			`if (t = 7, g) if (a=5, e) d = 5;`,
			"t = 7,g && (a = 5,e && (d = 5))"
		);
		/// ditto
		assertMinifier(
			`if (g || t) if (a=5, e) d = 5;`,
			"(g || t) && (a = 5,e && (d = 5))"
		);
		/// ditto
		assertMinifier(
			`if ((a)) if ((b)) d = 5;`,
			"a && b && (d = 5)"
		);
		/// ditto
		assertMinifier(
			`if (!a) if (!b) d = 5;`,
			"!a && !b && (d = 5)"
		);
		/// ditto
		assertMinifier(
			`if (a=5) if (b) d = 5;`,
			"(a = 5) && b && (d = 5)"
		);

		/// optimizeReturningIfStatement
		// Note: Need to test that we don't apply this optimisation if the `if (c) return;` is nested in a if-statement
		assertMinifier(
			`function a() { if (a) return; a = 5; }`,
			`function a() { !a && (a = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; }`,
			`function a() { b }`
		);
		/// ditto
		assertMinifier(
			"function d() { if (!a) { return; }; }",
			"function d() { !a }"
		);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return; a = 5; if (b) return; a = 7; }`,
		//	`function a() { !a && (a = 5,!b && (a = 7)) }`
		//);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } e = 5; }`,
			`function a() { b ? d = 5 : e = 5 }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (e) if (b) if (c) return; d = 5; }`,
			`function a() { !(e && b && c) && (d = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; else d = 4; }`,
			`function a() { !b && (d = 4) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; else d = 4; f = 5;}`,
			`function a() { !b && (d = 4,f = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { return; } else { d = 4; } f = 5;}`,
			`function a() { !b && (d = 4,f = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { g = 6; return; } else { d = 4; } f = 5;}`,
			`function a() { b ? g = 6 : (d = 4,f = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } }`,
			`function a() { b && (d = 5) }`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } else d = 4; f = 5;}`,
			`function a() { b ? d = 5 : (d = 4,f = 5) }`
		);
		/// ditto
		/*assertMinifier(
			`function a() { for (var k in keys) { if (b) { return; } else d = 4; f = 5; } g = 66; }`,
			`function a() { var k; for(k in keys){ if (b) { return } else  d = 4; f = 5 }; g = 66 }`
		);*/
		/// ditto
		assertMinifier(
			`function b(z) { if (z.p !== 'value') return; if (z.q === k) return; k = z.v; }`,
			"function b(a) { !('value' != a.p) && !(a.q === k) && (k = a.v) }"
		);
		/// ditto
		assertMinifier(
			`function b(z) { if (z.p !== 'value') { return; } if (z.q === k) { return; } k = z.v; }`,
			"function b(a) { !('value' != a.p) && !(a.q === k) && (k = a.v) }"
		);
		/// ditto
		assertMinifier(
			`function b() { switch (bla) { case 5: return; }; op() }`,
			`function b() { switch (bla) { case 5: return; }; op() }`
		);
		/// ditto
		assertMinifier(
			`function b() { if (a) for(;;)return; op() }`,
			`function b() { if (a) for(;;)return; op() }`
		);
		/// ditto
		assertMinifier(
			`function b() { if (a) { for(;;)return; } op() }`,
			`function b() { if (a) for(;;)return; op() }`
		);
		/// removeStatementBlockFromSwitchCaseClauses
		//assertMinifier(
		//	`switch (a) { case 7: { e(); } };`,
		//	`switch (a) { case 7: e(); }`
		//);
		///// ditto
		//assertMinifier(
		//	`switch (a) { case 7: { e(); } { f(); }};`,
		//	`switch (a) { case 7: e(); f(); }`
		//);
		/// putRemainingStatementsInElseBlockWhenIfReturns
		assertMinifier(
			`function a() { if (a) return 5; d = 6; }`,
			`function a() { if (a) return 5; d = 6 }`
		);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else d = 6; return 4; }`,
		//	`function a() { return a ? 5 : (d = 6,4) }`
		//);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else if (c) d = 6; return 4; }`,
		//	`function a() { return a ? 5 : (c && (d = 6),4) }`
		//);
		///// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else if (c) { d = 6; return 4; } return 4; }`,
		//	`function a() { return a ? 5 : c ? (d = 6,4) : 4 }`
		//);
		/// removeEmptyStatementsFromStatementLists
		//assertMinifier(
		//	";;var a = 4;;;",
		//	"var a = 4;"
		//);
		///// ditto
		//assertMinifier(
		//	"var a = 4;;;if (b);;;",
		//	"var a = 4; b"
		//);
		/// shortenOftenUsedStringLiterals
		//assertMinifier(
		//	`var a; function bla() { a = "str"; var c = "doit!"; function gek() { var b = 6; if (c == "doit!") return 17; }} a = "str";`,
		//	`function bla() { function gek() { var b = 6; if (c == d) return 17; }; var d = "doit!", c = d; a = "str" }; var a; a = "str"`
		//);
		/// negateContinueStatements
/*		assertMinifier(
			`for (var k in keys) { if (b) continue; else doSomething(); }`,
			`var k; for(k in keys){ !b && doSomething() }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) continue; else { doSomething(); } d = 5;}`,
			`var k; for(k in keys){ !b && (doSomething(),d = 5) }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) continue; else doSomething(); d = 5;}`,
			`var k; for(k in keys){ !b && (doSomething(),d = 5) }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) continue; doSomething(); }`,
			`var k; for(k in keys){ !b && doSomething() }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {continue}; else doSomething(); }`,
			`var k; for(k in keys){ !b && doSomething() }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {continue}; else { doSomething(); } d = 5;}`,
			`var k; for(k in keys){ !b && (doSomething(),d = 5) }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {continue}; else doSomething(); d = 5;}`,
			`var k; for(k in keys){ !b && (doSomething(),d = 5) }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {continue}; doSomething(); }`,
			`var k; for(k in keys){ !b && doSomething() }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {d = 5; continue; } else { doSomething(); } b = 5; }`,
			`var k; for(k in keys){ b ? d = 5 : (doSomething(),b = 5) }`
		);
		/// ditto
		assertMinifier(
			`for (var k in keys) { if (b) {d = 5; continue; } doSomething(); b = 5; }`,
			`var k; for(k in keys){ b ? d = 5 : (doSomething(),b = 5) }`
		);
		/// ditto
		assertMinifier(
			"for (var a in b) { if (!a) { continue; } else { d = 5; }};",
			"var a; for(a in b){ a && (d = 5) }"
		);
		/// ditto
		assertMinifier(
			"for (var a in b) { if (!a) { continue; } d = 5; };",
			"var a; for(a in b){ a && (d = 5) }"
		);*/
		/// simplifyEmptyIfStatementsScp
	/*assertMinifier(
		"if (a) { };",
		"a"
	);
	/// ditto
	assertMinifier(
		"if (a) { } else { d = 5 }",
		"!a && (d = 5)"
	);
	/// ditto
	assertMinifier(
		"for (var a in b) { if (!a) { continue; } };",
		"var a; for(a in b){ !a }"
	);*/
	/// reduceBinaryExpressions
	assertMinifier(
		`var a = "a" + "string" + "that" + "can" + "be" + "concatenated" +
		"even" + "across" + "lines";`,
		`var a = "astringthatcanbeconcatenatedevenacrosslines";`
	);
	/// ditto
	assertMinifier(
		`var a = 5 ^ 7, b = 99 * 12, c = "asdf" + 55 * 9;`,
		`var a = 2, b = 1188, c = "asdf495";`
	);
	/// ditto
	assertMinifier(
		`var a = 5 ^ 7 + unknown(), b = 99 * 12 - unknown, c = "asdf" + 55 * 9 + really.unknown;`,
		`var a = 5 ^ 7 + unknown(), b = 99 * 12 - unknown, c = "asdf" + 55 * 9 + really.unknown;`
	);
	/// ditto
	assertMinifier(
		`if (6 < 7 && 6 * 11 != 0) hallo(); else youWontSeeMe();`,
		`hallo()`
	);
	/// ditto
	assertMinifier(
		`if (5 + 6 + "abc" == "11abc") hallo(); else youWontSeeMe();`,
		`hallo()`
	);
	/// hexOptimisation
	//assertMinifier(
	//	`a = 0x4456;`,
	//	`a = 17494`
	//);
	assertMinifier(
		`var t=arguments.length>1&&void 0!==arguments[1]?arguments[1]:.15;`,
		`var t=arguments.length>1&&void 0!==arguments[1]?arguments[1]:.15;`
	);
	assertMinifier(
		`function a(b, c) { if(c) if (c===b) return "b" else return null; switch (b) { case 7: if (b) return null else return c.data } return 4; }`,
		`function a(a,b){if(b)return b===a?'b':null;switch(a){case 7:return a?null:b.data}return 4}`
	);
	/// Issue #58
	//assertMinifier(
	//	`function abc() { for (var propKey in props) { k = props[propKey]; } if (b) { return ret + '>'; } return ret + ' ' + markupForID + '>'; }`,
	//	"function abc() { var propKey; for(propKey in props){ k = props[ propKey ] }; return b ? ret + '>' : ret + ' ' + markupForID + '>' }"
	//);
	///// ditto
	//assertMinifier(
	//	`function abc() { for (var propKey in props) { k = props[propKey]; } if (b) { return ret + '>'; } if (c) return ret + ' ' + markupForID + '>'; }`,
	//	`function abc() { var propKey; for(propKey in props){ k = props[ propKey ] }; return b ? ret + '>' : c ? ret + ' ' + markupForID + '>' : void 0 }`
	//);
	///// ditto
	//assertMinifier(
	//	`function abc() { for (var propKey in props) { k = props[propKey]; } if (b) { return ret + '>'; } for (var i in p) k = p[i]; if (c) return ret + ' ' + markupForID + '>'; if (k) return 77; }`,
	//	`function abc() { var propKey, i; for(propKey in props){ k = props[ propKey ] }; if (b) { return ret + '>' } else { for(i in p)k = p[ i ]; return c ? ret + ' ' + markupForID + '>' : k ? 77 : void 0 } }`
	//);
	//assertMinifier(
	//	`function t(){c.width=w=1920;π=Math.PI/2;λ=0;r=w/4;for(φ=-π;φ<π;φ+=1/r){λ+=.1;x.lineTo(C(φ)*S(λ-t)*r+w/2,(C(t)*S(φ)-S(t)*C(φ)*C(λ-t))*r+r)}x.stroke()}`,
	//	`function t(){c.width=w=1920;π=Math.PI/2;λ=0;r=w/4;for(φ=-π;φ<π;φ+=1/r){λ+=.1;x.lineTo(C(φ)*S(λ-t)*r+w/2,(C(t)*S(φ)-S(t)*C(φ)*C(λ-t))*r+r)}x.stroke()}`
	//);
	assertMinifier(
		`(function b(a,b,c){return 7})({1:[function(_dereq_,module,exports){ somethingSomething() }]})`,
		`(function b(){ return 7 })({ 1: [ function (){ somethingSomething() } ] })`
	);
	assertMinifier(
		`if (a) {  } else {  }`,
		`if (a) {  } else {  }`
	);
	assertMinifier(
		`function bla( ch, asCodePoint ) { if ( asCodePoint ) { if ( ch === "\0" ) { return "\uFFFD"; } return ch.slice( 0, -1 ) + "\\" + ch.charCodeAt( ch.length - 1 ).toString( 16 ) + " "; } return "\\" + ch; }`,
		`function bla(a,b){return b?'\0'==a?'\uFFFD':a.slice(0,-1)+'\\'+a.charCodeAt(a.length-1).toString(16)+' ':'\\'+a}`
	);
	assertMinifier(
		`try{ b = 9; } catch(exception) { c = 5; } finally { k = 6 }`,
		`try{b=9}catch(exception){c=5}finally{k=6}`
	);

	assertMinifier(
		`g; if (h) b; d; if (f) a;`,
		`g,h&&b,d,f&&a;`
	);
	assertMinifier(
		`function k() { if (d) { if (c) return; return a; } else { return b; } }`,
		`function k() { return d ? c ? void 0 : a : b }`
	);
}



