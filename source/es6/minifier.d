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
import es6.bench;
import es6.charfreq;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import std.stdio;
}

enum MinifyFlags {
	SmallRaw,
	SmallGZip
}

void minify(Node root, in string file = __FILE__, in size_t line = __LINE__)
{
	debug {
		root.assertTreeInternals();
	}
	// For some unknown reason, this increases the size of the file after gzipping
	//measure!("Factor out common strings",(){
	//	foreach(s; root.branch.scp.children)
	//		factorOutCommonStrings(s);
	//});
	//
	 //we are going through the transforms one-by-one to see their impact on the gzip size

	 //removeUnusedFunctions removes also functions that are assigned to a variable and have a name, but is only called through the var

	measure!("Transform", (){
		root.runTransform!(
			//simplifyVoid0Conditionals,	// does well on almost all files, except very bad on migration-reporter.max (worth looking into). Also we might want to consider transforming into if statement instead of a conditional
			simplifyComparisions,
			simplifyArithmeticOperations,
			convertToScientificNotation,		// some files get 1-2 bytes bigger
			simplifyLogicalOperations,
			minifyLabels,
			simplifyArrayIndexNode,
			rewriteReturnUndefined,
			removeUnusedReturn,
			inlineVariables,	// tests/inferno-1.2.2.js and tests/acorn-4.0.4.js and tests/react-0.13.3.js get a little bigger (7-17 chars)
			reuseVariable,	// tests/jquery-3.1.1.js and tests/angular-1.6.1.js grow a 3-6 bytes, otherwise good gains 20-300 bytes
			convertUndefinedToVoid0,
			convertInfinityTo1div0,
			negateIfContinue,
			removeFunctionExpressionUnusedName,
			invertBinaryExpressions,	// tests/RxJS/dist/rx.all.js actually grows in size
			convertHexToDecimalLiterals,	// tests/jquery/dist/jquery.js gzip grows by 4 byte
			removeRedundantUseStrict,
			//mergeDuplicateVariableDeclarations, 	// makes things bigger in general (and is REALLY SLOW!!!!!!!!!!!) (only tests/migration-reporter-max.js benefits)
			//convertWhileToForLoop,	// makes things bigger in almost all cases (except small files)
			//moveVariableDeclarationsIntoForLoops,	// makes things bigger in general
			mergeNeighbouringVariableDeclarationStatements,
			//convertEscapedUnicodeToUnicode,	// makes gzip bigger
			//mergeVariableDeclarationStatements,	// makes gzip bigger
			//moveLiteralComparisonToLeftOperand,	// makes most files bigger
			//hoistFunctions,	// makes gzip bigger in general (0.5-1% difference)
			shortenLiteralPropertyNames,
			combineBlockStatementIntoExpression,	// makes only tests/underscore-1.8.3.js 2 bytes bigger with gzip
			removeRedundantBlockStatements,
			combineFunctionBodyIntoExpression,	// makes some files with gzip bigger (some files are worthwhile though)
			combineModuleIntoExpression,	// no impact !?!?
			//simplifyBinaryExpressions,	// redudant now that we have simplifyComparisions and simplifyLogicalOperations
			simplifyStaticIfStatement,
			simplifyStaticConditional,
			swapNegatedConditionals,	// in general good but sometimes in gzip makes it a byte bigger
			removeUnnecessaryParenthesis,
			//moveExpressionsIntoForLoops,	// makes gzip bigger (some files a few bytes smaller)
			removeUnusedParameters,	// only tests/react-0.13.3.js gets 80 bytes bigger
			shortenBooleanNodes,
			simplifyRedundantAssignmentExpressions,
			//moveExpressionsIntoIfCond,	// makes gzip bigger
			//moveExpressionsIntoReturn,	// makes gzip bigger
			//negateReturningIf,	// some files get bigger, most smaller (only 5-30 bytes)
			convertIfElseAssignmentToConditionalExpression,
			//combineNestedIfs,		// in gzip some up, some down (but only 3-8 bytes)
			//convertIfsToExpressionStatements,	// makes bigger in gzip
			convertIfElseToConditionalExpression,
			removeRedundantElse,
			removeUnusedFunctions,
			combineReturnStatements  // some files get a 4-8 bytes bigger, but other files get more smaller
		)(file,line);
	});

	auto freq = root.charfreq();

	measure!("Shorten Variables",(){root.branch.scp.shortenVariables(freq);});
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
		//got.assertAllNodesVisited(file,line);

		auto diff = diffTree(got,expected);
		got.assertTreeInternals(file,line);

		if (diff.type == Diff.No)
			return;

		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}

	assertMinifier(
		`function r(e) { for (var t = 5, s = 0;;) { var c = 3; var u = 4, l = 5; } for (var f = [], d = [];;) { var v = r[p]; v.data = 5; } }`,
		`function r(a){for(var c=5,v=0;;){var t=3;var f=4,o=5}for(var c=[],v=[];;){t=r[p];t.data=5}}`
	);

	assertMinifier(
		`function k(w) { if (p) { var dom$1 = 5; } var tag = w.a; var flags = w.b; k(flags); h(tag, isSVG); } k();`,
		`function k(a){if(p)var o=5;o=a.a;var w=a.b;k(w);h(o,isSVG)}k();`
	);
	assertMinifier(
		`function c(a) {
    for (;;) {
        if (b) {
            continue;
        }
        u = 0;
    }
}c();`,
`function c(a){for(;;){if(!b){u=0}}}`
	);
	assertMinifier(
		`function p() {
    if (d) {
    }
    outer: while (true) {
    }
}`,
		``
	);
	assertMinifier(
		`function b() { var type; if (l) { var k = 4; if (o) { for (var i = 0;;) { l(); } } } } b();`,
		`function b(){var c,a,b;if(l&&(a=4,o))for(b=0;;)l()}b();`
	);
	assertMinifier(
		`function b(p) { k = 3; var id = 4; a('=JSONP_CALLBACK&','=JSONP_CALLBACK&'); } b();`,
		`function b(){var b='=JSONP_CALLBACK&',c;k=3,c=4,a(b,b)}b();`
	);
	assertMinifier(
		`function k(){ for (var key in b) a(key); }; k();`,
		`function k(){ for (var c in b) a(c); }; k();`
	);
	assertMinifier(
		`function b(){ for (var key of b) a(key); }; b();`,
		`function b(){ for (var c of b) a(c); }; b();`
	);
	assertMinifier(
		`function b(){ for (const key in b) a(key); }; b();`,
		`function b(){ for (const c in b) a(c); }; b();`
	);
	assertMinifier(
		`function b(){ for (const key of b) a(key); }; b();`,
		`function b(){ for (const c of b) a(c); }; b();`
	);
	assertMinifier(
		`function b(){ for (let key in b) a(key); }; b();`,
		`function b(){ for (let c in b) a(c); }; b();`
	);
	assertMinifier(
		`function b(){ for (let key of b) a(key); }; b();`,
		`function b(){ for (let c of b) a(c); }; b();`
	);

	assertMinifier(
		`function b(b) { while (a) { b(); } return true; } b();`,
		`function b(b) { for (;a;) b(); return !0 } b();`
	);

	assertMinifier(
		`function a(obj) { var keys = _.keys(obj); var length = keys.length; var values = Array(length); for (var i = 0; i < length; i++) { values[i] = obj[keys[i]]; } return values; };a();`,
  		`function a(b) { for (var c=_.keys(b), d=c.length, e=Array(d), a=0; a<d; a++) e[a] = b[c[a]]; return e } a();`
  	);

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
		"function handly(event) { if (a) return; b = 4; d = 6;} handly();",
		"function handly(){a||(b=4,d=6)} handly();"
	);

/*	see unittest in returns
	assertMinifier(
		`function cd() { if (a) { return 7; } else if (b) return 5; d(); }`,
		`function cd() { return a ? 7 : b ? 5 : (d(),void 0) }`
	);*/

		/// ditto
		assertMinifier(
			"function z(d) { if (a) b ? d() : e(); } z();",
			"function z(c) { a && (b ? c() : e()) } z();"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) g = f(); } z();",
			"function z() { a && (g = f()) } z();"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) g&&d&&(k=7),f(); } z();",
			"function z(b) { a && (g && b && (k = 7),f()) } z();"
		);
		/// ditto
		assertMinifier(
			"function z(d) { if (a) if (b) d(); else e() } z();;",
			"function z(c) { a && (b ? c() : e()) } z();"
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
			"function d(a) { if (a) { if (b) { b = 3; return p; } } } d();",
			"function d(a) { if (a && b) return b = 3, p } d();"
		);
		/// ditto
		assertMinifier(
			"function d(a) { if (a) { if (b) { e(); return p; } } } d();",
			"function d(a) { if (a && b) return e(), p } d();"
		);
		/// ditto
		assertMinifier(
			"function d(a) { if (a) { if (b) { if (k) e(); return p; } } } d();",
			"function d(a) { if (a && b) return k && e(), p } d();"
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
			"function a() { if (a) if (c && d) return bla; } a();",
			"function a() { if (a && c && d) return bla; } a();"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a) { if (c) { return bla; } } } a();",
			"function a() { if (a && c) return bla } a();"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a || b) { if (c) { return bla; } } } a();",
			"function a() { if ((a || b) && c) return bla } a();"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a) { if (b || c) { return bla; } } } a();",
			"function a() { if (a && (b || c)) return bla } a();"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a || b) { if (c || d) { return bla; } } } a();",
			"function a() { if ((a || b) && (c || d)) return bla } a();"
		);
		/// ditto
		assertMinifier(
			"function a() { if (a && b) { if (c || d) { return bla; } } } a();",
			"function a() { if (a && b && (c || d)) return bla } a();"
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
			"a || b || (d = 5)"
		);
		/// ditto
		assertMinifier(
			`if (a=5) if (b) d = 5;`,
			"(a = 5) && b && (d = 5)"
		);

		/// optimizeReturningIfStatement
		// Note: Need to test that we don't apply this optimisation if the `if (c) return;` is nested in a if-statement
		assertMinifier(
			`function a() { if (a) return; a = 5; } a();`,
			`function a() { a || (a = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; } a();`,
			`function a() { b } a();`
		);
		/// ditto
		assertMinifier(
			"function d() { if (!a) { return; }; } d();",
			"function d() { !a } d();"
		);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return; a = 5; if (b) return; a = 7; } a();`,
		//	`function a() { !a && (a = 5,!b && (a = 7)) } a();`
		//);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } e = 5; } a();`,
			`function a() { b ? d = 5 : e = 5 } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (e) if (b) if (c) return; d = 5; } a();`,
			`function a() { e && b && c || (d = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; else d = 4; } a();`,
			`function a() { b || (d = 4) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) return; else d = 4; f = 5; } a();`,
			`function a() { b || (d = 4,f = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { return; } else { d = 4; } f = 5; } a();`,
			`function a() { b || (d = 4,f = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { g = 6; return; } else { d = 4; } f = 5; } a();`,
			`function a() { b ? g = 6 : (d = 4,f = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } } a();`,
			`function a() { b && (d = 5) } a();`
		);
		/// ditto
		assertMinifier(
			`function a() { if (b) { d = 5; return; } else d = 4; f = 5; } a();`,
			`function a() { b ? d = 5 : (d = 4,f = 5) } a();`
		);
		/// ditto
		/*assertMinifier(
			`function a() { for (var k in keys) { if (b) { return; } else d = 4; f = 5; } g = 66; } a();`,
			`function a() { var k; for(k in keys){ if (b) { return } else  d = 4; f = 5 }; g = 66 } a();`
		);*/
		/// ditto
		assertMinifier(
			`function b(z) { if (z.p !== 'value') return; if (z.q === k) return; k = z.v; } b();`,
			"function b(a) { 'value' != a.p || a.q === k || (k = a.v) } b();"
		);
		/// ditto
		assertMinifier(
			`function b(z) { if (z.p !== 'value') { return; } if (z.q === k) { return; } k = z.v; } b();`,
			"function b(a) { 'value' != a.p || a.q === k || (k = a.v) } b();"
		);
		/// ditto
		assertMinifier(
			`function b() { switch (bla) { case 5: return; }; op() } b();`,
			`function b() { switch (bla) { case 5: return; }; op() } b();`
		);
		/// ditto
		assertMinifier(
			`function b() { if (a) for(;;)return; op() } b();`,
			`function b() { if (a) for(;;)return; op() } b();`
		);
		/// ditto
		assertMinifier(
			`function b() { if (a) { for(;;)return; } op() } b();`,
			`function b() { if (a) for(;;)return; op() } b();`
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
			`function a() { if (a) return 5; d = 6; } a();`,
			`function a() { if (a) return 5; d = 6 } a();`
		);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else d = 6; return 4; } a();`,
		//	`function a() { return a ? 5 : (d = 6,4) } a();`
		//);
		/// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else if (c) d = 6; return 4; } a();`,
		//	`function a() { return a ? 5 : (c && (d = 6),4) } a();`
		//);
		///// ditto
		//assertMinifier(
		//	`function a() { if (a) return 5; else if (c) { d = 6; return 4; } return 4; } a();`,
		//	`function a() { return a ? 5 : c ? (d = 6,4) : 4 } a();`
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
		`function a(b, c) { if(c) if (c===b) return "b" else return null; switch (b) { case 7: if (b) return null else return c.data } return 4; } a();`,
		`function a(a,b){if(b)return b===a?'b':null;switch(a){case 7:return a?null:b.data}return 4}a();`
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
	assertMinifier(
		`function t(){c.width=w=1920;π=Math.PI/2;λ=0;r=w/4;for(φ=-π;φ<π;φ+=1/r){λ+=.1;x.lineTo(C(φ)*S(λ-t)*r+w/2,(C(t)*S(φ)-S(t)*C(φ)*C(λ-t))*r+r)}x.stroke()} t();`,
		`function t(){for(c.width=w=1920,π=Math.PI/2,λ=0,r=w/4,φ=-π;φ<π;φ+=1/r)λ+=.1,x.lineTo(C(φ)*S(λ-t)*r+w/2,(C(t)*S(φ)-S(t)*C(φ)*C(λ-t))*r+r);x.stroke()}t();`
	);
	assertMinifier(
		`(function b(a,b,c){return 7})({1:[function(_dereq_,module,exports){ somethingSomething() }]})`,
		`(function(){ return 7 })({ 1: [ function (){ somethingSomething() } ] })`
	);
	assertMinifier(
		`if (a) {  } else {  }`,
		`if (a) {  } else {  }`
	);
	assertMinifier(
		`function bla( ch, asCodePoint ) { if ( asCodePoint ) { if ( ch === "\0" ) { return "\uFFFD"; } return ch.slice( 0, -1 ) + "\\" + ch.charCodeAt( ch.length - 1 ).toString( 16 ) + " "; } return "\\" + ch; } bla();`,
		`function bla(a,b){return b?'\0'==a?'�':a.slice(0,-1)+'\\'+a.charCodeAt(a.length-1).toString(16)+' ':'\\'+a}bla();`
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
		`function k() { if (d) { if (c) return; return a; } else { return b; } } k();`,
		`function k() { return d ? c ? void 0 : a : b } k();`
	);
	assertMinifier(
		`function fun(){ switch(a){ case 5: var node = getNode(); node.something(); case 6: var node = getNode(); node.somethingElse();} } fun();`,
		`function fun(){switch(a){case 5:var b=getNode();b.something();case 6:b=getNode();b.somethingElse()}} fun();`
	);
	assertMinifier(
		`function finishNodeAt(node, type, pos, loc) { node.type = type; if (this.options.locations) node.loc.end = loc; if (this.options.ranges) node.range[1] = pos; return node } finishNodeAt();`,
		`function finishNodeAt(a,b,c,d){return a.type=b,this.options.locations&&(a.loc.end=d),this.options.ranges&&(a.range[1]=c),a} finishNodeAt();`
	);
	assertMinifier(
		`function bla() { if (!declaration) this.unexpected(); return this.parseClass(node, true) } bla();`,
		`function bla(){return declaration||this.unexpected(),this.parseClass(node,!0)} bla();`
	);
	assertMinifier(
		`function bls(node) { if (a) { this.checkPatternErrors(refDestructuringErrors, true); this.toAssignable(init); this.checkLVal(init); return this.parseForIn(node, init) } else { this.checkExpressionErrors(refDestructuringErrors, true) } return this.parseFor(node, init) } bls();`,
		`function bls(b){return a?(this.checkPatternErrors(refDestructuringErrors,!0),this.toAssignable(init),this.checkLVal(init),this.parseForIn(b,init)):(this.checkExpressionErrors(refDestructuringErrors,!0),this.parseFor(b,init))} bls();`
	);
	assertMinifier(
		`function bla(noIn, refDestructuringErrors) { var this$1 = this; var startPos = this.start, startLoc = this.startLoc; var expr = this.parseMaybeAssign(noIn, refDestructuringErrors) if (this.type === tt.comma) { var node = this.startNodeAt(startPos, startLoc); node.expressions = [expr]; while (this.eat(tt.comma)) node.expressions.push(this$1.parseMaybeAssign(noIn, refDestructuringErrors)); return this.finishNode(node, "SequenceExpression"); } return expr } bla();`,
		`function bla(b,c){var e=this,f=this.start,g=this.startLoc,d=this.parseMaybeAssign(b,c),a;if(this.type===tt.comma){for(a=this.startNodeAt(f,g),a.expressions=[d];this.eat(tt.comma);)a.expressions.push(e.parseMaybeAssign(b,c));return this.finishNode(a,'SequenceExpression')}return d}bla();`
	);
	assertMinifier(
		`var a; if (b) { var c = 5; doBla(); }`,
		`var a,c;b&&(c=5,doBla());`
	);
	assertMinifier(
		`function bk(z) { if (n) { e(); } else if (l) { if (z) { var w = h; a(); } else { b(); } } else if (k) { var q = i; c(); } return z; } bk();`,
		`function bk(d){if(n)e();else if(l)if(d){var g=h,f;a()}else b();else k&&(f=i,c());return d} bk();`
	);
	assertMinifier(
		`for (var i;;) { var c = 5; b(); }`,
		`for (var i,c;;) c = 5, b();`
	);
	assertMinifier(
		`function a() { if (a) { c = 4; if (b) return 5; return 6; } return 7; } a();`,
		`function a(){return a?(c=4,b)?5:6:7} a();`
	);
	assertMinifier(
		`if (d) { if (!a && (b || c) && d) k(); f = 6; }`,
		`d&&(a||(b||c)&&d&&k(),f=6);`
	);
	assertMinifier(
		`if (d) e() else if (!h || j === 1) b(); else if (q) s(); else k = 5;`,
		`d?e():!h||j===1?b():q?s():k=5;` // TODO: can be shorter by rewriting conditional expressions into h&&j!==1?q?...:b() (thus swapping conditional branches)
	);
	assertMinifier(
		`function bas() { var l, k; if (q) { return function(obj) { var key = keys[i]; var desc = Object.d(obj, key, key,key); obj = es5.w(obj); }; } else { var hasProp = {}; return function(obj) { }; } } bas();`,
		`function bas(){var b,c,a;return q?function(a){var b=keys[i],c=Object.d(a,b,b,b);a=es5.w(a)}:(a={},function(){})} bas();`
	);
	assertMinifier(
		`var a = 9; export function parse(input, options) {};`,
		`export function parse() {}; var a = 9;`
	);
	assertMinifier(
		`var a = 9; export default function () {};`,
		`export default function () {}; var a = 9;`
	);
	assertMinifier(
		"noop\nnew ReaddirReq(path, cb)",
		`noop, new ReaddirReq(path, cb);`
	);
	assertMinifier(
		`function abc(keysFunc, undefinedOnly) { return function(obj) { var length = arguments.length; if (length < 2 || obj == null) return obj; for (var index = 1; index < length; index++) { var source = arguments[index], keys = keysFunc(source), l = keys.length; for (var i = 0; i < l; i++) { var key = keys[i]; if (!undefinedOnly || obj[key] === void 0) obj[key] = source[key]; } } return obj; }; }; abc();`,
		`function abc(h, i) { return function(a) { var e = arguments.length, b, c; if (e < 2 || a == null) return a; for (b = 1; b < e; b++) for (var f = arguments[b], g = h(f), j = g.length, d = 0; d < j; d++) c = g[d], (!i || a[c] === void 0) && (a[c] = f[c]); return a } } abc();`
	);
	assertMinifier(
		`function a(){}; if (false) a();`,
		``
	);
	assertMinifier(
		`var a = {'\u0100': 'A',  '\u0102': 'A', '\u0104': 'A'};`,
		`var a = {Ā: 'A', Ă: 'A', Ą: 'A'};`
	);
	assertMinifier(
		`var a = {"123":123, "": 65, "123b": 77, "abc": "abc", "null": null, "Ä": 5, "a€": 6, "€": 9 };`,
		`var a = { 123: 123, "": 65, "123b": 77, abc: "abc", null: null, Ä: 5, "a€": 6, "€": 9 };`
	);
	assertMinifier(
		`function a(){ var undefined; b = undefined; } a();`,
		`function a(){ var a; b = a; } a();`
	);
	assertMinifier(
		`var ErrorHandler = (function () { function ErrorHandler() { this.errors = []; this.tolerant = false; } ; ErrorHandler.prototype.recordError = function (error) { this.errors.push(error); }; ; ErrorHandler.prototype.tolerate = function (error) { if (this.tolerant) { this.recordError(error); } else { throw error; } }; ; return ErrorHandler; });;;`,
		`var ErrorHandler=(function(){function a(){this.errors=[],this.tolerant=!1}return a.prototype.recordError=function(a){this.errors.push(a)},a.prototype.tolerate=function(a){if(this.tolerant)this.recordError(a);else throw a},a});`
	);
	assertMinifier(
		`var a = 6; while(b);`,
		`for (var a = 6;b;);`
	);
	assertMinifier(
		`for (var a = 5;;); for(var a = 6;;);`,
		`for (var a = 5;;); for(a = 6;;);`
	);
	assertMinifier(
		"function a() { if (a) if (c) return bla; if (b) if (d) return alb; } a();",
		"function a() { return a && c ? bla : b && d ? alb : void 0 } a();"
	);
	assertMinifier(
		`function a(b) { k = 6; if (b instanceof a) return b; if (!(this instanceof a)) return new a(b); this._wrapped = b; b = 5, e = 6; p = 8;} a();`,
		`function a(b){return k=6,b instanceof a?b:(this instanceof a)?void(this._wrapped=b,b=5,e=6,p=8):new a(b)} a();` // TODO: can remove the parens here around instance of a
	);
	assertMinifier(
		`function updateChildren (parentElm, oldCh, newCh, insertedVnodeQueue, removeOnly) { var newStartIdx = 0; var oldStartVnode = oldCh[0]; var newStartVnode = newCh[0]; var oldKeyToIdx, idxInOld, elmToMove, refElm; var canMove = !removeOnly; while (a) { if (sameVnode(elmToMove, newStartVnode)) { patchVnode(elmToMove, newStartVnode, insertedVnodeQueue); oldCh[idxInOld] = undefined; canMove && nodeOps.insertBefore(parentElm, newStartVnode.elm, oldStartVnode.elm); newStartVnode = newCh[++newStartIdx]; } else { createElm(newStartVnode, insertedVnodeQueue, parentElm, oldStartVnode.elm); newStartVnode = newCh[++newStartIdx]; } } } updateChildren();`,
		`function updateChildren(e,n,d,t,o){var r=0,l=n[0],w=d[0],m,h,i,v,u=!o;while(a)w=sameVnode(i,w)?(patchVnode(i,w,t),n[h]=void 0,u&&nodeOps.insertBefore(e,w.elm,l.elm),d[++r]):(createElm(w,t,e,l.elm),d[++r])}updateChildren();`
	);
	assertMinifier(
		`var a = 12300 + 1230000`,
		`var a = 1242300`
	);
}



