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
module es6.transforms.stringliterals;

import es6.nodes;
import es6.scopes;
import es6.analyse;
import es6.eval;
import es6.lexer;
import es6.transforms.expressions;
import std.algorithm : each, countUntil;
import std.range : retro;
import std.array : appender, Appender;
import es6.bench;

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

void convertEscapedUnicodeToUnicode(StringLiteralNode str, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	import std.array : replaceSlice;
	import std.conv : to;
	size_t start = 0;
	while(start < str.value.length)
	{
		auto idx = str.value[start..$].countUntil(cast(const(ubyte)[])"\\u");
		if (idx == -1)
			break;

		if (start+idx+6 > str.value.length)
			break;

		auto cntEscapes = str.value[0..idx+start].retro.countUntil!(b => b != cast(ubyte)'\\');
		if (cntEscapes != -1 && (cntEscapes & 0x1) != 0)
		{
			start += idx + 4;
		} else
		{
			auto base10 = (cast(const(char)[])str.value[start+idx+2..start+idx+6]).to!uint(16);
			if (base10 == 39)
			{
				str.value = str.value.replaceSlice(str.value[start+idx..start+idx+6], cast(const(ubyte)[])`\'`);
				start += idx + 2;
			} else {
				if (!isLineTerminator(base10))
				{
					auto unicode = base10.toUnicodeRepresentation;
					str.value = str.value.replaceSlice(str.value[start+idx..start+idx+6], unicode);
					start += idx + unicode.length;
				} else
					start += idx + 6;
			}
		}
	}
	start = 0;
	while(start < str.value.length)
	{
		auto idx = str.value[start..$].countUntil(cast(const(ubyte)[])"\\x");
		if (idx == -1)
			return;

		if (start+idx+4 > str.value.length)
			return;
		auto cntEscapes = str.value[0..idx+start].retro.countUntil!(b => b != cast(ubyte)'\\');
		if (cntEscapes != -1 && (cntEscapes & 0x1) != 0)
		{
			start += idx + 2;
		} else
		{
			auto base10 = (cast(const(char)[])str.value[start+idx+2..start+idx+4]).to!uint(16);
			if (base10 == 39)
			{
				str.value = str.value.replaceSlice(str.value[start+idx..start+idx+4], cast(const(ubyte)[])`\'`);
				start += idx + 2;
			} else {
				if (!isLineTerminator(base10))
				{
					auto unicode = base10.toUnicodeRepresentation;
					str.value = str.value.replaceSlice(str.value[start+idx..start+idx+4], unicode);
					start += idx + unicode.length;
				} else
					start += idx + 4;
			}
		}
	}
}

@("convertEscapedUnicodeToUnicode")
unittest
{


	//We need to determine valid chars here that we want to convert from \\uxxxx to raw form (some arent, like \u2028)....

	alias assertConvertEscapedUnicode = assertTransformations!(convertEscapedUnicodeToUnicode);

	assertConvertEscapedUnicode(
		`"asdfbcasdf";`,
		`"asdfbcasdf";`
	);
	assertConvertEscapedUnicode(
		`"asdfbc\u00AAbasdf"`,
		`"asdfbcªbasdf"`
	);
	assertConvertEscapedUnicode(
		`"asdfbc\u0309basdf"`,
		`"asdfbc̉basdf"`
	);
	assertConvertEscapedUnicode(
		`"ab\u00AAc\u309b\u0acbas"`,
		`"abªc゛ોas"`
	);
	assertConvertEscapedUnicode(
		`"ab\\u00A2c\u309b\u0acbas"`,
		`"ab\\u00A2c゛ોas"`
	);
	assertConvertEscapedUnicode(
		`"ab\\\u00AAc\u309b\u0acbas"`,
		`"ab\\ªc゛ોas"`
	);
	assertConvertEscapedUnicode(
		`"\u"`,
		`"\u"`
	);
	assertConvertEscapedUnicode(
		`"\\u"`,
		`"\\u"`
	);
	assertConvertEscapedUnicode(
		`var a = {quot:'\u0022',amp:'\u0026',apos:'\u0027',gt:'\u003E',nbsp:'\u00A0',iexcl:'\u00A1'}`,
		`var a= {quot: '"', amp: '&', apos: '\'', gt: '>', nbsp: ' ', iexcl: '¡'};`
	);
	assertConvertEscapedUnicode(
		`var a = {'\u0100': 'A', '\x38': 5, '\x27': 'b', '\u2028': 'bla'}`,
		`var a = {'Ā': 'A', '8': 5, '\'': 'b', '\u2028': 'bla'};`
	);
}

bool removeRedundantUseStrict(StringLiteralNode strLit, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (strLit.parent.type != NodeType.ModuleNode &&
		strLit.parent.type != NodeType.FunctionBodyNode)
		return false;
	if (cast(const(char)[])strLit.value != "use strict")
		return false;
	strLit.detach();
	return true;
}

@("removeRedundantUseStrict")
unittest
{
	alias assertRemoveUseStrict = assertTransformations!(removeRedundantUseStrict);

	assertRemoveUseStrict(
		`"use strict";var a = 4;`,
		`"use strict";var a = 4;`
	);
	assertRemoveUseStrict(
		`"use strict";var a = 4;"use strict";function bla(){"use strict";return 5}`,
		`"use strict";var a = 4;function bla(){return 5}`
	);
	assertRemoveUseStrict(
		`var b = function(e,t,r){"use strict";'use strict';};`,
		`var b = function(e,t,r){"use strict";};`
	);
}

void factorOutCommonStrings(Scope scp)
{
	import std.algorithm : reverse, map, uniq, min;
	import std.range : zip, retro;
	Scope findCommonScope(Scope a, Scope b)
	{
		if (a is b)
			return a;

		auto appA = appender!(Scope[]);
		auto ac = a;
		do
		{
			appA.put(ac);
			ac = ac.parent;
		} while (ac !is null);

		auto appB = appender!(Scope[]);
		auto bc = b;
		do
		{
			appB.put(bc);
			bc = bc.parent;
		} while (bc !is null);

		auto zipped = appA.data.retro.zip(appB.data.retro);
		//TODO: create helper so we can do `return zipped.getLastWhere!(z => z[0] is z[1]);` instead of stuff below
		auto idx = zipped.countUntil!(z => z[0] !is z[1]);

		if (idx == -1)
		{
			auto shortest = min(appA.data.length,appB.data.length);
			assert(appA.data.retro[shortest-1] is appB.data.retro[shortest-1]);
			return appA.data.retro[shortest-1];
		}

		assert(idx != 0);

		return appA.data.retro[idx-1];
	}
	struct Dict
	{
		struct Item
		{
			Scope commonScope;
			Appender!(StringLiteralNode[]) nodes;
		}
		Item[const(ubyte)[]] dict;
		void add(StringLiteralNode n)
		{
			if (auto p = n.value in dict)
			{
				(*p).nodes.put(n);
				(*p).commonScope = findCommonScope(n.branch.scp,(*p).commonScope);
			} else
				dict[n.value] = Item(n.branch.scp,appender!(StringLiteralNode[])([n]));
		}
	}
	Dict dict;
	void collect(Node n)
	{
		if (n.type == NodeType.StringLiteralNode)
			dict.add(n.as!StringLiteralNode);
		else
			foreach(c; n.children)
				collect(c);
	}

	collect(scp.entry);

	int idCnt = 0;
	foreach(key, ref value; dict.dict)
	{
		auto len = key.length;
		auto cnt = value.nodes.data.length;
		auto idenLen = 2; // we don't know so we speculate

		auto currentLength = len * cnt;
		auto speculatedLength = 5 + idenLen + len + (idenLen * cnt);
		if (speculatedLength >= currentLength)
			continue;

		auto varName = "__gen" ~ generateIdentifier(idCnt++);/*measure!("getFreeIdentifier", () { return value.commonScope.getFreeIdentifier(idCnt); })*/
		auto definition = new IdentifierReferenceNode(cast(const(ubyte)[])varName);
		auto lit = new StringLiteralNode(key);
		auto varStmt = new VariableStatementNode([new VariableDeclarationNode(definition, lit)]);

		value.commonScope.entry.prependChildren([varStmt]);
		auto idRefs = value.nodes.data.map!(n => n.replaceWith(new IdentifierReferenceNode(cast(const(ubyte)[])varName))).array();
		foreach(r; idRefs)
			r.branch.scp.addIdentifier(Identifier(r.as!IdentifierReferenceNode,definition));

		auto var = Variable(definition, IdentifierType.Variable);
		var.references = idRefs;

		value.commonScope.addVariable(var, true);
		varStmt.reanalyseHints();
	}
}

@("factorOutCommonStrings")
unittest
{
	void assertFactorOutStrings(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		import es6.transforms.variables;
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		foreach(s; got.branch.scp.children)
			factorOutCommonStrings(s);
		got.assertTreeInternals(file,line);
		got.branch.scp.shortenVariables();
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}

	assertFactorOutStrings(
		`function a(){ a = "a string"; }`,
		`function a(){ a = "a string"; }`
	);
	assertFactorOutStrings(
		`function a(){ a = "a string"; b = "a string";}`,
		`function a(){ a = "a string"; b = "a string";}`
	);
	assertFactorOutStrings(
		`function a(){ a = "a string"; b = "a string"; c = "a string"; }`,
		`function a(){var n='a string';a=n;b=n;c=n}`
	);
	assertFactorOutStrings(
		`function a(){ function b(){ a = "a string"; } function c() { b = "a string"; } function d() { function e() { l = "a string"; } } }`,
		`function a(){var n='a string';function t(){a=n}function e(){t=n}function r(){function t(){l=n}}}`
	);
	assertFactorOutStrings(
		`function a(){ function b(){ a = "a string"; } function c() { b = "a string"; } function d() { function e() { l = "a string"; } } }function k(){ function b(){ a = "a string"; } function c() { b = "a string"; } function d() { function e() { l = "a string"; } } }`,
		`function a(){var n='a string';function t(){a=n}function e(){t=n}function r(){function t(){l=n}}}function k(){var n='a string';function t(){a=n}function e(){t=n}function r(){function t(){l=n}}}`
	);
	assertFactorOutStrings(
		`function z(){ function s(){ x = "a string"; } function r() { y = "a string"; } function q() { var a = function() { l = "a string"; } } }`,
		`function z(){var n='a string';function t(){x=n}function e(){y=n}function r(){var t=function(){l=n}}}`
	);
	assertFactorOutStrings(
		`function z(){ function s(){ x = "a string"; } function r() { y = "a string"; function w() { var e = a * 66; } } function q() { function h() { l = "a string"; } } }`,
		`function z(){var n='a string';function t(){x=n}function e(){y=n;function t(){var n=a*66}}function r(){function t(){l=n}}}`
	);
}


