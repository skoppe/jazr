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
	import std.array : replaceSlice;
	import std.conv : to;
	size_t start = 0;
	while(true)
	{
		auto idx = str.value[start..$].countUntil(cast(const(ubyte)[])"\\u");
		if (idx == -1)
			return;
		auto cntEscapes = str.value[0..idx+start].retro.countUntil!(b => b != cast(ubyte)'\\');
		if ((cntEscapes & 0x1) != 0)
		{
			start += idx + 4;
		} else
		{
			auto base10 = (cast(const(char)[])str.value[start+idx+2..start+idx+6]).to!uint(16);
			auto unicode = base10.toUnicodeRepresentation;
			str.value = str.value.replaceSlice(str.value[start+idx..start+idx+6], unicode);
			start += idx + unicode.length;
		}
		if (start >= str.value.length)
			return;
	}
}

@("convertEscapedUnicodeToUnicode")
unittest
{
	alias assertConvertEscapedUnicode = assertTransformations!(convertEscapedUnicodeToUnicode);

	assertConvertEscapedUnicode(
		`"asdfbcasdf";`,
		`"asdfbcasdf";`
	);
	assertConvertEscapedUnicode(
		`"asdfbc\u00A2basdf"`,
		`"asdfbc¢basdf"`
	);
	assertConvertEscapedUnicode(
		`"asdfbc\u0309basdf"`,
		`"asdfbc̉basdf"`
	);
	assertConvertEscapedUnicode(
		`"ab\u00A2c\u309b\u20ACas"`,
		`"ab¢c゛€as"`
	);
	assertConvertEscapedUnicode(
		`"ab\\u00A2c\u309b\u20ACas"`,
		`"ab\\u00A2c゛€as"`
	);
	assertConvertEscapedUnicode(
		`"ab\\\u00A2c\u309b\u20ACas"`,
		`"ab\\¢c゛€as"`
	);
	assertConvertEscapedUnicode(
		`"\u"`,
		`"\u"`
	);
	assertConvertEscapedUnicode(
		`"\\u"`,
		`"\\u"`
	);
}

void removeUseStrict(StringLiteralNode strLit, out Node replacedWith)
{
	if (strLit.parent.type != NodeType.ModuleNode &&
		strLit.parent.type != NodeType.FunctionBodyNode)
		return;
	if (cast(const(char)[])strLit.value != "use strict")
		return;
	strLit.detach();
}

@("removeUseStrict")
unittest
{
	alias assertRemoveUseStrict = assertTransformations!(removeUseStrict);

	assertRemoveUseStrict(
		`"use strict";var a = 4;`,
		`var a = 4;`
	);
	assertRemoveUseStrict(
		`"use strict";var a = 4;"use strict";function bla(){"use strict";return 5}`,
		`var a = 4;function bla(){return 5}`
	);
}