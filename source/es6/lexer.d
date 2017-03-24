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
module es6.lexer;

@safe:

import std.range : empty, front, popFront, save, take, drop, ElementType;
import std.array : Appender, appender;
import std.traits : Unqual;
import es6.tokens;
import core.cpuid : sse42;
import es6.bench;

version (D_InlineAsm_X86_64)
{
    version (Windows) {}
    else version = iasm64NotWindows;
}
version (unittest)
{
	import unit_threaded;
	import std.stdio;
	import std.algorithm : equal;
}
enum State
{
	TokensRemaining,
	EndOfFile,
	LexingTemplateLiteral
}

enum Goal
{
	//InputElementDiv, // this is the default goal
	//InputElementRegExpOrTemplateTail, // is used in syntactic grammar contexts where a RegularExpressionLiteral, a TemplateMiddle, or a TemplateTail is permitted (only in yield)
	//InputElementRegExp, // is used in all syntactic grammar contexts where a RegularExpressionLiteral is permitted but neither a TemplateMiddle, nor a TemplateTail is permitted (in primaryexpression)
	//InputElementTemplateTail, // is used in all syntactic grammar contexts where a TemplateMiddle or a TemplateTail is permitted but a RegularExpressionLiteral is not permitted. (unused)
	All,
	ProhibitRegex
}

enum Keyword {
	Unknown = 0, 
	Static = 1, 
	True = 2, 
	Await = 3, 
	Enum = 4, 
	Instanceof = 5, 
	Import = 6, 
	Null = 7, 
	New = 8, 
	Throw = 9, 
	Export = 10, 
	False = 11, 
	This = 12, 
	Continue = 13, 
	For = 14, 
	With = 15, 
	Extends = 16, 
	If = 17, 
	Return = 18, 
	Function = 19, 
	Catch = 20, 
	Var = 21, 
	While = 22, 
	Switch = 23, 
	Void = 24, 
	Const = 25, 
	Finally = 26, 
	Try = 27, 
	Set = 28, 
	Typeof = 29, 
	Let = 30, 
	Get = 31, 
	Case = 32, 
	Delete = 33, 
	In = 34, 
	Super = 35, 
	Else = 36, 
	Break = 37, 
	Yield = 38, 
	Default = 39, 
	Class = 40, 
	Debugger = 41, 
	Do = 42
};


pure struct Keywords
{

private:

    /*
        rendered on 2017-Jan-10 18:04:32.066315 by IsItThere.
         - PRNG seed: 0
         - map length: 128
    */

    static const string[128] _words = ["", "static", "", "true", "", "", "", "await", "enum", "", "instanceof", "", "", "import", "", "", "null", "", "", "", "new", "", "", "", "throw", "", "export", "", "", "false", "this", "continue", "", "for", "with", "", "", "", "extends", "", "", "", "", "if", "", "return", "", "function", "", "catch", "var", "", "", "", "", "", "while", "", "", "", "", "", "", "switch", "void", "", "", "const", "", "", "", "finally", "", "", "", "", "", "", "", "", "try", "", "set", "typeof", "let", "", "", "", "", "", "get", "", "case", "delete", "in", "super", "", "", "", "", "", "", "", "", "else", "", "", "", "break", "", "", "yield", "", "", "", "", "", "default", "class", "", "", "", "", "", "debugger", "", "", "do"];

    static const bool[128] _filled = [false, true, false, true, false, false, false, true, true, false, true, false, false, true, false, false, true, false, false, false, true, false, false, false, true, false, true, false, false, true, true, true, false, true, true, false, false, false, true, false, false, false, false, true, false, true, false, true, false, true, true, false, false, false, false, false, true, false, false, false, false, false, false, true, true, false, false, true, false, false, false, true, false, false, false, false, false, false, false, false, true, false, true, true, true, false, false, false, false, false, true, false, true, true, true, true, false, false, false, false, false, false, false, false, true, false, false, false, true, false, false, true, false, false, false, false, false, true, true, false, false, false, false, false, true, false, false, true];

    static const ubyte[256] _coefficients = [132, 83, 10, 140, 38, 217, 84, 227, 160, 231, 126, 71, 63, 65, 208, 248, 35, 53, 231, 149, 28, 47, 221, 156, 181, 179, 242, 128, 220, 240, 200, 1, 7, 98, 188, 71, 101, 80, 212, 39, 255, 159, 40, 138, 80, 28, 128, 181, 67, 0, 6, 33, 202, 183, 205, 167, 49, 197, 132, 164, 23, 32, 126, 196, 135, 3, 8, 66, 61, 205, 138, 124, 250, 217, 135, 125, 115, 200, 112, 18, 167, 137, 94, 185, 78, 59, 26, 1, 239, 80, 152, 75, 64, 46, 102, 194, 62, 37, 231, 4, 213, 154, 42, 161, 229, 1, 141, 249, 27, 20, 221, 42, 226, 210, 205, 153, 159, 125, 192, 29, 8, 100, 149, 30, 201, 110, 176, 63, 110, 1, 147, 21, 201, 5, 128, 207, 169, 255, 135, 247, 137, 126, 204, 90, 128, 215, 176, 225, 172, 224, 82, 13, 61, 91, 74, 43, 102, 249, 165, 178, 238, 153, 209, 87, 46, 214, 5, 231, 27, 138, 124, 44, 62, 79, 239, 189, 5, 68, 227, 235, 190, 91, 222, 155, 123, 11, 161, 33, 250, 199, 246, 21, 151, 248, 44, 39, 20, 80, 131, 88, 200, 133, 109, 0, 2, 164, 3, 177, 25, 87, 220, 53, 190, 122, 102, 130, 110, 8, 92, 240, 135, 96, 34, 240, 60, 8, 59, 107, 230, 82, 131, 104, 206, 168, 155, 135, 98, 40, 82, 207, 100, 238, 20, 139, 169, 187, 39, 255, 193, 91, 51, 21, 174, 226, 230, 200];

    static const Keyword[128] _map = [Keyword.Unknown, Keyword.Static, Keyword.Unknown, Keyword.True, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Await, Keyword.Enum, Keyword.Unknown, Keyword.Instanceof, Keyword.Unknown, Keyword.Unknown, Keyword.Import, Keyword.Unknown, Keyword.Unknown, Keyword.Null, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.New, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Throw, Keyword.Unknown, Keyword.Export, Keyword.Unknown, Keyword.Unknown, Keyword.False, Keyword.This, Keyword.Continue, Keyword.Unknown, Keyword.For, Keyword.With, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Extends, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.If, Keyword.Unknown, Keyword.Return, Keyword.Unknown, Keyword.Function, Keyword.Unknown, Keyword.Catch, Keyword.Var, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.While, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Switch, Keyword.Void, Keyword.Unknown, Keyword.Unknown, Keyword.Const, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Finally, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Try, Keyword.Unknown, Keyword.Set, Keyword.Typeof, Keyword.Let, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Get, Keyword.Unknown, Keyword.Case, Keyword.Delete, Keyword.In, Keyword.Super, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Else, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Break, Keyword.Unknown, Keyword.Unknown, Keyword.Yield, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Default, Keyword.Class, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Unknown, Keyword.Debugger, Keyword.Unknown, Keyword.Unknown, Keyword.Do];
    
    static ushort hash(const ubyte[] word) nothrow pure @safe @nogc
    {
        ushort result;
        foreach(i; 0..word.length)
        {
            result += _coefficients[word[i]];
        }
        return result % 128;
    }

public:

    static string opBinaryRight(string op: "in")(const string word)
    {
        const ushort h = hash(cast(const(ubyte)[])word);
        final switch(_filled[h])
        {
            case false: return word;
            case true:  
            	if (_words[h].length != word.length || _words[h] != word)
            		return word;
            	return _words[h];
        }
    }
    static Keyword get()(const(ubyte)[] word) nothrow @nogc
    {
    	//return measure!("Keywords.get",(){
	        const ushort h = hash(word);
	        if (!_filled[h])
	        	return Keyword.Unknown;
	        auto type = _map[h];
	        if (type == Keyword.Unknown || _words[h].length != word.length || _words[h] != word)
	        	return Keyword.Unknown;
	        return type;
    	//});
    }
}

Keyword matchKeyword(const(ubyte)[] keyword) nothrow
{
	if (keyword.length < 2 || keyword.length > 10)
		return Keyword.Unknown;
	return Keywords.get(keyword);
}

bool isReservedKeyword(Keyword keyword) nothrow pure @nogc
{
	return keyword != Keyword.Unknown &&
		keyword != Keyword.Static &&
		keyword != Keyword.Set &&
		keyword != Keyword.Let &&
		keyword != Keyword.Get &&
		keyword != Keyword.Enum &&	// TODO: enum and await are only reserved if Module is the goal symbol
		keyword != Keyword.Await;
}

bool isReservedKeyword(const(ubyte)[] keyword) nothrow
{
	if (keyword.length < 2 || keyword.length > 10)
		return false;
	auto k = Keywords.get(keyword);
	return k.isReservedKeyword();
}
bool isWhitespace(Char)(Char c) nothrow
{
	if (c < '\u0021')
		return c == '\u0009' || c == '\u000B' || c == '\u000C' || c == '\u0020';
	if (c == '\u00A0')
		return true;
	if (c < '\u02B0')
		return false;
	return (c == '\uFEFF' || (c >= '\u02B0' && c <= '\u02FF'));
}
// TODO: can we make this into a index computation and switch (1,2,3)?
size_t getWhiteSpaceLength(Range)(Range r, size_t idx = 0) nothrow
{
	if (r[idx] < 0xc0) // 1 byte code point
	{
		return (r[idx] == 0x09 || r[idx] == 0x0B || r[idx] == 0x0C || r[idx] == 0x20) ? 1 : 0;
	} else if (r[idx] < 0xe0) // 2 byte code point
	{
		return (r.decodeUnicodeCodePoint!(2)(idx) == 0xA0) ? 2 : 0;
	} else if (r[idx] < 0xf0) // 3 byte code point
	{
		auto hex = r.decodeUnicodeCodePoint!(3)(idx);
		if (hex > 0xfeff || hex < 0x2b0)
			return 0;
		return (hex <= 0x2ff || hex == 0xfeff) ? 3 : 0;
	}
	return 0;
}
@("isWhitespace")
unittest
{
	import std.algorithm : all;
	import std.range : iota;
	assert("\x09\x0b\x0c\x20\u00a0\ufeff".all!isWhitespace);
	assert(iota('\u02b0','\u02ff').all!isWhitespace);
}
auto next(Source)(ref Source s) nothrow
{
	auto chr = s.front();
	s.popFront();
	return chr;
}
bool popNextIf(Source, Elem)(ref Source s, Elem e)
{
	//import std.format : format;
	//assert(!s.empty,format("Invalid popNextIf from %s @ %s",file,line));
	if (s.front != e)
		return false;
	s.popFront();
	return true;
}
@("popNextIf")
unittest
{
	auto r = "\x09abc";
	r.popNextIf('\x09').shouldBeTrue();
	r.shouldEqual("abc");
	r.popNextIf('b').shouldBeFalse();
	r.shouldEqual("abc");
}
//auto isLineTerminator(Char)(Char s)
//{
//	return (s == 0x0a || s == 0x0d);// || s == '\u2028' || s == '\u2029');
//}
// TODO: can we make this into a index computation and switch (1,2,3)?
size_t getLineTerminatorLength(Range)(Range r, size_t idx = 0) pure nothrow
{
	if (r[idx] <= 0x7f)
	{
		if (r[idx] == 0x0d)
			return r[idx+1] == 0x0a ? 2 : 1;
		return r[idx] == 0x0a ? 1 : 0;
	}
	if (r[idx] >= 0xf0) // 4 byte unicode
	{
		return 0;
	} else if (r[idx] >= 0xe0)
	{
		auto hex = r.decodeUnicodeCodePoint!3;	// 3 byte unicode
		return (hex == 0x2028 || hex == 0x2029) ? 3 : 0;
	}
	return 0;
}
auto getLineTerminatorUnicodeLength(Range)(Range r) pure nothrow
{
	if (r[0] < 0xe0 && r[0] >= 0xf0)
		return 0;
	auto hex = r.decodeUnicodeCodePoint!3;	// 3 byte unicode
	return (hex == 0x2028 || hex == 0x2029) ? 3 : 0;
}
auto getRegexLength(Range)(Range r) pure nothrow
{
	size_t idx = 1;
	bool inOneOfExpr = false;
	if (r[idx] == '*' || r[idx] == '/')
		return 0;
	while (true)
	{
		switch (r[idx])
		{
			case '[':
				inOneOfExpr = true;
				idx++;
				break;
			case ']':
				inOneOfExpr = false;
				idx++;
				break;
			case '\\':
				idx++;
				auto len = r.getLineTerminatorLength(idx);
				if (len != 0)
					return 0;
				if (r[idx] == 0)
					return 0;
				idx += r[idx..$].getUnicodeLength();
				break;
			case '/':
				idx++;
				if (!inOneOfExpr)
				{
					while (true)
					{
						auto len = r[idx..$].getTailIdentifierLength();
						if (len == 0)
							return idx;
						idx += len;
					}
				}
				break;
			default:
				auto len = r[idx..$].getLineTerminatorLength();
				if (len != 0)
					return 0;
				if (r[idx] == 0)
					return 0;
				idx += r[idx..$].getUnicodeLength();
				break;
		}
	}
}

bool isValid2ByteIdentifierStart(size_t cp) nothrow @nogc
{
	static const int[] cp2b = [0xaa,0xaa,0xb5,0xb5,0xba,0xba,0xc0,0xd6,0xd8,0xf6,0xf8,0x2c1,0x2c6,0x2d1,0x2e0,0x2e4,0x2ec,0x2ec,0x2ee,0x2ee,0x370,0x374,0x376,0x377,0x37a,0x37d,0x37f,0x37f,0x386,0x386,0x388,0x38a,0x38c,0x38c,0x38e,0x3a1,0x3a3,0x3f5,0x3f7,0x481,0x48a,0x52f,0x531,0x556,0x559,0x559,0x561,0x587,0x5d0,0x5ea,0x5f0,0x5f2,0x620,0x64a,0x66e,0x66f,0x671,0x6d3,0x6d5,0x6d5,0x6e5,0x6e6,0x6ee,0x6ef,0x6fa,0x6fc,0x6ff,0x6ff,0x710,0x710,0x712,0x72f,0x74d,0x7a5,0x7b1,0x7b1,0x7ca,0x7ea,0x7f4,0x7f5,0x7fa,0x7fa];
	size_t len = cp2b.length / 2;
	size_t idx = len / 2;
	while (true)
	{
		if (cp2b[idx*2] > cp)
		{
			len /= 2;
			if (len == 0)
				return false;
			idx -= (len / 2);
		} else if (cp2b[idx*2] < cp)
		{
			if (cp2b[idx*2+1] >= cp)
				return true;
			len /= 2;
			if (len == 0)
				return false;
			idx += (len / 2);
		} else
			return true;
	}
}
@("isValid2ByteIdentifierStart")
unittest
{
	isValid2ByteIdentifierStart(0xba).shouldBeTrue;
	isValid2ByteIdentifierStart(0x2aa).shouldBeTrue;
	isValid2ByteIdentifierStart(0xd7).shouldBeFalse;
	isValid2ByteIdentifierStart(0x2c5).shouldBeFalse;
}

auto getStartIdentifierLength(Range)(Range r) nothrow
	if (is (ElementType!Range : ubyte))
{
	ubyte first = r.front();
	if (first <= 0x7f)
		return (first == '$' || first == '_' || (first >= '\x41' && first <= '\x5a') || (first >= '\x61' && first <= '\x7a')) ? 1 : 0;
	if (first >= 0xf0) // 4 byte unicode
	{
		return 0;
	} else if (first >= 0xe0)
	{
		auto hex = r.decodeUnicodeCodePoint!3;	// 3 byte unicode
		return (hex == 0x2118 || hex == 0x212e || hex == 0x309b || hex == 0x309c) ? 3 : 0;
	} else if (first >= 0xc0)
	{
		auto hex = r.decodeUnicodeCodePoint!2;	// 2 byte unicode
		return hex.isValid2ByteIdentifierStart ? 2 : 0;
		//return (hex == 0xaa || hex == 0xb5 || hex == 0xba || (hex >= 0xc0 && hex <= 0xd6) || (hex >= 0xd8 && hex <= 0xf6)) ? 2 : 0;
	}
	return 0;
}

size_t decodeUnicodeCodePoint(size_t length, Range)(Range r, size_t idx = 0) pure nothrow
{
	static if (length == 2)
	{
		return ((r[idx] & 0x1f) << 6) | (r[idx+1] & 0x3f);
	} else static if (length == 3)
	{
		return ((r[idx] & 0x0f) << 12) | ((r[idx+1] & 0x3f) << 6) | (r[idx+2] & 0x3f);
	} else static if (length == 4)
	{
		return ((r[idx] & 0x07) << 18) | ((r[idx+1] & 0x3f) << 12) | ((r[idx+2] & 0x3f) << 6) | (r[idx+3] & 0x3f);
	}
	static assert("Invalid unicode code point byte length");
}

@("decodeUnicodeCodePoint")
unittest {
	assert([0xC2,0xA2].decodeUnicodeCodePoint!(2) == 0xA2);
	assert([0xE3,0x82,0x9B].decodeUnicodeCodePoint!(3) == 0x309b);
	assert([0xE2,0x82,0xAC].decodeUnicodeCodePoint!(3) == 0x20AC);
	assert([0xF0,0x90,0x8D,0x88].decodeUnicodeCodePoint!(4) == 0x10348);
}
// TODO: can we make this into a index computation and switch (1,2,3)?
size_t getUnicodeLength(Range)(Range r, size_t idx = 0) pure nothrow @nogc
{
	if (r[idx] < 0xc0)
		return 1;
	if (r[idx] < 0xe0)
		return 2;
	if (r[idx] < 0xf0)
		return 3;
	return 4;
}

template unicodeRepresentation(uint i) {
	static if (i < 0x800) {
		enum unicodeRepresentation = cast(ubyte[])[0xc0 | ((i >> 6) & 0x1f), 0x80 | (i & 0x3f)];
	} else static if (i < 0x10000) {
		enum unicodeRepresentation = cast(ubyte[])[0xe0 | ((i >> 12) & 0x0f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else static if (i < 0x110000) {
		enum unicodeRepresentation = cast(ubyte[])[0xf0 | ((i >> 18) & 0x07), 0x80 | ((i >> 12) & 0x3f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else {
		static assert("invalid unicode code point");
	}
}
auto toUnicodeRepresentation(uint i) pure nothrow
{
	if (i < 0x800) {
		return cast(ubyte[])[0xc0 | ((i >> 6) & 0x1f), 0x80 | (i & 0x3f)];
	} else if (i < 0x10000) {
		return cast(ubyte[])[0xe0 | ((i >> 12) & 0x0f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else if (i < 0x110000) {
		return cast(ubyte[])[0xf0 | ((i >> 18) & 0x07), 0x80 | ((i >> 12) & 0x3f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else {
		return cast(ubyte[])[];
	}
}
//auto isTailIdentifier(Char)(Char chr)
//{
//	return 
//		(chr >= '\u0061' && chr <= '\u007a') || (chr >= '\u0041' && chr <= '\u005a') ||  (chr >= '\u0030' && chr <= '\u0039') || 
//		(!isWhitespace(chr) && (
//			chr == '$' || chr == '_' || chr == '\u005f' ||
//			(chr >= '\u00c0' && chr <= '\u00d6') || (chr >= '\u00d8' && chr <= '\u00f6') ||
//			chr == '\u2118' || chr == '\u212e' || (chr >= '\u309b' && chr <= '\u309c') || chr == '\u00aa' || 
//			chr == '\u00b5' || chr == '\u00b7' || chr == '\u00ba' ||
//			chr == '\u00b7' || chr == '\u0387' || (chr >= '\u1369' && chr <= '\u1371') || chr == '\u19da' || chr == '\u200C' ||
//			chr == '\u200D')
//		);
//}

auto getTailIdentifierLength(Range)(Range r, size_t idx = 0) pure nothrow @nogc
{
	if (r[idx] < 0x80)
		return ((r[idx] >= 0x61 && r[idx] <= 0x7a) || (r[idx] >= 0x41 && r[idx] <= 0x5a) || (r[idx] >= 0x30 && r[idx] <= 0x39) || r[idx] == '$' || r[idx] == '_' || r[idx] == 0x5f) ? 1 : 0;
	if (r[idx] >= 0xf0) // 4 byte unicode
	{
		return 0;
	} else if (r[idx] >= 0xe0)
	{
		auto hex = r.decodeUnicodeCodePoint!3;	// 3 byte unicode
		return ((hex >= 0x1369 && hex <= 0x1371) || hex == 0x19da || hex == 0x200C || hex == 0x200D || hex == 0x2118 || hex == 0x212e || hex == 0x309b || hex == 0x309c) ? 3 : 0;
	} else if (r[idx] >= 0xc0)
	{
		auto hex = r.decodeUnicodeCodePoint!2;	// 2 byte unicode
		return hex == 0xaa || hex == 0xb5 || hex == 0xb7 || hex == 0xba || (hex >= 0xc0 && hex <= 0xd6) || (hex >= 0xd8 && hex <= 0xf6) || hex == 0x387;
	}
	assert(0);
}

auto coerceToSingleQuotedString(const ubyte[] str) pure nothrow
{
	import std.algorithm : max,min;
	if (str.length == 0)
		return str;
	auto sink = appender!(ubyte[]);
	sink.reserve(str.length+max(2,min(str.length / 4, 5)));

	bool escape = false;
	foreach(s; str)
	{
		if (escape)
		{
			if (s != '"')
				sink.put('\\');
			escape = s == '\\';
		}
		if (s == '\\')
		{
			escape = true;
			continue;
		}
		if (s == '\'')
			sink.put('\\');
		sink.put(s);
	}
	if (escape)
		sink.put('\\');
	return sink.data;
}
auto coerceToSingleQuotedString(string str) pure nothrow
{
	return cast(const(char)[])coerceToSingleQuotedString(cast(const(ubyte)[])str);
}
@("coerceToSingleQuotedString")
unittest
{
	assert("".coerceToSingleQuotedString == "");
	assert(`isn't it nice`.coerceToSingleQuotedString == `isn\'t it nice`);
	assert(`a 'good' way to go`.coerceToSingleQuotedString == `a \'good\' way to go`);
	"a \\\"good\\\" way to go".coerceToSingleQuotedString.shouldEqual(`a "good" way to go`);
	assert(`*(?:'((?:\\\\.|[^\\\\'])*)'|\"((?:\\\\.|[^\\\\\"])*)\"|(`.coerceToSingleQuotedString == `*(?:\'((?:\\\\.|[^\\\\\'])*)\'|"((?:\\\\.|[^\\\\"])*)"|(`);
	assert(`a \xaa\xbb`.coerceToSingleQuotedString == `a \xaa\xbb`);
	assert(`\\`.coerceToSingleQuotedString == `\\`);
}
ulong rangeMatch(bool invert, chars...)(const ubyte*) pure nothrow @trusted @nogc
{
    static assert (chars.length % 2 == 0);
    enum constant = ByteCombine!chars;
    static if (invert)
        enum rangeMatchFlags = 0b0000_0100;
    else
        enum rangeMatchFlags = 0b0001_0100;
    enum charsLength = chars.length;
    asm pure nothrow @nogc
    {
        naked;
        movdqu XMM1, [RDI];
        mov R10, constant;
        movq XMM2, R10;
        mov RAX, charsLength;
        mov RDX, 16;
        pcmpestri XMM2, XMM1, rangeMatchFlags;
        mov RAX, RCX;
        ret;
    }
}
ulong skip(bool matching, chars...)(const ubyte*) pure nothrow
    @trusted @nogc if (chars.length <= 8)
{
    enum constant = ByteCombine!chars;
    enum charsLength = chars.length;
    static if (matching)
        enum flags = 0b0001_0000;
    else
        enum flags = 0b0000_0000;
    asm pure nothrow @nogc
    {
        naked;
        movdqu XMM1, [RDI];
        mov R10, constant;
        movq XMM2, R10;
        mov RAX, charsLength;
        mov RDX, 16;
        pcmpestri XMM2, XMM1, flags;
        mov RAX, RCX;
        ret;
    }
}
template ByteCombine(c...)
{
    static assert (c.length <= 8);
    static if (c.length > 1)
        enum ulong ByteCombine = c[0] | (ByteCombine!(c[1..$]) << 8);
    else
        enum ulong ByteCombine = c[0];
}
// TODO: in general whenever we have an invalid char (e.g. an a-z in a decimalliteral), we need to skip all chars until we hit a separator (whitespace or punctuation)
struct Lexer
{
	private
	{
		Appender!(State[]) lexerState;
		bool haveSSE42;
	}
	State lexState;
	Token token;
	size_t line;
	size_t column;
	private size_t tokenLength;
	const (ubyte)[] s;
	void pushState(State state) pure
	{
		lexerState.put(lexState);
		lexState = state;
	}
	void popState()
	{
		assert(lexerState.data.length > 0);
		lexerState.shrinkTo(lexerState.data.length-1);
		lexState = lexerState.data[$-1];
	}
	Lexer save()
	{
		auto l = Lexer(s);
		l.lexState = this.lexState;
		l.token = this.token;
		l.line = this.line;
		l.column = this.column;
		l.tokenLength = this.tokenLength;
		l.lexerState = appender!(State[])(this.lexerState.data.dup());
		return l;
	}
	this(const (ubyte)[] source) pure
	{
		s = source;
		pushState(s.empty ? State.EndOfFile : State.TokensRemaining);
		haveSSE42 = sse42();
	}
	version(unittest)
	{
		auto scanToken(Goal goal = Goal.All, in string file = __FILE__, in size_t orgLine = __LINE__)
		{
			//return measure!("Scantoken",(){
				column += tokenLength;
				tokenLength = 0;
				token = lexToken(goal);
				version(chatty) { import std.stdio; writefln("Scan: %s with %s @%s:%s %s called from %s:%s",token, goal, line, column, tokenLength, file,orgLine); }
				return token;
			//});
		}
	} else
	{
		auto scanToken(Goal goal = Goal.All)
		{
			//return measure!("Scantoken",(){
				column += tokenLength;
				tokenLength = 0;
				token = lexToken(goal);
				version(chatty) { import std.stdio; writefln("Scan: %s with %s @%s:%s %s called",token, goal, line, column, tokenLength); }
				return token;
			//});
		}
	}
	private auto createTokenAndAdvance(Type tokenType, size_t len, const (ubyte)[] match) nothrow
	{
		s = s[len..$];
		return Token(tokenType, match);
	}
	private auto createTokenAndAdvance(Type tokenType, size_t len, string match) nothrow
	{
		s = s[len..$];
		return Token(tokenType, match);
	}
	Token lexUnicodeEscapeSequence(size_t idx)
	{
		import std.format : format;
		foreach(i; 0..4)
		{
			auto chr = s[idx+i];
			if ((chr <= '0' || chr >= '9') && (chr <= 'a' && chr >= 'z') && (chr <= 'A' && chr >= 'Z'))
			{
				if (chr == 0)
					return Token(Type.Error,"Found eof before finishing parsing UnicodeEscapeSequence");
				// TODO: if chr is lineterminator (as well as the \r\n one) we need to reset column and line++
				return Token(Type.Error,format("Invalid hexdigit %s for UnicodeEscapeSequence",chr));
			}
		}
		return Token(Type.UnicodeEscapeSequence);
	}
	Token lexStartIdentifier(ref size_t idx)
	{
		import std.format : format;
		auto chr = s[idx];
		auto len = s.getStartIdentifierLength();
		if (len > 0)
		{
			tokenLength ++;
			idx += len;
			return Token(Type.StartIdentifier);
		}
		if (chr == '\\')
		{
			auto chr2 = s[idx];
			idx++;
			tokenLength++;
			if (chr2 != 'u')
			{
				if (chr2 == 0)
					return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
				return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr2));
			}
			idx += 4;
			tokenLength += 4;
			return lexUnicodeEscapeSequence(idx-4);
		}
		idx++;
		return Token(Type.Error,format("Invalid character %s to start identifier",chr));
	}
	Token lexIdentifier() @trusted
	{
		import std.format : format;
		size_t idx = 0;
		version (iasm64NotWindows)
		{
			//if (haveSSE42)
			//{
				immutable ulong i = rangeMatch!(false, 'a', 'z', 'A', 'Z', '_', '_', '$', '$')(s.ptr + idx);
				if (i > 0)
				{
					//import std.stdio;
					//writeln(cast(const(char)[])s[idx..idx+10],":",i,",");
					idx += i;
					tokenLength += i;
				} else
				{
					auto t = lexStartIdentifier(idx);
					if (t.type == Type.Error)
					{
						s = s[idx..$];
						return t;
					}
				}
			//}
		} else {
			auto t = lexStartIdentifier(idx);
			if (t.type == Type.Error)
			{
				s = s[idx..$];
				return t;
			}			
		}
		for(;;)
		{
			auto len = s.getTailIdentifierLength(idx);
			if (len == 0)
			{
				if (s[idx] == '\\')
				{
					idx++;
					tokenLength++;
					ubyte chr = s[idx++];
					tokenLength++;
					if (chr != 'u')
					{
						s = s[idx..$];
						if (chr == 0)
							return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
						return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr));
					}
					// TODO: when lexUnicodeEscapeSequence fails midway, we have already advanced idx and tokenLength by 4...
					idx += 4;
					tokenLength += 4;
					auto token = lexUnicodeEscapeSequence(idx-4);
					if (token.type == Type.Error)
					{
						s = s[idx..$];
						return token;
					}
				} else {
					auto tok = Token(Type.Identifier,s[0..idx]);
					s = s[idx..$];
					return tok;
				}
			} else 
			{
				idx += len;
				tokenLength ++;
			}
		}
	}
	void popWhitespace()
	{
		size_t idx = 0;
		for (;;)
		{
			auto len = s.getWhiteSpaceLength(idx);
			if (len == 0)
				break;
			idx += len;
			column += 1;
		}
		s = s[idx..$];
	}
	// TODO: test with nested [ or ]
	auto lookAheadRegex()
	{
		return s.getRegexLength();
	}
	Token lexString()
	{
		auto type = s.front();	// either " or '
		size_t idx = 1;
		while (true)
		{
			// TODO: lineterminators are not allowed!
			auto len = s.getUnicodeLength(idx);
			switch (len)
			{
				case 1:
					ubyte chr = s[idx++];
					column++;
					if (chr == type)
					{
						if (type == '"')
							return createTokenAndAdvance(Type.StringLiteral,idx,s[1..idx-1].coerceToSingleQuotedString);
						return createTokenAndAdvance(Type.StringLiteral,idx,s[1..idx-1]);
					}
					switch (chr)
					{
						case '\\':
							if (s[idx] == 0)
								goto eof;
							idx += s.getUnicodeLength(idx);
							break;
						case 0:
							goto eof;
						default:
							break;
					}
					break;
				default:
					idx += len;
					break;
			}
		}
		eof: return createTokenAndAdvance(Type.Error,idx,"Invalid eof while lexing string");
	}
	Token lexBaseLiteral(int base, alias TokenType)(bool isFloat = false)
	{
		static assert(base >= 1 && base <= 16,"Can only lex base 1..16 NumericLiterals");
		import std.array : appender;
		import std.format : format;
		size_t idx = 0;
		bool fraction = false;
		bool exponent = false;
		bool expectingNumbers = true;
		while (true)
		{
			ubyte chr = s[idx];
			if (chr == '.' && isFloat)
			{
				if (fraction)
					return createTokenAndAdvance(TokenType,idx,s[0..idx]);
				if (exponent)
					return createTokenAndAdvance(TokenType,idx,s[0..idx]);
				idx++;
				fraction = true;
				continue;
			}

			switch(chr)
			{
				case '0': .. case '9':
					auto digit = cast(int)(chr-'0');
					static if (base < 10)
					{
						if (digit >= base)
							return createTokenAndAdvance(Type.Error,idx,format("Invalid digit %s in base %s NumericLiteral",cast(char)chr,base));
					}
					idx++;
					expectingNumbers = false;
					break;
				case 'A': .. case 'F':
				case 'a': .. case 'f':
					expectingNumbers = false;
					static if (base < 11)
						goto default;
					else
					{
						auto digit = chr >= 'A' ? cast(int)(chr-'A') : cast(int)(chr-'a');
						static if (base != 16)
						{
							if (digit >= base)
								return createTokenAndAdvance(Type.Error,idx,format("Invalid digit %s in base %s NumericLiteral",cast(char)chr,base));
						}
						idx++;
						break;
					}
				default:
					if (chr == 0)
					{
						if (expectingNumbers)
							return createTokenAndAdvance(Type.Error,idx,format("Invalid eof in base %s literal",base));
						return createTokenAndAdvance(TokenType,idx,s[0..idx]);
					}
					static if (base == 10)
					{
						if (chr == 'e' || chr == 'E')
						{
							if (exponent)
								return createTokenAndAdvance(Type.Error,idx,format("Already processed exponent part"));
							exponent = true;
							idx++;
							chr = s[idx];
							if (chr == '+' || chr == '-')
								idx++;
							expectingNumbers = true;
							continue;
						}
					}
					auto len = s.getTailIdentifierLength(idx);
					if (len == 0)
						return createTokenAndAdvance(TokenType,idx,s[0..idx]);
					// if it is whitespace we are done, else we need to advance the idx with unicodeLength and
					return createTokenAndAdvance(Type.Error,idx,format("Invalid char (%s) in NumericLiteral",cast(char)chr));
			}
		}
		assert(0);
	}
	Token lexBinaryLiteral()
	{
		return lexBaseLiteral!(2,Type.BinaryLiteral)();
	}
	Token lexOctalLiteral()
	{
		return lexBaseLiteral!(8,Type.OctalLiteral)();
	}
	Token lexDecimalLiteral()
	{
		return lexBaseLiteral!(10,Type.DecimalLiteral)(true);
	}
	Token lexHexLiteral()
	{
		return lexBaseLiteral!(16,Type.HexLiteral)();
	}
	Token lexTemplateLiteral(Type t = Type.Template)
	{
		import std.array : appender;
		size_t idx = 0;
		while (true)
		{
			switch (s[idx])
			{
				case '$':
					if (s[idx+1] == '{')
					{
						pushState(State.LexingTemplateLiteral);
						return createTokenAndAdvance(t == Type.TemplateTail ? Type.TemplateMiddle : Type.TemplateHead,idx+2,s[0..idx]);
					}
					idx++;
					break;
				case 0x60:
					return createTokenAndAdvance(t,idx+1,s[0..idx]);
				case 0:
					goto eof;
				case '\\':
					idx++;
					if (s[idx] == 0)
						goto eof;
					idx += s.getUnicodeLength(idx);
					break;
				default:
					auto len = s.getLineTerminatorLength(idx);
					if (len == 0)
						idx += s.getUnicodeLength(idx);
					else {
						line += 1;
						column = 0;
						tokenLength = 0;
						idx += len;
					}
			}
		}
		eof: return Token(Type.Error,"Found eof before finishing lexing TemplateLiteral");
	}
	auto empty()
	{
		return token.type == Type.EndOfFile;
	}
	bool lookAhead(Type t)
	{
		auto l = this.save();
		while (1)
		{
			auto token = l.scanToken();
			if (token.type == t)
				return true;
			switch(token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					continue;
				default:
					return false;
			}
		}
	}
	bool lookAheadForAny(Ts...)()
	{
		auto l = this.save();
		while (1)
		{
			auto token = l.scanToken();
			switch(token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					continue;
				foreach(t; Ts) { case t:
						return true;
				}
				default:
					return false;
			}
		}
	}
	Token lexMultiLineComment() @trusted
	{
		size_t idx = 0;
		for(;;)
		{
			// TODO: this doesn't work very well with the new lines that aren't being counted and the cases where we have /********* bla *********/
			// there are probably substring sse42 matches that we can run on `*/`. that still leaves the unaccounted newlines though
			//version (iasm64NotWindows)
			//{
			//	if (haveSSE42)
			//	{
			//		immutable ulong i = skip!(false, '*', '\x00')(s.ptr + idx);
			//		idx += i;
			//		tokenLength += i;
			//		if (i == 16)
			//			continue;
			//	}
			//}
			ubyte chr = s[idx];
			if (chr == '*')
			{
				column++;
				idx++;
				chr = s[idx];
				if (chr == '/')
				{
					column++;
					return createTokenAndAdvance(Type.MultiLineComment, idx+1, s[0..idx-1]);
				}
				continue;
			} else if (chr == 0)
				goto eof;
			// TODO: we can do better here by determining line terminator and unicode length in one (there are also other places)
			auto len = s.getLineTerminatorLength(idx);
			if (len == 0)
			{
				len = s.getUnicodeLength(idx);
				idx += len;
				column++;
			} else {
				idx += len;
				line++;
				column=0;
			}
		}
		eof:
		return createTokenAndAdvance(Type.Error,idx,"Expected end of MultiLineComment before eof");
	}
	Token lexSingleLineComment() @trusted
	{
		size_t idx = 0;
		start:
		version (iasm64NotWindows)
		{
			//if (haveSSE42)
			//{
				// NOTE: if whole comment is full with unicode stuff we waste alot of time calling the expensive sse4.2 instruction
				immutable ulong i = skip!(false, '\n', '\r', '\xE2', '\x00')(s.ptr + idx);
				idx += i;
				tokenLength += i;
				if (i == 16)
					goto start;
			//}
		}
		auto len = s.getLineTerminatorLength(idx);
		if (len == 0 && s[idx] != 0)
		{
			len = s.getUnicodeLength(idx);
			idx += len;
			column ++;
		} else
		{
			auto tok = Token(Type.SingleLineComment,s[0..idx]);
			idx += len;
			line += 1;
			column = 0;
			s = s[idx..$];
			return tok;
		}
		goto start;
	}
	Token lexSheBang() @trusted
	{
		assert(s[0] == '#');
		assert(s[1] == '!');
		size_t idx = 0;
		start:
		version (iasm64NotWindows)
		{
			//if (haveSSE42)
			//{
				// NOTE: if whole she-bang is full with unicode stuff we waste alot of time calling the expensive sse4.2 instruction
				immutable ulong i = skip!(false, '\n', '\r', '\xE2', '\x00')(s.ptr + idx);
				idx += i;
				tokenLength += i;
				if (i == 16)
					goto start;
			//}
		}
		auto len = s.getLineTerminatorLength(idx);
		if (len == 0 && s[idx] != 0)
		{
			len = s.getUnicodeLength(idx);
			idx += len;
			column ++;
		} else
		{
			auto tok = Token(Type.SheBang,s[0..idx]);
			idx += len;
			line += 1;
			column = 0;
			s = s[idx..$];
			return tok;
		}
		goto start;
	}
	Token lexToken(Goal goal = Goal.All)
	{
		popWhitespace();
		auto chr = s.front();

		switch(chr)
		{
			case 0x0a:
			case 0x0d:
				line ++;
				column = 0;
				s.popFront();
				if (chr == 0x0d)
					s.popNextIf(0x0a);
				return Token(Type.LineTerminator);
			case '{': tokenLength++; s.popFront(); return Token(Type.OpenCurlyBrace);
			case '}': 
				tokenLength++;
				if (lexState == State.LexingTemplateLiteral)
				{
					popState();
					s.popFront();
					return lexTemplateLiteral(Type.TemplateTail);
				}
				s.popFront(); return Token(Type.CloseCurlyBrace);
			case '(': tokenLength++; s.popFront(); return Token(Type.OpenParenthesis);
			case ')': tokenLength++; s.popFront(); return Token(Type.CloseParenthesis);
			case '[': tokenLength++; s.popFront(); return Token(Type.OpenSquareBrackets);
			case ']': tokenLength++; s.popFront(); return Token(Type.CloseSquareBrackets);
			case '#': if (s[1] == '!') return lexSheBang();
				goto default;
			case '.': 
				if (s[1] >= '0' && s[1] <= '9')
					return lexDecimalLiteral();
				tokenLength++;
				s.popFront();
				if (s.popNextIf('.'))
				{
					if (s.popNextIf('.'))
						return Token(Type.SpreadOperator);
					return Token(Type.Error,"One dot too few or too less");
				}
				return Token(Type.Dot);
			case ';': tokenLength++; s.popFront(); return Token(Type.Semicolon);
			case ',': tokenLength++; s.popFront(); return Token(Type.Comma);
			case '<':
				// continue work
				tokenLength++;
				s.popFront();
				if (s.popNextIf('<'))
				{
					tokenLength++;
					if (s.popNextIf('='))
					{
						tokenLength++;
						return Token(Type.LeftShiftAssignment);
					}
					return Token(Type.LeftShift);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.LessOrEqual);
				}
				return Token(Type.LessThan);
			case '>':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('>'))
				{
					tokenLength++;
					if (s.popNextIf('>'))
					{
						tokenLength++;
						if (s.popNextIf('='))
						{
							tokenLength++;
							return Token(Type.TripleRightShiftAssignment);
						}
						return Token(Type.TripleRightSift);
					}
					if (s.popNextIf('='))
					{
						tokenLength++;
						return Token(Type.RightShiftAssignment);
					}
					return Token(Type.RightShift);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.GreaterOrEqual);
				}
				return Token(Type.GreaterThan);
			case '=':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('='))
				{
					tokenLength++;
					if (s.popNextIf('='))
					{
						tokenLength++;
						return Token(Type.StrictEqual);
					}
					return Token(Type.Equal);
				}
				if (s.popNextIf('>'))
				{
					tokenLength++;
					return Token(Type.Arrow);
				}
				return Token(Type.Assignment);
			case '!':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('='))
				{
					tokenLength++;
					if (s.popNextIf('='))
					{
						tokenLength++;
						return Token(Type.StrictNotEqual);
					}
					return Token(Type.NotEqual);
				}
				return Token(Type.Negation);
			case '+':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('+'))
				{
					tokenLength++;
					return Token(Type.Increment);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.AdditiveAssignment);
				}
				return Token(Type.Add);
			case '-':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('-'))
				{
					tokenLength++;
					return Token(Type.Decrement);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.DecrementalAssignment);
				}
				return Token(Type.Minus);
			case '*':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.MultiplicativeAssignment);
				}
				return Token(Type.Multiply);
			case '%':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.ModAssignment);
				}
				return Token(Type.Mod);
			case '&':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('&'))
				{
					tokenLength++;
					return Token(Type.LogicalAnd);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.BitwiseAndAssignment);
				}
				return Token(Type.BitwiseAnd);
			case '|':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('|'))
				{
					tokenLength++;
					return Token(Type.LogicalOr);
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.BitwiseOrAssignment);
				}
				return Token(Type.BitwiseOr);
			case '^':
				tokenLength++;
				s.popFront();
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.BitwiseXorAssignment);
				}
				return Token(Type.BitwiseXor);
			case '~':
				tokenLength++;
				s.popFront();
				return Token(Type.Tilde);
			case '?':
				tokenLength++;
				s.popFront();
				return Token(Type.QuestionMark);
			case ':':
				tokenLength++;
				s.popFront();
				return Token(Type.Colon);
			case '/':
				if (goal != Goal.ProhibitRegex)
				{
					auto len = lookAheadRegex;
					if (len > 0)
						return createTokenAndAdvance(Type.Regex,len,s[0..len]);
				}
				tokenLength++;
				s.popFront();
				if (s.popNextIf('/'))
				{
					tokenLength++;
					return lexSingleLineComment();
				}
				if (s.popNextIf('='))
				{
					tokenLength++;
					return Token(Type.DivisionAssignment);
				}
				if (s.popNextIf('*'))
				{
					tokenLength++;
					return lexMultiLineComment();
				}
				return Token(Type.Division);
			case '"':
			case '\'':
				return lexString();
			case '0':
				auto lookahead = s.save();
				lookahead.popFront();
				if (!lookahead.empty && lookahead.front() == '.')
					return lexDecimalLiteral();
				tokenLength++;
				s.popFront();
				if (s.popNextIf('b') || s.popNextIf('B'))
				{
					tokenLength++;
					return lexBinaryLiteral;
				}
				if (s.popNextIf('o') || s.popNextIf('O'))
				{
					tokenLength++;
					return lexOctalLiteral;
				}
				if (s.popNextIf('x') || s.popNextIf('X'))
				{
					tokenLength++;
					return lexHexLiteral;
				}
				if (s.front() < '0' || s.front() > '9')
					return Token(Type.DecimalLiteral,"0");
				return Token(Type.Error,"Not expecting NumericLiteral to start with 0");
			case '1': .. case '9':
				return lexDecimalLiteral;
			case '`':
				tokenLength++;
				s.popFront();
				return lexTemplateLiteral();
			case 0:
				return Token(Type.EndOfFile);
			default:
				break;
		}
		auto len = s.getLineTerminatorUnicodeLength();
		if (len != 0)
		{
			line ++;
			column = 0;
			s = s[len..$];
			return Token(Type.LineTerminator);
		}		
		return lexIdentifier();
	}
}
auto createLexer(const ubyte[] s)
{
	auto input = s ~ cast(const(ubyte)[])[0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0,0x0];
	return Lexer(input);
}
auto createLexer(string s)
{
	return createLexer(cast(const(ubyte)[])s);
}
@("lineterminators")
unittest
{
	auto lexer = createLexer("\n\r\n\u2028");
	lexer.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
}
@("lexIdentifier")
unittest
{
	// TODO: need to test unicode escape sequence stuff
	auto lexer = createLexer("abcde");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"abcde"));
	lexer = createLexer("π");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"π"));
}
@("popWhitespace")
unittest
{
	auto lexer = createLexer("  \u0009abc");
	lexer.popWhitespace();
	lexer.column.shouldEqual(3);
	lexer = createLexer("  ");
	lexer.popWhitespace();
	lexer.column.shouldEqual(2);
}
@("lookAheadRegex")
unittest
{
	auto lexer = createLexer("/abcd/");
	lexer.lookAheadRegex.shouldEqual(6);
	lexer = createLexer(`/ab\/ab/`);
	lexer.lookAheadRegex.shouldEqual(8);
	lexer = createLexer(`/also no
		regex`);
	lexer.scanToken.shouldEqual(Token(Type.Division));
	lexer.empty.shouldBeFalse;
	lexer.column.shouldEqual(0);
	lexer = createLexer(`/regex with modifiers/gi;`);
	lexer.lookAheadRegex.shouldEqual(24);
	lexer = createLexer("/ab[^/]cd/");
	lexer.lookAheadRegex.shouldEqual(10);
	lexer = createLexer(`/ab[^/\\]cd/`);
	lexer.lookAheadRegex.shouldEqual(12);
	lexer = createLexer(`/^\/(.+)\/([a-z]*)$/`);
	lexer.lookAheadRegex.shouldEqual(20);
}
@("lexString")
unittest
{
	auto lexer = createLexer(`"a string"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"a string"));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`"a stringin'"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"a stringin\\'"));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`'another string'`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"another string"));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`"escaped \"string\""`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`escaped "string"`));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`'escaped \'string\''`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`escaped \'string\'`));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`"\xaa\xb5"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`\xaa\xb5`));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`'\\';`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"\\\\"));
	lexer.scanToken().shouldEqual(Token(Type.Semicolon));
	lexer = createLexer(`"\\";`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"\\\\"));
	lexer.scanToken().shouldEqual(Token(Type.Semicolon));
}
@("lexBinaryLiteral")
unittest
{
	auto lexer = createLexer("010101");
	lexer.lexBinaryLiteral().shouldEqual(Token(Type.BinaryLiteral,"010101"));
	lexer = createLexer("010102");
	lexer.lexBinaryLiteral().shouldEqual(Token(Type.Error,"Invalid digit 2 in base 2 NumericLiteral"));
	lexer = createLexer("01010a");
	lexer.lexBinaryLiteral().shouldEqual(Token(Type.Error,"Invalid char (a) in NumericLiteral"));
}
@("lexOctalLiteral")
unittest
{
	auto lexer = createLexer("6027");
	lexer.lexOctalLiteral().shouldEqual(Token(Type.OctalLiteral,"6027"));
	lexer = createLexer("9");
	lexer.lexOctalLiteral().shouldEqual(Token(Type.Error,"Invalid digit 9 in base 8 NumericLiteral"));
	lexer = createLexer("6027a");
	lexer.lexOctalLiteral().shouldEqual(Token(Type.Error,"Invalid char (a) in NumericLiteral"));
}
@("lexDecimalLiteral")
unittest
{
	auto lexer = createLexer("60279");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279"));
	lexer = createLexer("60279e12");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279e12"));
	lexer = createLexer("60279.e12");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279.e12"));
	lexer = createLexer("6027a");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.Error,"Invalid char (a) in NumericLiteral"));
	lexer = createLexer("60279.01234");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279.01234"));
	lexer = createLexer("60279.01234e12");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279.01234e12"));
	lexer = createLexer("60279.01234e+12");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279.01234e+12"));
	lexer = createLexer("60279.01234e-12");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"60279.01234e-12"));
	lexer = createLexer("0.1");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"0.1"));
	lexer = createLexer(".5");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,".5"));
	lexer = createLexer("1..toFixed");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"1."));
	lexer = createLexer("15 ");
	lexer.lexDecimalLiteral().shouldEqual(Token(Type.DecimalLiteral,"15"));
	lexer = createLexer("0123");
	lexer.scanToken().shouldEqual(Token(Type.Error,"Not expecting NumericLiteral to start with 0"));
}
@("lexHexLiteral")
unittest
{
	auto lexer = createLexer("04af");
	lexer.lexHexLiteral().shouldEqual(Token(Type.HexLiteral,"04af"));
	lexer = createLexer("04afg");
	lexer.lexHexLiteral().shouldEqual(Token(Type.Error,"Invalid char (g) in NumericLiteral"));
}
auto tokens(ref Lexer lexer)
{
	struct Range
	{
		private {
			Lexer lexer;
			Token tok;
		}
		this(ref Lexer lexer)
		{
			this.lexer = lexer;
			tok = this.lexer.lexToken;
		}
		Token front()
		{
			return tok;
		}
		bool empty()
		{
			return tok.type == Type.EndOfFile;
		}
		void popFront()
		{
			tok = lexer.lexToken;
		}
	}
	return Range(lexer);
}
@("lexTemplateLiteral")
unittest
{
	auto lexer = createLexer("some\n$template\\\u0060\nstring\u0060");
	lexer.lexTemplateLiteral.shouldEqual(Token(Type.Template,"some\n$template\\\u0060\nstring"));
	lexer = createLexer("some\n$template\\\u0060\nstring${identifier}\u0060");
	lexer.lexTemplateLiteral.shouldEqual(Token(Type.TemplateHead,"some\n$template\\\u0060\nstring"));
	lexer.s[0..13].shouldEqual("identifier}\u0060\u0000");

	lexer = createLexer("\u0060some\n$template\\\u0060\nstring${identifier}\u0060");
	lexer.lexToken().shouldEqual(Token(Type.TemplateHead,"some\n$template\\\u0060\nstring"));
	lexer.lexToken().shouldEqual(Token(Type.Identifier,"identifier"));
	lexer.lexToken().shouldEqual(Token(Type.TemplateTail,""));

	lexer = createLexer("`${double(2)} != null ? ${double(2)} : {}`");
	assert(lexer.tokens.equal([
		Token(Type.TemplateHead),
		Token(Type.Identifier,"double"),
		Token(Type.OpenParenthesis),
		Token(Type.DecimalLiteral,"2"),
		Token(Type.CloseParenthesis),
		Token(Type.TemplateMiddle," != null ? "),
		Token(Type.Identifier,"double"),
		Token(Type.OpenParenthesis),
		Token(Type.DecimalLiteral,"2"),
		Token(Type.CloseParenthesis),
		Token(Type.TemplateTail," : {}")
	]));
}
@("comments")
unittest
{
	auto lexer = createLexer("// comment \nidentifier");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment," comment "));
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"identifier"));

	lexer = createLexer("// comment");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment," comment"));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("//");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));
	
	lexer = createLexer("//\r\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));
	
	lexer = createLexer("//\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("//01234567890123456789\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,"01234567890123456789"));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("//\u2022\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,"•"));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/* multi \n line \r\n\n comment */ identifier");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment," multi \n line \r\n\n comment "));
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"identifier"));

	lexer = createLexer("/**/");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment,""));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/*a*b*/");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment,"a*b"));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/*");
	lexer.lexToken.shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexer.s[0].shouldEqual('\u0000');
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("foobar(abc, /** bla **/, def) /** hup */;");
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"foobar"));
	lexer.lexToken();
	lexer.lexToken();
	lexer.lexToken();
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment,"* bla *"));
}
@("save")
unittest
{
	auto lexer = createLexer("{set\n/* comment */,get}");
	lexer.scanToken().shouldEqual(Token(Type.OpenCurlyBrace));
	lexer.column.shouldEqual(0);
	lexer.scanToken().shouldEqual(Token(Type.Identifier,"set"));
	auto lexer2 = lexer.save();
	lexer2.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer2.scanToken().shouldEqual(Token(Type.MultiLineComment," comment "));
	lexer2.scanToken().shouldEqual(Token(Type.Comma));
	lexer2.column.shouldEqual(13);
	lexer.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer.scanToken().shouldEqual(Token(Type.MultiLineComment," comment "));
	lexer.scanToken().shouldEqual(Token(Type.Comma));
	lexer.column.shouldEqual(13);
}
@("lookAhead")
unittest
{
	auto lexer = createLexer("set\n/* comment */,");
	lexer.scanToken().shouldEqual(Token(Type.Identifier,"set"));
	lexer.lookAhead(Type.Comma).shouldBeTrue();
	lexer.lookAhead(Type.Multiply).shouldBeFalse();
}
@("lookAheadForAny")
unittest
{
	auto lexer = createLexer("set\n/* comment */,");
	lexer.scanToken().shouldEqual(Token(Type.Identifier,"set"));
	lexer.lookAheadForAny!(Type.Comma).shouldBeTrue();
	lexer.lookAheadForAny!(Type.Multiply).shouldBeFalse();
	lexer.lookAheadForAny!(Type.Multiply,Type.Comma).shouldBeTrue();
}
@("degenerate")
unittest
{
	auto lexer = createLexer(".");
	lexer.scanToken().shouldEqual(Token(Type.Dot));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
}
@("keywords")
unittest
{
	assert("await" in Keywords);
	isReservedKeyword(cast(const(ubyte)[])"await").shouldBeFalse;
	isReservedKeyword(cast(const(ubyte)[])"default").shouldBeTrue;
	isReservedKeyword(cast(const(ubyte)[])"default2").shouldBeFalse;
	isReservedKeyword(cast(const(ubyte)[])"yield").shouldBeTrue;
	isReservedKeyword(cast(const(ubyte)[])"").shouldBeFalse;
	isReservedKeyword(cast(const(ubyte)[])"a").shouldBeFalse;
}
@("scanToken")
unittest
{
	auto lexer = createLexer(".5");
	lexer.scanToken().shouldEqual(Token(Type.DecimalLiteral,".5"));
	lexer = createLexer("..");
	lexer.scanToken().shouldEqual(Token(Type.Error,"One dot too few or too less"));
}