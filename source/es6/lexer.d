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

import es6.bench;
public import es6.tokens;
import std.range : empty, front, popFront, save, take, drop, ElementType;
import std.array : Appender, appender;
import std.traits : Unqual;
import std.typecons : tuple;
import core.cpuid : sse42;
import core.simd;

//version = tracing;
//version = chatty;
version(LDC)
{
	static if (__traits(targetHasFeature, "sse4.2"))
	{
		import core.simd;
		import ldc.simd;
		import ldc.gccbuiltins_x86;
		pragma(msg, "Info: inline SSE4.2 instructions are used.");
		version = ldcsse42;
	}
	else
	{
		pragma(msg, "Info: inline SSE4.2 instructions are not used.");
	}
}
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
version (tracing)
{
	import std.stdio;
}
enum InvalidUTF8 = size_t.max;

enum State
{
	TokensRemaining,
	EndOfFile,
	LexingTemplateLiteral
}
struct LexState
{
	State state;
	size_t openCurlyCount; // this is used for template literal parsing
}

enum Goal
{
	None = 0,
	In = 1,
	Yield = 1 << 1,
	Return = 1 << 2,
	Default = 1 << 3,
	NoRegex = 1 << 4,
	NoEmptyParen = 1 << 5
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
size_t getWhiteSpaceLength(Range)(Range r, size_t idx = 0) nothrow
{
	if (r[idx] < 0x80) // 1 byte code point
	{
		return (r[idx] == 0x09 || r[idx] == 0x0B || r[idx] == 0x0C || r[idx] == 0x20) ? 1 : 0;
	} else if (r[idx] < 0xc0)
	{
		return 0;
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
size_t getLineTerminatorLength(Range)(Range r, size_t idx = 0) pure nothrow
{
	if (r[idx] < 0x80)
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
	if (r[0] < 0xe0 || r[0] >= 0xf0)
		return 0;
	auto hex = r.decodeUnicodeCodePoint!3;	// 3 byte unicode
	return (hex == 0x2028 || hex == 0x2029) ? 3 : 0;
}
bool isLineTerminator(size_t cp)
{
	if (cp <= 0x7f)
	{
		return cp == 0x0d || cp == 0x0a;
	}
	return cp == 0x2028 || cp == 0x2029;
}
ulong decodeCodePoint(Range)(Range r, size_t idx = 0) pure nothrow
{
	if (r[idx] <= 0x7f)
		return r[idx];
	if (r[idx] >= 0xf0) // 4 byte unicode
	{
		if (r[idx] >= 0xf7)
			return InvalidUTF8;
		return r.decodeUnicodeCodePoint!4(idx);	// 4 byte unicode
	} else if (r[idx] >= 0xe0)
	{
		return r.decodeUnicodeCodePoint!3(idx);	// 3 byte unicode
	} else if (r[idx] >= 0xc0)
	{
		return r.decodeUnicodeCodePoint!2(idx);	// 2 byte unicode
	}
	return InvalidUTF8;
}
size_t getCodePointLength(ulong cp) pure nothrow @nogc
{
	if (cp >= 0xf0)
		return 4;
	if (cp >= 0xe0)
		return 3;
	if (cp >= 0xc0)
		return 2;
	return 1;
}
size_t getCodePointEncodedLength(ulong cp) pure nothrow @nogc
{
	if (cp >= 0x10000)
		return 4;
	if (cp >= 0x800)
		return 3;
	if (cp >= 0x80)
		return 2;
	return 1;
}
@("isLineTerminator")
unittest
{
	isLineTerminator(0x0d).shouldBeTrue;
	isLineTerminator(0x0a).shouldBeTrue;
	isLineTerminator(0x2028).shouldBeTrue;
	isLineTerminator(0x2029).shouldBeTrue;
	isLineTerminator(0x400).shouldBeFalse;
}
auto getRegexLength(Range)(Range r, ref size_t tokenLength) pure nothrow
{
	size_t idx = 1;
	bool inOneOfExpr = false;
	if (r[idx] == '*' || r[idx] == '/')
		return 0;
	tokenLength = 1;
	while (true)
	{
		switch (r[idx])
		{
			case '[':
				inOneOfExpr = true;
				tokenLength++;
				idx++;
				break;
			case ']':
				inOneOfExpr = false;
				tokenLength++;
				idx++;
				break;
			case '\\':
				tokenLength++;
				idx++;
				auto len = r.getLineTerminatorLength(idx);
				if (len != 0)
					return 0;
				if (r[idx] == 0)
					return 0;
				idx += r[idx..$].getUnicodeLength();
				tokenLength++;
				break;
			case '/':
				tokenLength++;
				idx++;
				if (!inOneOfExpr)
				{
					while (true)
					{
						auto len = r.getTailIdentifierLength(idx);
						if (len == 0)
							return idx;
						tokenLength++;
						idx += len;
					}
				}
				break;
			default:
				tokenLength++;
				auto len = r.getLineTerminatorLength(idx);
				if (len == InvalidUTF8)
					return InvalidUTF8;
				if (len != 0)
					return 0;
				if (r[idx] == 0)
					return 0;
				len = r.getUnicodeLength(idx);
				if (len == InvalidUTF8)
					return InvalidUTF8;
				idx += len;
				break;
		}
	}
}

bool isAmongRangeList(size_t cp, const int[] list) nothrow pure
{
	size_t len = list.length / 2;
	size_t idx = len / 2;
	size_t offset = 0;
	while (true)
	{
		if (list[idx*2] > cp)
		{
			if (len == 1)
				return false;
			len /= 2;
			idx = offset + (len / 2);
		} else if (list[idx*2] < cp)
		{
			if (list[idx*2+1] >= cp)
				return true;
			if (len == 1)
				return false;
			offset += (len / 2) + 1;
			len = (len - 1) / 2;
			idx = offset + (len / 2);
			if (len == 0)
				return false;
		} else
			return true;
	}
}

bool isValid2ByteIdentifierStart(size_t cp) nothrow pure
{
	static immutable int[] cp2b = [0xaa,0xaa,0xb5,0xb5,0xba,0xba,0xc0,0xd6,0xd8,0xf6,0xf8,0x2c1,0x2c6,0x2d1,0x2e0,0x2e4,0x2ec,0x2ec,0x2ee,0x2ee,0x370,0x374,0x376,0x377,0x37a,0x37d,0x37f,0x37f,0x386,0x386,0x388,0x38a,0x38c,0x38c,0x38e,0x3a1,0x3a3,0x3f5,0x3f7,0x481,0x48a,0x52f,0x531,0x556,0x559,0x559,0x561,0x587,0x5d0,0x5ea,0x5f0,0x5f2,0x620,0x64a,0x66e,0x66f,0x671,0x6d3,0x6d5,0x6d5,0x6e5,0x6e6,0x6ee,0x6ef,0x6fa,0x6fc,0x6ff,0x6ff,0x710,0x710,0x712,0x72f,0x74d,0x7a5,0x7b1,0x7b1,0x7ca,0x7ea,0x7f4,0x7f5,0x7fa,0x7fa];
	return isAmongRangeList(cp, cp2b);
}
bool isValid3ByteIdentifierStart(size_t cp) nothrow pure
{
	static immutable int[] cp3b = [0x800,0x815,0x81a,0x81a,0x824,0x824,0x828,0x828,0x840,0x858,0x8a0,0x8b4,0x904,0x939,0x93d,0x93d,0x950,0x950,0x958,0x961,0x971,0x980,0x985,0x98c,0x98f,0x990,0x993,0x9a8,0x9aa,0x9b0,0x9b2,0x9b2,0x9b6,0x9b9,0x9bd,0x9bd,0x9ce,0x9ce,0x9dc,0x9dd,0x9df,0x9e1,0x9f0,0x9f1,0xa05,0xa0a,0xa0f,0xa10,0xa13,0xa28,0xa2a,0xa30,0xa32,0xa33,0xa35,0xa36,0xa38,0xa39,0xa59,0xa5c,0xa5e,0xa5e,0xa72,0xa74,0xa85,0xa8d,0xa8f,0xa91,0xa93,0xaa8,0xaaa,0xab0,0xab2,0xab3,0xab5,0xab9,0xabd,0xabd,0xad0,0xad0,0xae0,0xae1,0xaf9,0xaf9,0xb05,0xb0c,0xb0f,0xb10,0xb13,0xb28,0xb2a,0xb30,0xb32,0xb33,0xb35,0xb39,0xb3d,0xb3d,0xb5c,0xb5d,0xb5f,0xb61,0xb71,0xb71,0xb83,0xb83,0xb85,0xb8a,0xb8e,0xb90,0xb92,0xb95,0xb99,0xb9a,0xb9c,0xb9c,0xb9e,0xb9f,0xba3,0xba4,0xba8,0xbaa,0xbae,0xbb9,0xbd0,0xbd0,0xc05,0xc0c,0xc0e,0xc10,0xc12,0xc28,0xc2a,0xc39,0xc3d,0xc3d,0xc58,0xc5a,0xc60,0xc61,0xc85,0xc8c,0xc8e,0xc90,0xc92,0xca8,0xcaa,0xcb3,0xcb5,0xcb9,0xcbd,0xcbd,0xcde,0xcde,0xce0,0xce1,0xcf1,0xcf2,0xd05,0xd0c,0xd0e,0xd10,0xd12,0xd3a,0xd3d,0xd3d,0xd4e,0xd4e,0xd5f,0xd61,0xd7a,0xd7f,0xd85,0xd96,0xd9a,0xdb1,0xdb3,0xdbb,0xdbd,0xdbd,0xdc0,0xdc6,0xe01,0xe30,0xe32,0xe33,0xe40,0xe46,0xe81,0xe82,0xe84,0xe84,0xe87,0xe88,0xe8a,0xe8a,0xe8d,0xe8d,0xe94,0xe97,0xe99,0xe9f,0xea1,0xea3,0xea5,0xea5,0xea7,0xea7,0xeaa,0xeab,0xead,0xeb0,0xeb2,0xeb3,0xebd,0xebd,0xec0,0xec4,0xec6,0xec6,0xedc,0xedf,0xf00,0xf00,0xf40,0xf47,0xf49,0xf6c,0xf88,0xf8c,0x1000,0x102a,0x103f,0x103f,0x1050,0x1055,0x105a,0x105d,0x1061,0x1061,0x1065,0x1066,0x106e,0x1070,0x1075,0x1081,0x108e,0x108e,0x10a0,0x10c5,0x10c7,0x10c7,0x10cd,0x10cd,0x10d0,0x10fa,0x10fc,0x1248,0x124a,0x124d,0x1250,0x1256,0x1258,0x1258,0x125a,0x125d,0x1260,0x1288,0x128a,0x128d,0x1290,0x12b0,0x12b2,0x12b5,0x12b8,0x12be,0x12c0,0x12c0,0x12c2,0x12c5,0x12c8,0x12d6,0x12d8,0x1310,0x1312,0x1315,0x1318,0x135a,0x1380,0x138f,0x13a0,0x13f5,0x13f8,0x13fd,0x1401,0x166c,0x166f,0x167f,0x1681,0x169a,0x16a0,0x16ea,0x16ee,0x16f8,0x1700,0x170c,0x170e,0x1711,0x1720,0x1731,0x1740,0x1751,0x1760,0x176c,0x176e,0x1770,0x1780,0x17b3,0x17d7,0x17d7,0x17dc,0x17dc,0x1820,0x1877,0x1880,0x18a8,0x18aa,0x18aa,0x18b0,0x18f5,0x1900,0x191e,0x1950,0x196d,0x1970,0x1974,0x1980,0x19ab,0x19b0,0x19c9,0x1a00,0x1a16,0x1a20,0x1a54,0x1aa7,0x1aa7,0x1b05,0x1b33,0x1b45,0x1b4b,0x1b83,0x1ba0,0x1bae,0x1baf,0x1bba,0x1be5,0x1c00,0x1c23,0x1c4d,0x1c4f,0x1c5a,0x1c7d,0x1ce9,0x1cec,0x1cee,0x1cf1,0x1cf5,0x1cf6,0x1d00,0x1dbf,0x1e00,0x1f15,0x1f18,0x1f1d,0x1f20,0x1f45,0x1f48,0x1f4d,0x1f50,0x1f57,0x1f59,0x1f59,0x1f5b,0x1f5b,0x1f5d,0x1f5d,0x1f5f,0x1f7d,0x1f80,0x1fb4,0x1fb6,0x1fbc,0x1fbe,0x1fbe,0x1fc2,0x1fc4,0x1fc6,0x1fcc,0x1fd0,0x1fd3,0x1fd6,0x1fdb,0x1fe0,0x1fec,0x1ff2,0x1ff4,0x1ff6,0x1ffc,0x2071,0x2071,0x207f,0x207f,0x2090,0x209c,0x2102,0x2102,0x2107,0x2107,0x210a,0x2113,0x2115,0x2115,0x2118,0x211d,0x2124,0x2124,0x2126,0x2126,0x2128,0x2128,0x212a,0x2139,0x213c,0x213f,0x2145,0x2149,0x214e,0x214e,0x2160,0x2188,0x2c00,0x2c2e,0x2c30,0x2c5e,0x2c60,0x2ce4,0x2ceb,0x2cee,0x2cf2,0x2cf3,0x2d00,0x2d25,0x2d27,0x2d27,0x2d2d,0x2d2d,0x2d30,0x2d67,0x2d6f,0x2d6f,0x2d80,0x2d96,0x2da0,0x2da6,0x2da8,0x2dae,0x2db0,0x2db6,0x2db8,0x2dbe,0x2dc0,0x2dc6,0x2dc8,0x2dce,0x2dd0,0x2dd6,0x2dd8,0x2dde,0x3005,0x3007,0x3021,0x3029,0x3031,0x3035,0x3038,0x303c,0x3041,0x3096,0x309b,0x309f,0x30a1,0x30fa,0x30fc,0x30ff,0x3105,0x312d,0x3131,0x318e,0x31a0,0x31ba,0x31f0,0x31ff,0x3400,0x4db5,0x4e00,0x9fd5,0xa000,0xa48c,0xa4d0,0xa4fd,0xa500,0xa60c,0xa610,0xa61f,0xa62a,0xa62b,0xa640,0xa66e,0xa67f,0xa69d,0xa6a0,0xa6ef,0xa717,0xa71f,0xa722,0xa788,0xa78b,0xa7ad,0xa7b0,0xa7b7,0xa7f7,0xa801,0xa803,0xa805,0xa807,0xa80a,0xa80c,0xa822,0xa840,0xa873,0xa882,0xa8b3,0xa8f2,0xa8f7,0xa8fb,0xa8fb,0xa8fd,0xa8fd,0xa90a,0xa925,0xa930,0xa946,0xa960,0xa97c,0xa984,0xa9b2,0xa9cf,0xa9cf,0xa9e0,0xa9e4,0xa9e6,0xa9ef,0xa9fa,0xa9fe,0xaa00,0xaa28,0xaa40,0xaa42,0xaa44,0xaa4b,0xaa60,0xaa76,0xaa7a,0xaa7a,0xaa7e,0xaaaf,0xaab1,0xaab1,0xaab5,0xaab6,0xaab9,0xaabd,0xaac0,0xaac0,0xaac2,0xaac2,0xaadb,0xaadd,0xaae0,0xaaea,0xaaf2,0xaaf4,0xab01,0xab06,0xab09,0xab0e,0xab11,0xab16,0xab20,0xab26,0xab28,0xab2e,0xab30,0xab5a,0xab5c,0xab65,0xab70,0xabe2,0xac00,0xd7a3,0xd7b0,0xd7c6,0xd7cb,0xd7fb,0xf900,0xfa6d,0xfa70,0xfad9,0xfb00,0xfb06,0xfb13,0xfb17,0xfb1d,0xfb1d,0xfb1f,0xfb28,0xfb2a,0xfb36,0xfb38,0xfb3c,0xfb3e,0xfb3e,0xfb40,0xfb41,0xfb43,0xfb44,0xfb46,0xfbb1,0xfbd3,0xfd3d,0xfd50,0xfd8f,0xfd92,0xfdc7,0xfdf0,0xfdfb,0xfe70,0xfe74,0xfe76,0xfefc,0xff21,0xff3a,0xff41,0xff5a,0xff66,0xffbe,0xffc2,0xffc7,0xffca,0xffcf,0xffd2,0xffd7,0xffda,0xffdc];
	return isAmongRangeList(cp, cp3b);
}
bool isValid4ByteIdentifierStart(size_t cp) nothrow pure
{
	static immutable int[] cp4b = [0x10000,0x1000b,0x1000d,0x10026,0x10028,0x1003a,0x1003c,0x1003d,0x1003f,0x1004d,0x10050,0x1005d,0x10080,0x100fa,0x10140,0x10174,0x10280,0x1029c,0x102a0,0x102d0,0x10300,0x1031f,0x10330,0x1034a,0x10350,0x10375,0x10380,0x1039d,0x103a0,0x103c3,0x103c8,0x103cf,0x103d1,0x103d5,0x10400,0x1049d,0x10500,0x10527,0x10530,0x10563,0x10600,0x10736,0x10740,0x10755,0x10760,0x10767,0x10800,0x10805,0x10808,0x10808,0x1080a,0x10835,0x10837,0x10838,0x1083c,0x1083c,0x1083f,0x10855,0x10860,0x10876,0x10880,0x1089e,0x108e0,0x108f2,0x108f4,0x108f5,0x10900,0x10915,0x10920,0x10939,0x10980,0x109b7,0x109be,0x109bf,0x10a00,0x10a00,0x10a10,0x10a13,0x10a15,0x10a17,0x10a19,0x10a33,0x10a60,0x10a7c,0x10a80,0x10a9c,0x10ac0,0x10ac7,0x10ac9,0x10ae4,0x10b00,0x10b35,0x10b40,0x10b55,0x10b60,0x10b72,0x10b80,0x10b91,0x10c00,0x10c48,0x10c80,0x10cb2,0x10cc0,0x10cf2,0x11003,0x11037,0x11083,0x110af,0x110d0,0x110e8,0x11103,0x11126,0x11150,0x11172,0x11176,0x11176,0x11183,0x111b2,0x111c1,0x111c4,0x111da,0x111da,0x111dc,0x111dc,0x11200,0x11211,0x11213,0x1122b,0x11280,0x11286,0x11288,0x11288,0x1128a,0x1128d,0x1128f,0x1129d,0x1129f,0x112a8,0x112b0,0x112de,0x11305,0x1130c,0x1130f,0x11310,0x11313,0x11328,0x1132a,0x11330,0x11332,0x11333,0x11335,0x11339,0x1133d,0x1133d,0x11350,0x11350,0x1135d,0x11361,0x11480,0x114af,0x114c4,0x114c5,0x114c7,0x114c7,0x11580,0x115ae,0x115d8,0x115db,0x11600,0x1162f,0x11644,0x11644,0x11680,0x116aa,0x11700,0x11719,0x118a0,0x118df,0x118ff,0x118ff,0x11ac0,0x11af8,0x12000,0x12399,0x12400,0x1246e,0x12480,0x12543,0x13000,0x1342e,0x14400,0x14646,0x16800,0x16a38,0x16a40,0x16a5e,0x16ad0,0x16aed,0x16b00,0x16b2f,0x16b40,0x16b43,0x16b63,0x16b77,0x16b7d,0x16b8f,0x16f00,0x16f44,0x16f50,0x16f50,0x16f93,0x16f9f,0x1b000,0x1b001,0x1bc00,0x1bc6a,0x1bc70,0x1bc7c,0x1bc80,0x1bc88,0x1bc90,0x1bc99,0x1d400,0x1d454,0x1d456,0x1d49c,0x1d49e,0x1d49f,0x1d4a2,0x1d4a2,0x1d4a5,0x1d4a6,0x1d4a9,0x1d4ac,0x1d4ae,0x1d4b9,0x1d4bb,0x1d4bb,0x1d4bd,0x1d4c3,0x1d4c5,0x1d505,0x1d507,0x1d50a,0x1d50d,0x1d514,0x1d516,0x1d51c,0x1d51e,0x1d539,0x1d53b,0x1d53e,0x1d540,0x1d544,0x1d546,0x1d546,0x1d54a,0x1d550,0x1d552,0x1d6a5,0x1d6a8,0x1d6c0,0x1d6c2,0x1d6da,0x1d6dc,0x1d6fa,0x1d6fc,0x1d714,0x1d716,0x1d734,0x1d736,0x1d74e,0x1d750,0x1d76e,0x1d770,0x1d788,0x1d78a,0x1d7a8,0x1d7aa,0x1d7c2,0x1d7c4,0x1d7cb,0x1e800,0x1e8c4,0x1ee00,0x1ee03,0x1ee05,0x1ee1f,0x1ee21,0x1ee22,0x1ee24,0x1ee24,0x1ee27,0x1ee27,0x1ee29,0x1ee32,0x1ee34,0x1ee37,0x1ee39,0x1ee39,0x1ee3b,0x1ee3b,0x1ee42,0x1ee42,0x1ee47,0x1ee47,0x1ee49,0x1ee49,0x1ee4b,0x1ee4b,0x1ee4d,0x1ee4f,0x1ee51,0x1ee52,0x1ee54,0x1ee54,0x1ee57,0x1ee57,0x1ee59,0x1ee59,0x1ee5b,0x1ee5b,0x1ee5d,0x1ee5d,0x1ee5f,0x1ee5f,0x1ee61,0x1ee62,0x1ee64,0x1ee64,0x1ee67,0x1ee6a,0x1ee6c,0x1ee72,0x1ee74,0x1ee77,0x1ee79,0x1ee7c,0x1ee7e,0x1ee7e,0x1ee80,0x1ee89,0x1ee8b,0x1ee9b,0x1eea1,0x1eea3,0x1eea5,0x1eea9,0x1eeab,0x1eebb,0x20000,0x2a6d6,0x2a700,0x2b734,0x2b740,0x2b81d,0x2b820,0x2cea1,0x2f800,0x2fa1d];
	return isAmongRangeList(cp, cp4b);
}
bool isValid2ByteIdentifierTail(size_t cp) nothrow pure
{
	static immutable int[] cp2b = [0x0aa,0x0aa,0x0b5,0x0b5,0x0b7,0x0b7,0x0ba,0x0ba,0x0c0,0x0d6,0x0d8,0x0f6,0x0f8,0x2c1,0x2c6,0x2d1,0x2e0,0x2e4,0x2ec,0x2ec,0x2ee,0x2ee,0x300,0x374,0x376,0x377,0x37a,0x37d,0x37f,0x37f,0x386,0x38a,0x38c,0x38c,0x38e,0x3a1,0x3a3,0x3f5,0x3f7,0x481,0x483,0x487,0x48a,0x52f,0x531,0x556,0x559,0x559,0x561,0x587,0x591,0x5bd,0x5bf,0x5bf,0x5c1,0x5c2,0x5c4,0x5c5,0x5c7,0x5c7,0x5d0,0x5ea,0x5f0,0x5f2,0x610,0x61a,0x620,0x669,0x66e,0x6d3,0x6d5,0x6dc,0x6df,0x6e8,0x6ea,0x6fc,0x6ff,0x6ff,0x710,0x74a,0x74d,0x7b1,0x7c0,0x7f5,0x7fa,0x7fa];
	return isAmongRangeList(cp, cp2b);
}
bool isValid3ByteIdentifierTail(size_t cp) nothrow pure
{
	static immutable int[] cp3b = [0x800,0x82d,0x840,0x85b,0x8a0,0x8b4,0x8e3,0x963,0x966,0x96f,0x971,0x983,0x985,0x98c,0x98f,0x990,0x993,0x9a8,0x9aa,0x9b0,0x9b2,0x9b2,0x9b6,0x9b9,0x9bc,0x9c4,0x9c7,0x9c8,0x9cb,0x9ce,0x9d7,0x9d7,0x9dc,0x9dd,0x9df,0x9e3,0x9e6,0x9f1,0xa01,0xa03,0xa05,0xa0a,0xa0f,0xa10,0xa13,0xa28,0xa2a,0xa30,0xa32,0xa33,0xa35,0xa36,0xa38,0xa39,0xa3c,0xa3c,0xa3e,0xa42,0xa47,0xa48,0xa4b,0xa4d,0xa51,0xa51,0xa59,0xa5c,0xa5e,0xa5e,0xa66,0xa75,0xa81,0xa83,0xa85,0xa8d,0xa8f,0xa91,0xa93,0xaa8,0xaaa,0xab0,0xab2,0xab3,0xab5,0xab9,0xabc,0xac5,0xac7,0xac9,0xacb,0xacd,0xad0,0xad0,0xae0,0xae3,0xae6,0xaef,0xaf9,0xaf9,0xb01,0xb03,0xb05,0xb0c,0xb0f,0xb10,0xb13,0xb28,0xb2a,0xb30,0xb32,0xb33,0xb35,0xb39,0xb3c,0xb44,0xb47,0xb48,0xb4b,0xb4d,0xb56,0xb57,0xb5c,0xb5d,0xb5f,0xb63,0xb66,0xb6f,0xb71,0xb71,0xb82,0xb83,0xb85,0xb8a,0xb8e,0xb90,0xb92,0xb95,0xb99,0xb9a,0xb9c,0xb9c,0xb9e,0xb9f,0xba3,0xba4,0xba8,0xbaa,0xbae,0xbb9,0xbbe,0xbc2,0xbc6,0xbc8,0xbca,0xbcd,0xbd0,0xbd0,0xbd7,0xbd7,0xbe6,0xbef,0xc00,0xc03,0xc05,0xc0c,0xc0e,0xc10,0xc12,0xc28,0xc2a,0xc39,0xc3d,0xc44,0xc46,0xc48,0xc4a,0xc4d,0xc55,0xc56,0xc58,0xc5a,0xc60,0xc63,0xc66,0xc6f,0xc81,0xc83,0xc85,0xc8c,0xc8e,0xc90,0xc92,0xca8,0xcaa,0xcb3,0xcb5,0xcb9,0xcbc,0xcc4,0xcc6,0xcc8,0xcca,0xccd,0xcd5,0xcd6,0xcde,0xcde,0xce0,0xce3,0xce6,0xcef,0xcf1,0xcf2,0xd01,0xd03,0xd05,0xd0c,0xd0e,0xd10,0xd12,0xd3a,0xd3d,0xd44,0xd46,0xd48,0xd4a,0xd4e,0xd57,0xd57,0xd5f,0xd63,0xd66,0xd6f,0xd7a,0xd7f,0xd82,0xd83,0xd85,0xd96,0xd9a,0xdb1,0xdb3,0xdbb,0xdbd,0xdbd,0xdc0,0xdc6,0xdca,0xdca,0xdcf,0xdd4,0xdd6,0xdd6,0xdd8,0xddf,0xde6,0xdef,0xdf2,0xdf3,0xe01,0xe3a,0xe40,0xe4e,0xe50,0xe59,0xe81,0xe82,0xe84,0xe84,0xe87,0xe88,0xe8a,0xe8a,0xe8d,0xe8d,0xe94,0xe97,0xe99,0xe9f,0xea1,0xea3,0xea5,0xea5,0xea7,0xea7,0xeaa,0xeab,0xead,0xeb9,0xebb,0xebd,0xec0,0xec4,0xec6,0xec6,0xec8,0xecd,0xed0,0xed9,0xedc,0xedf,0xf00,0xf00,0xf18,0xf19,0xf20,0xf29,0xf35,0xf35,0xf37,0xf37,0xf39,0xf39,0xf3e,0xf47,0xf49,0xf6c,0xf71,0xf84,0xf86,0xf97,0xf99,0xfbc,0xfc6,0xfc6,0x1000,0x1049,0x1050,0x109d,0x10a0,0x10c5,0x10c7,0x10c7,0x10cd,0x10cd,0x10d0,0x10fa,0x10fc,0x1248,0x124a,0x124d,0x1250,0x1256,0x1258,0x1258,0x125a,0x125d,0x1260,0x1288,0x128a,0x128d,0x1290,0x12b0,0x12b2,0x12b5,0x12b8,0x12be,0x12c0,0x12c0,0x12c2,0x12c5,0x12c8,0x12d6,0x12d8,0x1310,0x1312,0x1315,0x1318,0x135a,0x135d,0x135f,0x1369,0x1371,0x1380,0x138f,0x13a0,0x13f5,0x13f8,0x13fd,0x1401,0x166c,0x166f,0x167f,0x1681,0x169a,0x16a0,0x16ea,0x16ee,0x16f8,0x1700,0x170c,0x170e,0x1714,0x1720,0x1734,0x1740,0x1753,0x1760,0x176c,0x176e,0x1770,0x1772,0x1773,0x1780,0x17d3,0x17d7,0x17d7,0x17dc,0x17dd,0x17e0,0x17e9,0x180b,0x180d,0x1810,0x1819,0x1820,0x1877,0x1880,0x18aa,0x18b0,0x18f5,0x1900,0x191e,0x1920,0x192b,0x1930,0x193b,0x1946,0x196d,0x1970,0x1974,0x1980,0x19ab,0x19b0,0x19c9,0x19d0,0x19da,0x1a00,0x1a1b,0x1a20,0x1a5e,0x1a60,0x1a7c,0x1a7f,0x1a89,0x1a90,0x1a99,0x1aa7,0x1aa7,0x1ab0,0x1abd,0x1b00,0x1b4b,0x1b50,0x1b59,0x1b6b,0x1b73,0x1b80,0x1bf3,0x1c00,0x1c37,0x1c40,0x1c49,0x1c4d,0x1c7d,0x1cd0,0x1cd2,0x1cd4,0x1cf6,0x1cf8,0x1cf9,0x1d00,0x1df5,0x1dfc,0x1f15,0x1f18,0x1f1d,0x1f20,0x1f45,0x1f48,0x1f4d,0x1f50,0x1f57,0x1f59,0x1f59,0x1f5b,0x1f5b,0x1f5d,0x1f5d,0x1f5f,0x1f7d,0x1f80,0x1fb4,0x1fb6,0x1fbc,0x1fbe,0x1fbe,0x1fc2,0x1fc4,0x1fc6,0x1fcc,0x1fd0,0x1fd3,0x1fd6,0x1fdb,0x1fe0,0x1fec,0x1ff2,0x1ff4,0x1ff6,0x1ffc,0x203f,0x2040,0x2054,0x2054,0x2071,0x2071,0x207f,0x207f,0x2090,0x209c,0x20d0,0x20dc,0x20e1,0x20e1,0x20e5,0x20f0,0x2102,0x2102,0x2107,0x2107,0x210a,0x2113,0x2115,0x2115,0x2118,0x211d,0x2124,0x2124,0x2126,0x2126,0x2128,0x2128,0x212a,0x2139,0x213c,0x213f,0x2145,0x2149,0x214e,0x214e,0x2160,0x2188,0x2c00,0x2c2e,0x2c30,0x2c5e,0x2c60,0x2ce4,0x2ceb,0x2cf3,0x2d00,0x2d25,0x2d27,0x2d27,0x2d2d,0x2d2d,0x2d30,0x2d67,0x2d6f,0x2d6f,0x2d7f,0x2d96,0x2da0,0x2da6,0x2da8,0x2dae,0x2db0,0x2db6,0x2db8,0x2dbe,0x2dc0,0x2dc6,0x2dc8,0x2dce,0x2dd0,0x2dd6,0x2dd8,0x2dde,0x2de0,0x2dff,0x3005,0x3007,0x3021,0x302f,0x3031,0x3035,0x3038,0x303c,0x3041,0x3096,0x3099,0x309f,0x30a1,0x30fa,0x30fc,0x30ff,0x3105,0x312d,0x3131,0x318e,0x31a0,0x31ba,0x31f0,0x31ff,0x3400,0x4db5,0x4e00,0x9fd5,0xa000,0xa48c,0xa4d0,0xa4fd,0xa500,0xa60c,0xa610,0xa62b,0xa640,0xa66f,0xa674,0xa67d,0xa67f,0xa6f1,0xa717,0xa71f,0xa722,0xa788,0xa78b,0xa7ad,0xa7b0,0xa7b7,0xa7f7,0xa827,0xa840,0xa873,0xa880,0xa8c4,0xa8d0,0xa8d9,0xa8e0,0xa8f7,0xa8fb,0xa8fb,0xa8fd,0xa8fd,0xa900,0xa92d,0xa930,0xa953,0xa960,0xa97c,0xa980,0xa9c0,0xa9cf,0xa9d9,0xa9e0,0xa9fe,0xaa00,0xaa36,0xaa40,0xaa4d,0xaa50,0xaa59,0xaa60,0xaa76,0xaa7a,0xaac2,0xaadb,0xaadd,0xaae0,0xaaef,0xaaf2,0xaaf6,0xab01,0xab06,0xab09,0xab0e,0xab11,0xab16,0xab20,0xab26,0xab28,0xab2e,0xab30,0xab5a,0xab5c,0xab65,0xab70,0xabea,0xabec,0xabed,0xabf0,0xabf9,0xac00,0xd7a3,0xd7b0,0xd7c6,0xd7cb,0xd7fb,0xf900,0xfa6d,0xfa70,0xfad9,0xfb00,0xfb06,0xfb13,0xfb17,0xfb1d,0xfb28,0xfb2a,0xfb36,0xfb38,0xfb3c,0xfb3e,0xfb3e,0xfb40,0xfb41,0xfb43,0xfb44,0xfb46,0xfbb1,0xfbd3,0xfd3d,0xfd50,0xfd8f,0xfd92,0xfdc7,0xfdf0,0xfdfb,0xfe00,0xfe0f,0xfe20,0xfe2f,0xfe33,0xfe34,0xfe4d,0xfe4f,0xfe70,0xfe74,0xfe76,0xfefc,0xff10,0xff19,0xff21,0xff3a,0xff3f,0xff3f,0xff41,0xff5a,0xff66,0xffbe,0xffc2,0xffc7,0xffca,0xffcf,0xffd2,0xffd7,0xffda,0xffdc];
	return isAmongRangeList(cp, cp3b);
}
bool isValid4ByteIdentifierTail(size_t cp) nothrow pure
{
	static immutable int[] cp4b = [0x10000,0x1000b,0x1000d,0x10026,0x10028,0x1003a,0x1003c,0x1003d,0x1003f,0x1004d,0x10050,0x1005d,0x10080,0x100fa,0x10140,0x10174,0x101fd,0x101fd,0x10280,0x1029c,0x102a0,0x102d0,0x102e0,0x102e0,0x10300,0x1031f,0x10330,0x1034a,0x10350,0x1037a,0x10380,0x1039d,0x103a0,0x103c3,0x103c8,0x103cf,0x103d1,0x103d5,0x10400,0x1049d,0x104a0,0x104a9,0x10500,0x10527,0x10530,0x10563,0x10600,0x10736,0x10740,0x10755,0x10760,0x10767,0x10800,0x10805,0x10808,0x10808,0x1080a,0x10835,0x10837,0x10838,0x1083c,0x1083c,0x1083f,0x10855,0x10860,0x10876,0x10880,0x1089e,0x108e0,0x108f2,0x108f4,0x108f5,0x10900,0x10915,0x10920,0x10939,0x10980,0x109b7,0x109be,0x109bf,0x10a00,0x10a03,0x10a05,0x10a06,0x10a0c,0x10a13,0x10a15,0x10a17,0x10a19,0x10a33,0x10a38,0x10a3a,0x10a3f,0x10a3f,0x10a60,0x10a7c,0x10a80,0x10a9c,0x10ac0,0x10ac7,0x10ac9,0x10ae6,0x10b00,0x10b35,0x10b40,0x10b55,0x10b60,0x10b72,0x10b80,0x10b91,0x10c00,0x10c48,0x10c80,0x10cb2,0x10cc0,0x10cf2,0x11000,0x11046,0x11066,0x1106f,0x1107f,0x110ba,0x110d0,0x110e8,0x110f0,0x110f9,0x11100,0x11134,0x11136,0x1113f,0x11150,0x11173,0x11176,0x11176,0x11180,0x111c4,0x111ca,0x111cc,0x111d0,0x111da,0x111dc,0x111dc,0x11200,0x11211,0x11213,0x11237,0x11280,0x11286,0x11288,0x11288,0x1128a,0x1128d,0x1128f,0x1129d,0x1129f,0x112a8,0x112b0,0x112ea,0x112f0,0x112f9,0x11300,0x11303,0x11305,0x1130c,0x1130f,0x11310,0x11313,0x11328,0x1132a,0x11330,0x11332,0x11333,0x11335,0x11339,0x1133c,0x11344,0x11347,0x11348,0x1134b,0x1134d,0x11350,0x11350,0x11357,0x11357,0x1135d,0x11363,0x11366,0x1136c,0x11370,0x11374,0x11480,0x114c5,0x114c7,0x114c7,0x114d0,0x114d9,0x11580,0x115b5,0x115b8,0x115c0,0x115d8,0x115dd,0x11600,0x11640,0x11644,0x11644,0x11650,0x11659,0x11680,0x116b7,0x116c0,0x116c9,0x11700,0x11719,0x1171d,0x1172b,0x11730,0x11739,0x118a0,0x118e9,0x118ff,0x118ff,0x11ac0,0x11af8,0x12000,0x12399,0x12400,0x1246e,0x12480,0x12543,0x13000,0x1342e,0x14400,0x14646,0x16800,0x16a38,0x16a40,0x16a5e,0x16a60,0x16a69,0x16ad0,0x16aed,0x16af0,0x16af4,0x16b00,0x16b36,0x16b40,0x16b43,0x16b50,0x16b59,0x16b63,0x16b77,0x16b7d,0x16b8f,0x16f00,0x16f44,0x16f50,0x16f7e,0x16f8f,0x16f9f,0x1b000,0x1b001,0x1bc00,0x1bc6a,0x1bc70,0x1bc7c,0x1bc80,0x1bc88,0x1bc90,0x1bc99,0x1bc9d,0x1bc9e,0x1d165,0x1d169,0x1d16d,0x1d172,0x1d17b,0x1d182,0x1d185,0x1d18b,0x1d1aa,0x1d1ad,0x1d242,0x1d244,0x1d400,0x1d454,0x1d456,0x1d49c,0x1d49e,0x1d49f,0x1d4a2,0x1d4a2,0x1d4a5,0x1d4a6,0x1d4a9,0x1d4ac,0x1d4ae,0x1d4b9,0x1d4bb,0x1d4bb,0x1d4bd,0x1d4c3,0x1d4c5,0x1d505,0x1d507,0x1d50a,0x1d50d,0x1d514,0x1d516,0x1d51c,0x1d51e,0x1d539,0x1d53b,0x1d53e,0x1d540,0x1d544,0x1d546,0x1d546,0x1d54a,0x1d550,0x1d552,0x1d6a5,0x1d6a8,0x1d6c0,0x1d6c2,0x1d6da,0x1d6dc,0x1d6fa,0x1d6fc,0x1d714,0x1da00,0x1da36,0x1da3b,0x1da6c,0x1da75,0x1da75,0x1da84,0x1da84,0x1da9b,0x1da9f,0x1daa1,0x1daaf,0x1d716,0x1d734,0x1d736,0x1d74e,0x1d750,0x1d76e,0x1d770,0x1d788,0x1d78a,0x1d7a8,0x1d7aa,0x1d7c2,0x1d7c4,0x1d7cb,0x1d7ce,0x1d7ff,0x1e800,0x1e8c4,0x1e8d0,0x1e8d6,0x1ee00,0x1ee03,0x1ee05,0x1ee1f,0x1ee21,0x1ee22,0x1ee24,0x1ee24,0x1ee27,0x1ee27,0x1ee29,0x1ee32,0x1ee34,0x1ee37,0x1ee39,0x1ee39,0x1ee3b,0x1ee3b,0x1ee42,0x1ee42,0x1ee47,0x1ee47,0x1ee49,0x1ee49,0x1ee4b,0x1ee4b,0x1ee4d,0x1ee4f,0x1ee51,0x1ee52,0x1ee54,0x1ee54,0x1ee57,0x1ee57,0x1ee59,0x1ee59,0x1ee5b,0x1ee5b,0x1ee5d,0x1ee5d,0x1ee5f,0x1ee5f,0x1ee61,0x1ee62,0x1ee64,0x1ee64,0x1ee67,0x1ee6a,0x1ee6c,0x1ee72,0x1ee74,0x1ee77,0x1ee79,0x1ee7c,0x1ee7e,0x1ee7e,0x1ee80,0x1ee89,0x1ee8b,0x1ee9b,0x1eea1,0x1eea3,0x1eea5,0x1eea9,0x1eeab,0x1eebb,0x20000,0x2a6d6,0x2a700,0x2b734,0x2b740,0x2b81d,0x2b820,0x2cea1,0x2f800,0x2fa1d,0xe0100,0xe01ef];
	return isAmongRangeList(cp, cp4b);
}
bool isValidByteIdentifierTail(size_t cp) nothrow pure
{
	if (cp < 0x80)
		return ((cp >= 0x61 && cp <= 0x7a) || (cp >= 0x41 && cp <= 0x5a) || (cp >= 0x30 && cp <= 0x39) || cp == '$' || cp == '_' || cp == 0x5f) ? true : false;
	if (cp < 0x800)
	{
		return cp.isValid2ByteIdentifierTail;
	} else if (cp < 0x10000)
	{
		return cp.isValid3ByteIdentifierTail;
	} else 
	{
		return cp.isValid4ByteIdentifierTail;
	}
}
@("isValidByteIdentifierStart")
unittest
{
	isValid2ByteIdentifierStart(0xba).shouldBeTrue;
	isValid2ByteIdentifierStart(0x2aa).shouldBeTrue;
	isValid2ByteIdentifierStart(0xd7).shouldBeFalse;
	isValid2ByteIdentifierStart(0x2c5).shouldBeFalse;
	isValid2ByteIdentifierStart(0x60c).shouldBeFalse;
	isValid3ByteIdentifierStart(0xca0).shouldBeTrue;
}
@("isValidByteIdentifierTail")
unittest
{
	isValid3ByteIdentifierTail(0x8a1).shouldBeTrue;
	isValid3ByteIdentifierTail(0xca0).shouldBeTrue;
	isValid3ByteIdentifierTail(0x800).shouldBeTrue;
	isValid3ByteIdentifierTail(0xffdc).shouldBeTrue;
}

size_t getStartIdentifierLength(Range)(Range r, size_t idx = 0) nothrow
{
	if (r[idx] <= 0x7f)
		return (r[idx] == '$' || r[idx] == '_' || (r[idx] >= '\x41' && r[idx] <= '\x5a') || (r[idx] >= '\x61' && r[idx] <= '\x7a')) ? 1 : 0;
	if (r[idx] >= 0xf0) // 4 byte unicode
	{
		if (r[idx] >= 0xf7)
			return InvalidUTF8;
		auto hex = r.decodeUnicodeCodePoint!4(idx);	// 4 byte unicode
		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid4ByteIdentifierStart ? 4 : 0;
	} else if (r[idx] >= 0xe0)
	{
		auto hex = r.decodeUnicodeCodePoint!3(idx);	// 3 byte unicode
		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid3ByteIdentifierStart ? 3 : 0;
	} else if (r[idx] >= 0xc0)
	{
		auto hex = r.decodeUnicodeCodePoint!2(idx);	// 2 byte unicode

		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid2ByteIdentifierStart ? 2 : 0;
	}
	return InvalidUTF8;
}

size_t decodeUnicodeCodePoint(size_t length, Range)(Range r, size_t idx = 0) pure nothrow
{
	// todo we can probably vectorize these utf8 decoding checks
	static if (length == 2)
	{
		if (r[idx+1] < 0x80)
			return InvalidUTF8;
		return ((r[idx] & 0x1f) << 6) | (r[idx+1] & 0x3f);
	} else static if (length == 3)
	{
		if (r[idx+1] < 0x80 || r[idx+2] < 0x80)
			return InvalidUTF8;
		return ((r[idx] & 0x0f) << 12) | ((r[idx+1] & 0x3f) << 6) | (r[idx+2] & 0x3f);
	} else static if (length == 4)
	{
		if (r[idx+1] < 0x80 || r[idx+2] < 0x80 || r[idx+3] < 0x80)
			return InvalidUTF8;
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
size_t getUnicodeLength(Range)(Range r, size_t idx = 0) pure nothrow @nogc
{
	size_t length = getCodePointLength(r[idx]);
	if (length == InvalidUTF8)
		return InvalidUTF8;
	version (unittest)
		assert(r.length > idx+length);
	foreach(i; 1..length)
		if (r[idx+i] < 0x80)
			return InvalidUTF8;
	return length;
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
	if (i < 0x80) {
		return cast(ubyte[])[cast(ubyte)i];
	} else if (i < 0x800) {
		return cast(ubyte[])[0xc0 | ((i >> 6) & 0x1f), 0x80 | (i & 0x3f)];
	} else if (i < 0x10000) {
		return cast(ubyte[])[0xe0 | ((i >> 12) & 0x0f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else if (i < 0x110000) {
		return cast(ubyte[])[0xf0 | ((i >> 18) & 0x07), 0x80 | ((i >> 12) & 0x3f), 0x80 | ((i >> 6) & 0x3f), 0x80 | (i & 0x3f)];
	} else {
		return cast(ubyte[])[];
	}
}

auto getTailIdentifierLength(Range)(Range r, size_t idx = 0) pure nothrow
{
	if (r[idx] < 0x80)
		return ((r[idx] >= 0x61 && r[idx] <= 0x7a) || (r[idx] >= 0x41 && r[idx] <= 0x5a) || (r[idx] >= 0x30 && r[idx] <= 0x39) || r[idx] == '$' || r[idx] == '_' || r[idx] == 0x5f) ? 1 : 0;
	if (r[idx] >= 0xf0) // 4 byte unicode
	{
		if (r[idx] >= 0xf7)
			return InvalidUTF8;
		auto hex = r.decodeUnicodeCodePoint!4(idx);	// 4 byte unicode
		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid4ByteIdentifierTail ? 4 : 0;
	} else if (r[idx] >= 0xe0)
	{
		auto hex = r.decodeUnicodeCodePoint!3(idx);	// 3 byte unicode
		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid3ByteIdentifierTail ? 3 : 0;
	} else if (r[idx] >= 0xc0)
	{
		auto hex = r.decodeUnicodeCodePoint!2(idx);	// 2 byte unicode
		if (hex == InvalidUTF8)
			return InvalidUTF8;
		return hex.isValid2ByteIdentifierTail ? 2 : 0;
	}
	return InvalidUTF8;
}

@("getTailIdentifierLength")
unittest
{
	"€".getTailIdentifierLength.shouldEqual(0);
	"b".getTailIdentifierLength.shouldEqual(1);
}

auto coerceToSingleQuotedString(const ubyte[] str) pure nothrow
{
	import std.algorithm : max,min;
	if (str.length == 0)
		return str;
	auto sink = appender!(ubyte[]);
	sink.reserve(str.length+max(2,min(str.length / 4, 5)));

	for (auto i = 0; i < str.length; i++)
	{
		ubyte s = str[i];
		if (s == '\\')
		{
			i++;
			if (i >= str.length)
			{
				sink.put('\\');
				break;
			}
			if (str[i] == '"')
				sink.put('"');
			else
			{
				sink.put('\\');
				sink.put(str[i]);
			}
			continue;
		}
		if (s == '\'')
			sink.put('\\');
		sink.put(s);
	}
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
	`\'`.coerceToSingleQuotedString.shouldEqual(`\'`);
}
size_t rangeMatch(bool invert, chars...)(const ubyte* str) pure nothrow @trusted @nogc
{
	version (ldcsse42)
	{
		static assert (chars.length % 2 == 0);
		enum byte16 str2E = ByteCombine!chars;
		static if (invert)
			enum flags = 0b0000_0100;
		else
			enum flags = 0b0001_0100;

		byte16 str1 = loadUnaligned!ubyte16(cast(ubyte*) str);
		byte16 str2 = str2E;

		return __builtin_ia32_pcmpestri128(str2, chars.length, str1, 16, flags);
	} else {
		static assert (chars.length % 2 == 0);
		enum byte16 str2E = ByteCombine!chars;
		static if (invert)
			enum rangeMatchFlags = 0b0000_0100;
		else
			enum rangeMatchFlags = 0b0001_0100;
		enum charsLength = chars.length;
		byte16 str2 = str2E;
		asm pure nothrow @nogc
		{
			naked;
			movdqu XMM1, [RDI];
			movq XMM2, str2;
			mov RAX, charsLength;
			mov RDX, 16;
			pcmpestri XMM2, XMM1, rangeMatchFlags;
			mov RAX, RCX;
			ret;
		}
	}
}

size_t skip(bool matching, chars...)(const ubyte* str) pure nothrow @trusted @nogc
{
	version (ldcsse42)
	{
		enum byte16 str2E = ByteCombine!chars;
		static if (matching)
			enum byte flags = 0b0001_0000;
		else
			enum byte flags = 0b0000_0000;

		byte16 str1 = loadUnaligned!ubyte16(cast(ubyte*) str);
		byte16 str2 = str2E;

		return __builtin_ia32_pcmpestri128(str2, chars.length, str1, 16, flags);
	} else {
		enum byte16 str2E = ByteCombine!chars;
		enum charsLength = chars.length;
		static if (matching)
			enum flags = 0b0001_0000;
		else
			enum flags = 0b0000_0000;
		byte16 str2 = str2E;
		asm pure nothrow @nogc
		{
			naked;
			movdqu XMM1, [RDI];
			movq XMM2, str2;
			mov RAX, charsLength;
			mov RDX, 16;
			pcmpestri XMM2, XMM1, flags;
			mov RAX, RCX;
			ret;
		}
	}
}

template ByteCombine(c...)
{
	//version (ldcsse42)
	//{
		enum byte16 ByteCombine = [
			Index!(0, c),
			Index!(1, c),
			Index!(2, c),
			Index!(3, c),
			Index!(4, c),
			Index!(5, c),
			Index!(6, c),
			Index!(7, c),
			Index!(8, c),
			Index!(9, c),
			Index!(10, c),
			Index!(11, c),
			Index!(12, c),
			Index!(13, c),
			Index!(14, c),
			Index!(15, c)
		];
	//} else {
	//	static assert (c.length <= 8);
	//	static if (c.length > 1)
	//		enum ulong ByteCombine = c[0] | (ByteCombine!(c[1..$]) << 8);
	//	else
	//		enum ulong ByteCombine = c[0];
	//}
}

template Index(size_t idx, c...)
{
	static if (c.length > idx)
		enum Index = cast(byte)c[idx];
	else
		enum Index = '\0';
}

size_t lengthToNextUnicodeCodePoint(Range)(Range s, size_t idx)
{
	size_t off = 0;
	for(;;off++)
	{
		if (s[idx+off] < 0x80)
			break;
	}
	return off;
}

// TODO: in general whenever we have an invalid char (e.g. an a-z in a decimalliteral), we need to skip all chars until we hit a separator (whitespace or punctuation)
struct Lexer
{
	private
	{
		Appender!(LexState[]) lexerState;
		bool haveSSE42;
	}
	LexState lexState;
	Token token;
	size_t line;
	size_t column;
	bool recentSeparator = false;
	private size_t tokenLength;	// TODO: is actually a wrong name. should be something meaning: the length on the current line
	private size_t _tokenLines;
	@property size_t tokenLines() { return _tokenLines; }
	const (ubyte)[] s;
	void pushState(State state) pure
	{
		this.pushState(LexState(state));
	}
	void pushState(LexState state) pure
	{
		lexerState.put(lexState);
		lexState = state;
	}
	void popState()
	{
		assert(lexerState.data.length > 0);
		lexState = lexerState.data[$-1];
		lexerState.shrinkTo(lexerState.data.length-1);
	}
	Lexer save()
	{
		auto l = Lexer(s);
		l.lexState = this.lexState;
		l.token = this.token;
		l.line = this.line;
		l.column = this.column;
		l.tokenLength = this.tokenLength;
		l.lexerState = appender!(LexState[])(this.lexerState.data.dup());
		return l;
	}
	this(const (ubyte)[] source) pure
	{
		s = source;
		pushState(s.empty ? State.EndOfFile : State.TokensRemaining);
		haveSSE42 = sse42(); //TODO: need to make template param
	}
	version(unittest)
	{
		auto scanToken(Goal goal = Goal.None, in string file = __FILE__, in size_t orgLine = __LINE__)
		{
			//return measure!("Scantoken",(){
				if (token.type == Type.SingleLineComment || token.type == Type.LineTerminator || token.type == Type.Semicolon)
				{
					recentSeparator = true;
				} else if (token.type == Type.MultiLineComment)
				{
					if (_tokenLines != 0)
						recentSeparator = true;
				} else
					recentSeparator = false;
				if (_tokenLines > 0)
				{
					line += _tokenLines;
					column = 0;
				}
				column += tokenLength;
				tokenLength = 0;
				_tokenLines = 0;
				token = lexToken(goal);
				version(chatty) { 
					import std.stdio; writefln("Scan: %s with %s @%s:%s %s called from %s:%s (prev newLine: %s)",token, goal, line, column, tokenLength, file,orgLine, recentSeparator); 
				}
				return token;
			//});
		}
	} else
	{
		auto scanToken(Goal goal = Goal.None)
		{
			//return measure!("Scantoken",(){
				if (token.type == Type.SingleLineComment || token.type == Type.LineTerminator || token.type == Type.Semicolon)
				{
					recentSeparator = true;
				} else if (token.type == Type.MultiLineComment)
				{
					if (_tokenLines != 0)
						recentSeparator = true;
				} else
					recentSeparator = false;
				if (_tokenLines > 0)
				{
					line += _tokenLines;
					column = 0;
				}
				column += tokenLength;
				tokenLength = 0;
				_tokenLines = 0;
				token = lexToken(goal);
				version(chatty) { 
					import std.stdio; writefln("Scan: %s with %s @%s:%s %s (prev newLine: %s)",token, goal, line, column, tokenLength, recentSeparator); 
				}
				return token;
			//});
		}
	}
	template traceFunction(string fun)
	{
		version (tracing) {
			enum traceFunction = 
				`writeln("`~fun~`");`;
		} else enum traceFunction = "";
	}
	private auto createTokenAndAdvance(Type tokenType, size_t len, const (ubyte)[] match) nothrow
	{
		s = s[len..$];
		return Token(tokenType, match);
	}
	private auto createTokenAndAdvance(Type tokenType, size_t len, string match = "") nothrow
	{
		s = s[len..$];
		return Token(tokenType, match);
	}
	auto getUnicodeEscapeLength(size_t idx)
	{
		auto result(bool valid)(size_t len) { return tuple!("valid","length")(valid,len); }
		if (s[idx] == '{')
		{
			size_t len = 1;
			if (s[idx+len] == '}')
				return result!(false)(2);
			for(;;len++)
			{
				auto chr = s[idx+len];
				if (chr == '}')
					return result!(true)(len+1);
				if ((chr <= '0' || chr >= '9') && (chr <= 'a' && chr >= 'z') && (chr <= 'A' && chr >= 'Z'))
					return result!(false)(len);
			}
		} else
		{
			foreach(i; 0..4)
			{
				auto chr = s[idx+i];
				if (chr == 0)
					return result!(false)(i);
				if ((chr <= '0' || chr >= '9') && (chr <= 'a' && chr >= 'z') && (chr <= 'A' && chr >= 'Z'))
					return result!(false)(i);
			}
			return result!(true)(4);
		}
	}
	Token lexStartIdentifier(ref size_t idx)
	{
		mixin(traceFunction!(__FUNCTION__));
		import std.format : format;
		auto chr = s[idx];
		auto len = s.getStartIdentifierLength(idx);
		if (len == InvalidUTF8)
		{
			idx += s.lengthToNextUnicodeCodePoint(idx);
			tokenLength += 1;
			return Token(Type.InvalidUTF8);
		}
		if (len > 0)
		{
			idx += len;
			tokenLength ++;
			return Token(Type.StartIdentifier);
		}
		if (s[idx] == '\\')
		{
			return lexUnicodeEscapeSequence(idx);
		}
		idx++;
		return Token(Type.Error,format("Invalid character %s to start identifier",chr));
	}
	Token lexUnicodeEscapeSequence(ref size_t idx)
	{
		mixin(traceFunction!(__FUNCTION__));
		import std.format : format;
		assert(s[idx] == '\\');
		idx++;
		tokenLength += 2;
		ubyte chr = s[idx++];
		if (chr != 'u')
		{
			s = s[idx..$];
			if (chr == 0)
				return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
			return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr));
		}
		auto escape = getUnicodeEscapeLength(idx);
		if (!escape.valid)
		{
			if (s[idx + escape.length - 1] == 0)
				return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
			return Token(Type.Error,format("Invalid UnicodeEscapeSequence \\u%s",cast(const(char)[])s[idx .. idx + escape.length]));
		}
		idx += escape.length;
		tokenLength += escape.length;
		return Token(Type.UnicodeEscapeSequence);
	}
	Token lexTailIdentifier(ref size_t idx)
	{
		mixin(traceFunction!(__FUNCTION__));
		auto len = s.getTailIdentifierLength(idx);
		if (len == InvalidUTF8)
		{
			idx += s.lengthToNextUnicodeCodePoint(idx);
			tokenLength += 1;
			return Token(Type.InvalidUTF8);
		}
		if (len > 0)
		{
			idx += len;
			tokenLength ++;
			return Token(Type.TailIdentifier);
		}
		if (s[idx] == '\\')
		{
			return lexUnicodeEscapeSequence(idx);
		}
		return Token(Type.EndIdentifier);
	}
	Token lexIdentifier() @trusted
	{
		mixin(traceFunction!(__FUNCTION__));
		import std.format : format;
		size_t idx = 0;
		version (iasm64NotWindows)
		{
			//if (haveSSE42)
			//{
				immutable ulong i = rangeMatch!(false, 'a', 'z', 'A', 'Z', '_', '_', '$', '$')(s.ptr + idx);
				if (i > 0)
				{
					idx += i;
					tokenLength += i;
				} else
				{
					auto t = lexStartIdentifier(idx);
					if (t.type == Type.Error || t.type == Type.InvalidUTF8)
					{
						s = s[idx..$];
						return t;
					}
				}
			//}
		} else {
			auto t = lexStartIdentifier(idx);
			if (t.type == Type.Error || t.type == Type.InvalidUTF8)
			{
				s = s[idx..$];
				return t;
			}
		}
		for(;;)
		{
			auto t2 = lexTailIdentifier(idx);
			if (t2.type == Type.Error || t2.type == Type.InvalidUTF8)
			{
				s = s[idx..$];
				return t2;
			}
			if (t2.type == Type.EndIdentifier)
			{
				auto tok = Token(Type.Identifier,s[0..idx]);
				s = s[idx..$];
				return tok;
			}
		}
	}
	void popWhitespace() @trusted
	{
		mixin(traceFunction!(__FUNCTION__));
		size_t idx = 0;
		for (;;)
		{
			version (iasm64NotWindows)
			{
				//if (haveSSE42)
				//{
					immutable ulong i = skip!(true, '\x09', '\x0B', '\x0C', ' ')(s.ptr + idx);
					if (i > 0)
					{
						idx += i;
						column += i;
					} else
					{
						auto len = s.getWhiteSpaceLength(idx);
						if (len == 0)
							break;
						idx += len;
						column += 1;
					}
				//}
			} else
			{
				auto len = s.getWhiteSpaceLength(idx);
				if (len == 0)
					break;
				idx += len;
				column += 1;
			}
		}
		s = s[idx..$];
	}
	auto lookAheadRegex()
	{
		size_t charLen = 0;
		size_t len = s.getRegexLength(charLen);
		if (len != 0)
			tokenLength += charLen;
		return len;
	}
	Token lexString() @trusted
	{
		mixin(traceFunction!(__FUNCTION__));
		auto type = s.front();	// either " or '
		column++;
		size_t idx = 1;
		while (true)
		{
			version (iasm64NotWindows)
			{
				ulong i;
				if (type == '\'')
					i = skip!(false, '\n', '\\', '\'', '\0')(s.ptr + idx);
				else
					i = skip!(false, '\n', '\\', '\"', '\0')(s.ptr + idx);
				if (i > 0)
				{
					idx += i;
					column += i; // TODO: we might have skipped over multibyte codepoint, at which point we advance the column by too much
				}
				if (i == 16)
					continue;
			}
			auto codePoint = s.decodeCodePoint(idx);
			if (codePoint < 0x80)
			{
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
					case 0x0d:
						if (s[idx] == '\x0a')
						{
							column++;
							idx++;
						}
						goto case 0x0a;
					case 0x0a:
						return createTokenAndAdvance(Type.Error,idx,"Invalid new line in string");
					case '\\':
						if (s[idx] == 0)
							goto eof;
						auto cp = s.decodeCodePoint(idx);
						auto len = cp == InvalidUTF8 ? 1 : getCodePointEncodedLength(cp);
						idx += len;
						column++;
						break;
					case 0:
						goto eof;
					default:
						break;
				}
			} else {
				if (codePoint == InvalidUTF8)
				{
					idx += 1;
					column ++;
					continue;
				}
				idx += getCodePointEncodedLength(codePoint);
				column ++;
				if (codePoint == 0x2028 || codePoint == 0x2029)
					return createTokenAndAdvance(Type.Error,idx,"Invalid new line in string");
				break;
			}
		}
		eof: return createTokenAndAdvance(Type.Error,idx,"Invalid eof while lexing string");
	}
	Token lexBaseLiteral(int base, alias TokenType)(bool isFloat = false)
	{
		mixin(traceFunction!(__FUNCTION__));
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
				tokenLength++;
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
					tokenLength++;
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
						tokenLength++;
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
							tokenLength++;
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
					if (len == InvalidUTF8)
						return createTokenAndAdvance(Type.InvalidUTF8,idx);
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
		mixin(traceFunction!(__FUNCTION__));
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
					auto len = s.getUnicodeLength(idx);
					if (len == InvalidUTF8)
						return createTokenAndAdvance(Type.InvalidUTF8,idx,s[0..idx]);
					idx += len;
					break;
				default:
					auto cp = s.decodeCodePoint(idx);
					if (cp == InvalidUTF8)
						return createTokenAndAdvance(Type.InvalidUTF8,idx,s[0..idx]);
					if (cp.isLineTerminator)
					{
						line += 1;
						column = 0;
						tokenLength = 0;
					}
					idx += cp.getCodePointEncodedLength();
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
		mixin(traceFunction!(__FUNCTION__));
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

			auto cp = s.decodeCodePoint(idx);
			if (cp == InvalidUTF8)
			{
				idx ++;
				column++;
			}
			else if (cp.isLineTerminator())
			{
				idx++;
				if (cp == 0x0d && s[idx+1] == 0x0a)
					idx++;
				_tokenLines++;
				column=0;
			} else
			{
				idx += cp.getCodePointEncodedLength();
				column++;
			}
		}
		eof:
		return createTokenAndAdvance(Type.Error,idx,"Expected end of MultiLineComment before eof");
	}
	Token lexSingleLineComment() @trusted
	{
		mixin(traceFunction!(__FUNCTION__));
		size_t idx = 0;
		size_t cp;
		for(;;)
		{
			version (iasm64NotWindows)
			{
				//if (haveSSE42)
				//{
					// NOTE: if whole comment is full with unicode stuff we waste alot of time calling the expensive sse4.2 instruction
					immutable ulong i = skip!(false, '\n', '\r', '\xE2', '\x00')(s.ptr + idx);
					idx += i;
					tokenLength += i;
					if (i == 16)
						continue;
				//}
			}
			cp = s.decodeCodePoint(idx);
			if (cp == InvalidUTF8)
			{
				idx++;
			} else if (cp.isLineTerminator() || cp == 0)
			{
				if (cp == 0x0d && s[idx+1] == 0x0a) {
					auto tok = Token(Type.SingleLineComment,s[0..idx]);
					_tokenLines++;
					tokenLength = 0;
					s = s[idx+2..$];
					return tok;
				}
				break;
			}
			else {
				idx += cp.getCodePointEncodedLength();
			}
		}
		auto tok = Token(Type.SingleLineComment,s[0..idx]);
		idx += cp.getCodePointEncodedLength();
		_tokenLines++;
		tokenLength = 0;
		s = s[idx..$];
		return tok;
	}
	Token lexSheBang() @trusted
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(s[0] == '#');
		assert(s[1] == '!');
		size_t idx = 0;
		size_t cp;
		for(;;)
		{
			version (iasm64NotWindows)
			{
				//if (haveSSE42)
				//{
					// NOTE: if whole comment is full with unicode stuff we waste alot of time calling the expensive sse4.2 instruction
					immutable ulong i = skip!(false, '\n', '\r', '\xE2', '\x00')(s.ptr + idx);
					idx += i;
					tokenLength += i;
					if (i == 16)
						continue;
				//}
			}
			cp = s.decodeCodePoint(idx);
			if (cp == InvalidUTF8)
			{
				idx++;
			} else if (cp.isLineTerminator() || cp == 0)
				break;
			else
				idx += cp.getCodePointEncodedLength();
		}
		auto tok = Token(Type.SingleLineComment,s[0..idx]);
		idx += cp.getCodePointEncodedLength();
		_tokenLines++;
		tokenLength = 0;
		s = s[idx..$];
		return tok;
	}
	Token lexToken(Goal goal = Goal.None)
	{
		mixin(traceFunction!(__FUNCTION__));
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
			case '{': tokenLength++; s.popFront(); if (lexState.state == State.LexingTemplateLiteral) lexState.openCurlyCount += 1; return Token(Type.OpenCurlyBrace);
			case '}': 
				tokenLength++;
				if (lexState.state == State.LexingTemplateLiteral)
				{
					if (lexState.openCurlyCount < 1)
					{
						popState();
						s.popFront();
						return lexTemplateLiteral(Type.TemplateTail);
					} else
						lexState.openCurlyCount -= 1;
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
				if (goal != Goal.NoRegex)
				{
					auto len = lookAheadRegex;
					if (len == InvalidUTF8)
					{
						len = lengthToNextUnicodeCodePoint(s,0);
						tokenLength++;
						return createTokenAndAdvance(Type.InvalidUTF8,len,s[0..len]);
					}
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
				tokenLength++;
				return lexOctalLiteral;
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
	auto lexer = createLexer("abcde");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"abcde"));
	lexer = createLexer("π");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"π"));
	lexer = createLexer("ಠ_ಠ");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"ಠ_ಠ"));
	lexer = createLexer(`\u00aa`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,`\u00aa`));
	lexer = createLexer(`\u{00aaf}`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,`\u{00aaf}`));
	lexer = createLexer(`\u{}`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Error,`Invalid UnicodeEscapeSequence \u{}`));
	lexer = createLexer(`\u069`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Error,`Invalid UnicodeEscapeSequence \u069`));
	lexer = createLexer(`a\u0069`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,`a\u0069`));
	lexer = createLexer(`a\u{00690}`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,`a\u{00690}`));
	lexer = createLexer(`a\u{}`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Error,`Invalid UnicodeEscapeSequence \u{}`));
	lexer = createLexer(`a\u069`);
	lexer.lexIdentifier.shouldEqual(Token(Type.Error,`Invalid UnicodeEscapeSequence \u069`));
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
	lexer = createLexer("/ab[[]]cd/");
	lexer.lookAheadRegex.shouldEqual(10);
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
	lexer = createLexer(`'\'';`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"\\'"));
	lexer.scanToken().shouldEqual(Token(Type.Semicolon));
	lexer = createLexer(`"\'";`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"\\'"));
	lexer.scanToken().shouldEqual(Token(Type.Semicolon));
	lexer = createLexer(`'\"';`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`\"`));
	lexer.scanToken().shouldEqual(Token(Type.Semicolon));
	lexer = createLexer(`"\"";`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`"`));
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
	lexer.scanToken().shouldEqual(Token(Type.OctalLiteral,"123"));
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

	lexer = createLexer("`\n${template(err.messageData || {})}`");
	assert(lexer.tokens.equal([
		Token(Type.TemplateHead,"\n"),
		Token(Type.Identifier,"template"),
		Token(Type.OpenParenthesis),
		Token(Type.Identifier,"err"),
		Token(Type.Dot),
		Token(Type.Identifier,"messageData"),
		Token(Type.LogicalOr),
		Token(Type.OpenCurlyBrace),
		Token(Type.CloseCurlyBrace),
		Token(Type.CloseParenthesis),
		Token(Type.TemplateTail)
	]));

	lexer = createLexer("`\n${bla ? `(${hup})` : ''}`");
	assert(lexer.tokens.equal([
		Token(Type.TemplateHead,"\n"),
		Token(Type.Identifier,"bla"),
		Token(Type.QuestionMark),
		Token(Type.TemplateHead,"("),
		Token(Type.Identifier,"hup"),
		Token(Type.TemplateTail,")"),
		Token(Type.Colon),
		Token(Type.StringLiteral,""),
		Token(Type.TemplateTail)
	]));
	
	lexer = createLexer("`a${fun(bla || {a: `hup${fun(bla || {a: 6})}`})}`");
	assert(lexer.tokens.equal([
		Token(Type.TemplateHead,"a"),
		Token(Type.Identifier,"fun"),
		Token(Type.OpenParenthesis),
		Token(Type.Identifier,"bla"),
		Token(Type.LogicalOr),
		Token(Type.OpenCurlyBrace),
		Token(Type.Identifier,"a"),
		Token(Type.Colon),
		Token(Type.TemplateHead,"hup"),
		Token(Type.Identifier,"fun"),
		Token(Type.OpenParenthesis),
		Token(Type.Identifier,"bla"),
		Token(Type.LogicalOr),
		Token(Type.OpenCurlyBrace),
		Token(Type.Identifier,"a"),
		Token(Type.Colon),
		Token(Type.DecimalLiteral,"6"),
		Token(Type.CloseCurlyBrace),
		Token(Type.CloseParenthesis),
		Token(Type.TemplateTail),
		Token(Type.CloseCurlyBrace),
		Token(Type.CloseParenthesis),
		Token(Type.TemplateTail)
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
@("Invalid UTF8")
unittest
{
// we can also tests unicode code points where the encoding is missing its last byte

	[0x81].getWhiteSpaceLength().shouldEqual(0);
	[0xa0].getWhiteSpaceLength().shouldEqual(0);
	[0xff].getWhiteSpaceLength().shouldEqual(0);

	[0x81].getLineTerminatorLength().shouldEqual(0);
	[0xa0].getLineTerminatorLength().shouldEqual(0);
	[0xff].getLineTerminatorLength().shouldEqual(0);

	[0x81,0x00,0x00].getLineTerminatorUnicodeLength().shouldEqual(0);
	[0xa0,0x00,0x00].getLineTerminatorUnicodeLength().shouldEqual(0);
	[0xe0,0x00,0x00].getLineTerminatorUnicodeLength().shouldEqual(0);
	[0xff,0x00,0x00].getLineTerminatorUnicodeLength().shouldEqual(0);

	[0x81,0x00,0x00,0x00].decodeCodePoint().shouldEqual(InvalidUTF8);
	[0xa0,0x00,0x00,0x00].decodeCodePoint().shouldEqual(InvalidUTF8);
	[0xc0,0x00,0x00,0x00].decodeCodePoint().shouldEqual(InvalidUTF8);
	[0xe0,0x00,0x00,0x00].decodeCodePoint().shouldEqual(InvalidUTF8);
	[0xff,0x00,0x00,0x00].decodeCodePoint().shouldEqual(InvalidUTF8);

	//getCodePointLength(0x81).shouldEqual(InvalidUTF8);
	//getCodePointLength(0xf8).shouldEqual(InvalidUTF8);

	{
		size_t dummy;
		// slash followed by incomplete code point (missing last byte)
		['/',0xF0,0x90,0x8D,0x0,0x0].getRegexLength(dummy).shouldEqual(InvalidUTF8);
		['/',0xF0,0x90,0x8D,0x0,0x0].getRegexLength(dummy).shouldEqual(InvalidUTF8);
		['/',0xE0,0x90,0x00,0x0,0x0].getRegexLength(dummy).shouldEqual(InvalidUTF8);
		['/',0xC0,0x00,0x00,0x0,0x0].getRegexLength(dummy).shouldEqual(InvalidUTF8);
	}

	[0x81].getStartIdentifierLength().shouldEqual(InvalidUTF8);
	[0xFF].getStartIdentifierLength().shouldEqual(InvalidUTF8);
	[0xF0,0x00,0x00,0x00].getStartIdentifierLength().shouldEqual(InvalidUTF8);
	[0xE0,0x00,0x00].getStartIdentifierLength().shouldEqual(InvalidUTF8);
	[0xC0,0x00,0x00].getStartIdentifierLength().shouldEqual(InvalidUTF8);

	[0xF0,0x00,0x00,0x00].decodeUnicodeCodePoint!4().shouldEqual(InvalidUTF8);
	[0xE0,0x00,0x00].decodeUnicodeCodePoint!3().shouldEqual(InvalidUTF8);
	[0xC0,0x00].decodeUnicodeCodePoint!2().shouldEqual(InvalidUTF8);

	//[0xFF].getUnicodeLength().shouldEqual(InvalidUTF8);
	[0xF0,0x00,0x00,0x00,0x00].getUnicodeLength().shouldEqual(InvalidUTF8);
	[0xE0,0x00,0x00,0x00].getUnicodeLength().shouldEqual(InvalidUTF8);
	[0xC0,0x00,0x00].getUnicodeLength().shouldEqual(InvalidUTF8);

	[0x81].getTailIdentifierLength().shouldEqual(InvalidUTF8);
	[0xFF].getTailIdentifierLength().shouldEqual(InvalidUTF8);
	[0xF0,0x00,0x00,0x00].getTailIdentifierLength().shouldEqual(InvalidUTF8);
	[0xE0,0x00,0x00].getTailIdentifierLength().shouldEqual(InvalidUTF8);
	[0xC0,0x00,0x00].getTailIdentifierLength().shouldEqual(InvalidUTF8);

	auto lexStartIdentifier(const (ubyte)[] data)
	{
		size_t dummy;
		auto lexer = createLexer(data);
		return lexer.lexStartIdentifier(dummy);
	}
	lexStartIdentifier([cast(ubyte)0x81]).shouldEqual(Token(Type.InvalidUTF8));
	lexStartIdentifier([cast(ubyte)0xFF]).shouldEqual(Token(Type.InvalidUTF8));
	lexStartIdentifier([cast(ubyte)0xF0]).shouldEqual(Token(Type.InvalidUTF8));
	lexStartIdentifier([cast(ubyte)0xE0]).shouldEqual(Token(Type.InvalidUTF8));
	lexStartIdentifier([cast(ubyte)0xC0]).shouldEqual(Token(Type.InvalidUTF8));
	
	auto lexTailIdentifier(const (ubyte)[] data)
	{
		size_t dummy;
		auto lexer = createLexer(data);
		return lexer.lexTailIdentifier(dummy);
	}
	lexTailIdentifier([cast(ubyte)0x81]).shouldEqual(Token(Type.InvalidUTF8));
	lexTailIdentifier([cast(ubyte)0xFF]).shouldEqual(Token(Type.InvalidUTF8));
	lexTailIdentifier([cast(ubyte)0xF0]).shouldEqual(Token(Type.InvalidUTF8));
	lexTailIdentifier([cast(ubyte)0xE0]).shouldEqual(Token(Type.InvalidUTF8));
	lexTailIdentifier([cast(ubyte)0xC0]).shouldEqual(Token(Type.InvalidUTF8));

	auto lexIdentifier(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexIdentifier();
	}
	lexIdentifier([cast(ubyte)0x81]).shouldEqual(Token(Type.InvalidUTF8));
	lexIdentifier([cast(ubyte)0xFF]).shouldEqual(Token(Type.InvalidUTF8));
	lexIdentifier([cast(ubyte)0xF0]).shouldEqual(Token(Type.InvalidUTF8));
	lexIdentifier([cast(ubyte)0xE0]).shouldEqual(Token(Type.InvalidUTF8));
	lexIdentifier([cast(ubyte)0xC0]).shouldEqual(Token(Type.InvalidUTF8));

	auto lexString(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexString();
	}
	lexString([cast(ubyte)'"',0x81,'"']).shouldEqual(Token(Type.StringLiteral,"\x81"));
	lexString([cast(ubyte)'"',0xFF,'"']).shouldEqual(Token(Type.StringLiteral,"\xFF"));
	lexString([cast(ubyte)'"',0xF0,'"']).shouldEqual(Token(Type.StringLiteral,"\xF0"));
	lexString([cast(ubyte)'"',0xE0,'"']).shouldEqual(Token(Type.StringLiteral,"\xE0"));
	lexString([cast(ubyte)'"',0xC0,'"']).shouldEqual(Token(Type.StringLiteral,"\xC0"));

	auto lexBinaryLiteral(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexBinaryLiteral();
	}
	lexBinaryLiteral([cast(ubyte)0x81]).shouldEqual(Token(Type.InvalidUTF8));
	lexBinaryLiteral([cast(ubyte)0xFF]).shouldEqual(Token(Type.InvalidUTF8));
	lexBinaryLiteral([cast(ubyte)0xF0]).shouldEqual(Token(Type.InvalidUTF8));
	lexBinaryLiteral([cast(ubyte)0xE0]).shouldEqual(Token(Type.InvalidUTF8));
	lexBinaryLiteral([cast(ubyte)0xC0]).shouldEqual(Token(Type.InvalidUTF8));

	auto lexTemplateLiteral(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexTemplateLiteral();
	}
	lexTemplateLiteral([cast(ubyte)0x81]).shouldEqual(Token(Type.InvalidUTF8));
	lexTemplateLiteral([cast(ubyte)0xFF]).shouldEqual(Token(Type.InvalidUTF8));
	lexTemplateLiteral([cast(ubyte)0xF0]).shouldEqual(Token(Type.InvalidUTF8));
	lexTemplateLiteral([cast(ubyte)0xE0]).shouldEqual(Token(Type.InvalidUTF8));
	lexTemplateLiteral([cast(ubyte)0xC0]).shouldEqual(Token(Type.InvalidUTF8));

	auto lexMultiLineComment(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexMultiLineComment();
	}
	lexMultiLineComment([cast(ubyte)0x81]).shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexMultiLineComment([cast(ubyte)0xFF]).shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexMultiLineComment([cast(ubyte)0xF0]).shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexMultiLineComment([cast(ubyte)0xE0]).shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexMultiLineComment([cast(ubyte)0xC0]).shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));

	auto lexSingleLineComment(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexSingleLineComment();
	}
	lexSingleLineComment([cast(ubyte)0x81]).shouldEqual(Token(Type.SingleLineComment,"\x81"));
	lexSingleLineComment([cast(ubyte)0xFF]).shouldEqual(Token(Type.SingleLineComment,"\xFF"));
	lexSingleLineComment([cast(ubyte)0xF0]).shouldEqual(Token(Type.SingleLineComment,"\xF0"));
	lexSingleLineComment([cast(ubyte)0xE0]).shouldEqual(Token(Type.SingleLineComment,"\xE0"));
	lexSingleLineComment([cast(ubyte)0xC0]).shouldEqual(Token(Type.SingleLineComment,"\xC0"));

	auto lexSheBang(const (ubyte)[] data)
	{
		auto lexer = createLexer(data);
		return lexer.lexSheBang();
	}
	lexSheBang([cast(ubyte)'#','!',0x81]).shouldEqual(Token(Type.SingleLineComment,"#!\x81"));
	lexSheBang([cast(ubyte)'#','!',0xFF]).shouldEqual(Token(Type.SingleLineComment,"#!\xFF"));
	lexSheBang([cast(ubyte)'#','!',0xF0]).shouldEqual(Token(Type.SingleLineComment,"#!\xF0"));
	lexSheBang([cast(ubyte)'#','!',0xE0]).shouldEqual(Token(Type.SingleLineComment,"#!\xE0"));
	lexSheBang([cast(ubyte)'#','!',0xC0]).shouldEqual(Token(Type.SingleLineComment,"#!\xC0"));

}
auto byLines(string input)
{
	import std.stdio;
	struct Lines
	{
		@safe:
		private size_t start, end, nextLine;
		private string input;
		size_t line;
		private void determineEnd() {
			if (end > start || empty)
				return;
			size_t cnt;
			foreach(dchar c; input[start..$])
			{
				cnt++;
				switch(c)
				{
					case '\x0d':
						if (start+cnt > input.length && input[start+cnt] == '\x0a')
							nextLine = start + cnt + 1;
						else
							nextLine = start + cnt;
						end = start + cnt - 1;
						return;
					case '\x0a':
						nextLine = start + cnt;
						end = start + cnt - 1;
						return;
					case '\u2028':
					case '\u2029':
						cnt += 2;
						nextLine = start + cnt;
						end = start + cnt - 1;
						return;
					default:
						if (cast(uint)c > 0xffff)
							cnt += 3;
						else if (c > '\u07ff')
							cnt += 2;
						else if (c > '\x7f')
							cnt += 1;
				}
			}
			line++;
			end = input.length;
		}
		this(string input)
		{
			this.input = input;
		}
		bool empty()
		{
			return start > input.length;
		}
		void popFront()
		{
			line++;
			start = nextLine;
			determineEnd();
		}
		string front()
		{
			determineEnd();
			return input[start..$];
		}
	}
	return Lines(input);
}

// NOTE: no longer works since change to visitor
/*void checkLineAndColumnCounts(string input)
{
	import std.stdio;
	void checkContentByToken(Line)(Line line, Token token, ref Lexer lexer, ptrdiff_t lineOffset = 0)
	{
		import std.algorithm : startsWith;
		import std.format : format;
		ptrdiff_t offset = lineOffset+lexer.column;
		assert(offset >= 0);
		if (!line[offset..$].startsWith(cast(const(char)[])token.match))
			throw new Exception(format("Lexer mentioned %s:%s but content doesn't match (offset %s)\ntoken-matched: %s (raw-length: %s)\nraw: %s",lexer.line,lexer.column,offset,cast(const(char)[])token.match,lexer.tokenLength,line[offset..offset+token.match.length]));
	}
	void checkContent(Line)(Line line, string match, ref Lexer lexer, ptrdiff_t lineOffset = 0)
	{
		import std.algorithm : startsWith;
		import std.format : format;
		ptrdiff_t offset = lineOffset+lexer.column;
		assert(offset >= 0);
		if (!line[offset..$].startsWith(match))
			throw new Exception(format("Lexer mentioned %s:%s but content doesn't match\ntoken-matched: %s\nraw: %s",lexer.line,lexer.column,match,line[offset..offset+match.length]));
	}
	size_t column = 0;
	auto lexer = createLexer(input);
	auto lines = input.byLines;
	auto token = lexer.scanToken();
	while (token.type != Type.EndOfFile)
	{
		writeln(token, " ", lexer.line,":",lexer.column);
		while (lexer.line > lines.line)
		{
			lines.popFront();
			if (lines.empty())
				throw new Exception("Input empty while still token left");
		}
		auto line = lines.front();
		switch(token.type)
		{
			default:
				break;
			case Type.StringLiteral:
				auto s = line[lexer.column+1..lexer.column+lexer.tokenLength].coerceToSingleQuotedString();
				writefln("Found: %s",s);
				checkContentByToken(s, token, lexer, 0-lexer.column); break;
			case Type.BinaryLiteral:
			case Type.HexLiteral:
			case Type.OctalLiteral:
			case Type.SingleLineComment:
				checkContentByToken(line, token, lexer, 2); break;
			case Type.DecimalLiteral:
			case Type.Identifier:
				checkContentByToken(line, token, lexer);
				break;
			//case Type.SheBang,
			//case Type.Error,
			//case Type.TemplateHead,
			//case Type.TemplateMiddle,
			//case Type.Template,
			//case Type.TemplateTail,
			//case Type.EndOfFile,
			//case Type.LineTerminator,
			//case Type.OpenCurlyBrace,
			//case Type.CloseCurlyBrace,
			case Type.OpenParenthesis:
				checkContent(line, "(", lexer);
				break;
			//case Type.CloseParenthesis,
			//case Type.OpenSquareBrackets,
			//case Type.CloseSquareBrackets,
			//case Type.Dot,
			//case Type.SpreadOperator,
			case Type.Semicolon:
				checkContent(line, ";", lexer);
				break;
			//case Type.Comma,
			//case Type.LeftShiftAssignment,
			//case Type.LeftShift,
			//case Type.LessOrEqual,
			//case Type.LessThan,
			//case Type.TripleRightShiftAssignment,
			//case Type.TripleRightSift,
			//case Type.RightShiftAssignment,
			//case Type.RightShift,
			//case Type.GreaterOrEqual,
			case Type.GreaterThan:
				checkContent(line, ">", lexer);
				break;
			//case Type.StrictEqual,
			//case Type.Equal,
			//case Type.Arrow,
			case Type.Assignment:
				checkContent(line, "=", lexer);
				break;
			//case Type.StrictNotEqual,
			//case Type.NotEqual,
			//case Type.Negation,
			//case Type.Increment,
			//case Type.AdditiveAssignment,
			//case Type.Add,
			//case Type.Decrement,
			//case Type.DecrementalAssignment,
			//case Type.Minus,
			//case Type.MultiplicativeAssignment,
			//case Type.Multiply,
			case Type.DivisionAssignment:
				checkContent(line, "/=", lexer);
				break;
			//case Type.Division,
			//case Type.ModAssignment,
			//case Type.Mod,
			//case Type.LogicalAnd,
			//case Type.BitwiseAndAssignment,
			//case Type.BitwiseAnd,
			//case Type.LogicalOr,
			//case Type.BitwiseOrAssignment,
			//case Type.BitwiseOr,
			//case Type.BitwiseXorAssignment,
			//case Type.BitwiseXor,
			//case Type.Tilde,
			case Type.QuestionMark:
				checkContent(line, "?", lexer);
				break;
			case Type.Colon:
				checkContent(line, ":", lexer);
				break;
			//case Type.Regex,
			//case Type.MultiLineComment,
			//case Type.ExponentIndicator,
			//case Type.UnicodeEscapeSequence,
			//case Type.StartIdentifier,
			//case Type.TailIdentifier,
			//case Type.EndIdentifier
			
		}
		token = lexer.scanToken();
	}
}

@("checkLineAndColumnCounts")
unittest
{
	checkLineAndColumnCounts("var a = identifier;");
	checkLineAndColumnCounts("var a =
identifier, b = \"basdfas\"");
	checkLineAndColumnCounts("var a =
	44, b = 0b01, c = 0x01, d = 0o55");
}*/
