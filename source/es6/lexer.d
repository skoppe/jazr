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

import std.range : empty, front, popFront, save, take, drop;
import std.array : Appender, appender;
import es6.tokens;

version (unittest)
{
	import unit_threaded;
	import std.stdio;
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


struct Keywords
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
    
    static ushort hash(const char[] word) nothrow pure @safe @nogc
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
        const ushort h = hash(word);
        final switch(_filled[h])
        {
            case false: return word;
            case true:  
            	if (_words[h].length != word.length || _words[h] != word)
            		return word;
            	return _words[h];
        }
    }
    static Keyword get()(const string word)
    {
        const ushort h = hash(word);
        if (!_filled[h])
        	return Keyword.Unknown;
        auto type = _map[h];
        if (type == Keyword.Unknown || _words[h].length != word.length || _words[h] != word)
        	return Keyword.Unknown;
        return type;
    }
}

bool isReservedKeyword(string keyword)
{
	if (keyword.length < 2 || keyword.length > 10)
		return false;
	auto k = Keywords.get(keyword);
	return k != Keyword.Unknown &&
		k != Keyword.Static &&
		k != Keyword.Set &&
		k != Keyword.Let &&
		k != Keyword.Get;
}
bool isWhitespace(Char)(Char c)
{
	if (c < '\u0021')
		return c == '\u0009' || c == '\u000B' || c == '\u000C' || c == '\u0020';
	if (c == '\u00A0')
		return true;
	if (c < '\u02B0')
		return false;
	return (c == '\uFEFF' || (c >= '\u02B0' && c <= '\u02FF'));
}
@("isWhitespace")
unittest
{
	import std.algorithm : all;
	import std.range : iota;
	assert("\x09\x0b\x0c\x20\u00a0\ufeff".all!isWhitespace);
	assert(iota('\u02b0','\u02ff').all!isWhitespace);
}
auto next(Source)(ref Source s)
{
	auto chr = s.front();
	s.popFront();
	return chr;
}
bool popNextIf(Source, Elem)(ref Source s, Elem e, in string file = __FILE__, in size_t line = __LINE__)
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
auto isLineTerminator(Char)(Char s)
{
	return (s == '\u000A' || s == '\u000D' || s == '\u2028' || s == '\u2029');
}
@("isLineTerminator")
unittest
{
	import std.algorithm : all;
	"\u2028\u2029\u000A\u000D".all!(isLineTerminator).shouldBeTrue();
}
auto isStartIdentifier(Char)(Char chr)
{
	return chr == '$' || chr == '_' || (chr >= '\u0041' && chr <= '\u005a') || (chr >= '\u0061' && chr <= '\u007a') || chr == '\u00aa' ||
		chr == '\u00b5' || chr == '\u00ba' || (chr >= '\u00c0' && chr <= '\u00d6') || (chr >= '\u00d8' && chr <= '\u00f6') || chr == '\u2118' ||
		chr == '\u212e' || (chr >= '\u309b' && chr <= '\u309c');
}
auto isTailIdentifier(Char)(Char chr)
{
	return 
		(chr >= '\u0061' && chr <= '\u007a') || (chr >= '\u0041' && chr <= '\u005a') ||  (chr >= '\u0030' && chr <= '\u0039') || 
		(!isWhitespace(chr) && (
			chr == '$' || chr == '_' || chr == '\u005f' ||
			(chr >= '\u00c0' && chr <= '\u00d6') || (chr >= '\u00d8' && chr <= '\u00f6') ||
			chr == '\u2118' || chr == '\u212e' || (chr >= '\u309b' && chr <= '\u309c') || chr == '\u00aa' || 
			chr == '\u00b5' || chr == '\u00b7' || chr == '\u00ba' ||
			chr == '\u00b7' || chr == '\u0387' || (chr >= '\u1369' && chr <= '\u1371') || chr == '\u19da' || chr == '\u200C' ||
			chr == '\u200D')
		);
}
auto coerceToSingleQuotedString(string str)
{
	import std.algorithm : max,min;
	if (str.length == 0)
		return str;
	auto sink = appender!string;
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
	return sink.data;
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
}
class Lexer(Source)
{
	private
	{
		Appender!(State[]) lexerState;
	}
	State lexState;
	Token token;
	size_t line;
	size_t column;
	private size_t tokenLength;
	private bool newLine;
	Source s;
	void pushState(State state)
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
		auto l = new Lexer!(Source)(s);
		l.lexState = this.lexState;
		l.token = this.token;
		l.line = this.line;
		l.column = this.column;
		l.tokenLength = this.tokenLength;
		l.newLine = this.newLine;
		l.lexerState = appender!(State[])(this.lexerState.data.dup());
		return l;
	}
	this(Source source)
	{
		s = source;
		pushState(s.empty ? State.EndOfFile : State.TokensRemaining);
	}
	version(unittest)
	{
		auto scanToken(Goal goal = Goal.All, in string file = __FILE__, in size_t orgLine = __LINE__)
		{
			if (newLine)
			{
				line+=1;
				column=0;
				newLine = false;
			} else{
				column += tokenLength;
			}
			tokenLength = 0;
			token = lexToken(goal);
			version(chatty) { import std.stdio; writefln("Scan: %s with %s @%s:%s %s called from %s:%s",token, goal, line, column, tokenLength, file,orgLine); }
			return token;
		}
	} else
	{
		auto scanToken(Goal goal = Goal.All)
		{
			if (newLine)
			{
				line+=1;
				column=0;
				newLine = false;
			} else{
				column += tokenLength;
			}
			tokenLength = 0;
			token = lexToken(goal);
			version(chatty) { import std.stdio; writefln("Scan: %s with %s @%s:%s %s called",token, goal, line, column, tokenLength); }
			return token;
		}
	}
		
	Token lexUnicodeEscapeSequence(size_t idx)
	{
		import std.format : format;
		foreach(i; 0..4)
		{
			auto chr = s[idx+i];
			if ((chr <= '0' || chr >= '9') && (chr <= 'a' && chr >= 'z') && (chr <= 'A' && chr >= 'Z'))
			{
				if (chr == '\u0000')
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
		dchar chr = s[idx];
		tokenLength++;
		idx++;
		if (chr.isStartIdentifier)
			return Token(Type.StartIdentifier);
		if (chr == '\\')
		{
			chr = s[idx];
			idx++;
			tokenLength++;
			if (chr != 'u')
			{
				if (chr == '\u0000')
					return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
				return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr));
			}
			idx += 4;
			tokenLength += 4;
			return lexUnicodeEscapeSequence(idx-4);
		}
		return Token(Type.Error,format("Invalid character %s to start identifier",chr));
	}
	Token lexIdentifier()
	{
		import std.format : format;
		size_t idx = 0;
		auto t = lexStartIdentifier(idx);
		if (t.type == Type.Error)
		{
			s = s[idx..$];
			return t;
		}
		for(;;)
		{
			dchar chr = s[idx++];
			if (chr == '\\')
			{
				tokenLength++;
				chr = s[idx++];
				tokenLength++;
				if (chr != 'u')
				{
					s = s[idx..$];
					if (chr == '\u0000')
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
			} else if (!chr.isTailIdentifier)
			{
				auto tok = Token(Type.Identifier,s[0..idx-1]);
				s = s[idx-1..$];
				return tok;
			} else
				tokenLength++;
		}
	}
	void popWhitespace()
	{
		while (s.front.isWhitespace)
		{
			column++;
			s.popFront();
		}
	}
	auto lookAheadRegex()
	{
		import std.array : appender;
		auto str = appender!string;
		auto cpy = s.save();
		bool inOneOfExpr = false;
		str.put('/');
		cpy.popFront();
		auto incr = 1;
		if (cpy.front == '*' || cpy.front == '/')
			return "";
		while (true)
		{
			auto chr = cpy.next();
			if (chr.isLineTerminator)
				return "";
			if (chr == '\u0000')
				return "";
			incr++;
			if (chr == '[')
				inOneOfExpr = true;
			else if (chr == ']')
				inOneOfExpr = false;
			if (chr == '\\')
			{
				str.put(chr);
				chr = cpy.next;
				incr++;
				if (chr.isLineTerminator)
					return "";
				if (chr == '\u0000')
					return "";
				str.put(chr);
			} else if (!inOneOfExpr && chr == '/')
			{
				str.put(chr);
				while (true)
				{
					chr = cpy.front();
					if (chr.isTailIdentifier)
					{
						str.put(chr);
						cpy.popFront();
						incr++;
					} else if (chr == '\u0000')
						break;
					else
						break;
				}
				tokenLength += incr;
				s = s.drop(incr);
				return str.data;
			} else
				str.put(chr);
		}
	}
	Token lexString()
	{
		import std.array : appender;
		auto type = s.front();	// either " or '
		s.popFront();
		tokenLength++;
		auto str = appender!string;
		while (true)
		{
			auto chr = s.front();
			tokenLength++;
			if (chr == type)
			{
				s.popFront();
				if (type == '"')
					return Token(Type.StringLiteral,str.data.coerceToSingleQuotedString);
				return Token(Type.StringLiteral,str.data);
			}
			switch (chr)
			{
				case '\\':
					s.popFront();
					chr = s.front();
					if (chr == '\u0000')
						goto eof;
					if (chr != type)
						str.put('\\');
					s.popFront();
					tokenLength++;
					str.put(chr);
					if (chr == '\x0D' && s.popNextIf('\x0A'))
					{
						newLine=true;
						str.put('\x0A');
					}
					break;
				case '\u0000':
					goto eof;
				default:
					s.popFront();
					str.put(chr);
					break;
			}
		}
		eof: return Token(Type.Error,"Invalid eof while lexing string");
	}
	Token lexBaseLiteral(int base, alias TokenType)(bool isFloat = false)
	{
		static assert(base >= 1 && base <= 16,"Can only lex base 1..16 NumericLiterals");
		import std.array : appender;
		import std.format : format;
		auto str = appender!string;
		bool fraction = false;
		bool exponent = false;
		bool expectingNumbers = true;
		while (true)
		{
			auto chr = s.front();
			if (chr == '.' && isFloat)
			{
				if (fraction)
					return Token(TokenType,str.data);
				if (exponent)
					return Token(TokenType,str.data);
				str.put(chr);
				tokenLength++;
				s.popFront();
				fraction = true;
				continue;
			}
			if (!chr.isTailIdentifier)
				break;

			switch(chr)
			{
				case '0': .. case '9':
					auto digit = cast(int)(chr-'0');
					static if (base < 10)
					{
						if (digit >= base)
							return Token(Type.Error,format("Invalid digit %s in base %s NumericLiteral",chr,base));
					}
					str.put(chr);
					tokenLength++;
					s.popFront();
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
								return Token(Type.Error,format("Invalid digit %s in base %s NumericLiteral",chr,base));
						}
						str.put(chr);
						tokenLength++;
						s.popFront();
						break;
					}
				default:
					if (chr == '\u0000')
					{
						if (expectingNumbers)
							return Token(Type.Error,format("Invalid eof in base %s literal",base));
						return Token(TokenType,str.data);
					}
					static if (base == 10)
					{
						if (chr == 'e' || chr == 'E')
						{
							if (exponent)
								return Token(Type.Error,format("Already processed exponent part"));
							exponent = true;
							str.put(chr);
							tokenLength++;
							s.popFront();
							chr = s.front();
							if (chr == '+' || chr == '-')
							{
								str.put(chr);
								tokenLength++;
								s.popFront();
							}
							expectingNumbers = true;
							continue;
						}
					}
					return Token(Type.Error,format("Invalid char (%s) in NumericLiteral",chr));
			}
		}
		return Token(TokenType,str.data);
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
		auto str = appender!string;
		while (true)
		{
			auto chr = s.next();
			if (chr.isLineTerminator)
			{
				if (chr == '\u000D')
					s.popNextIf('\u000A');
				newLine=true;
			} else
				tokenLength++;
			if (chr == '$')
			{
				chr = s.next();
				tokenLength++;
				if (chr == '{')
				{
					pushState(State.LexingTemplateLiteral);
					return Token(Type.TemplateHead,str.data);
				}
				str.put('$');
			}
			if (chr == '\u0060')
				return Token(t,str.data);
			if (chr == '\u0000')
				goto eof;
			str.put(chr);
			if (chr == '\\')
			{
				tokenLength++;
				chr = s.next();
				if (chr == '\u0000')
					goto eof;
				str.put(chr);
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
				foreach(t; Ts)
				{
					case t:
						return true;
				}
					//version(LDC) break;
				default:
					return false;
			}
		}
	}
	Token lexMultiLineComment()
	{
		size_t idx = 0;
		for(;;)
		{
			dchar chr = s[idx];
			if (chr == '*')
			{
				column++;
				idx++;
				chr = s[idx];
				if (chr == '/')
				{
					auto tok = Token(Type.MultiLineComment,s[0..idx-1]);
					s = s[idx+1..$];
					return tok;
				}
				continue;
			}
			if (chr.isLineTerminator())
			{
				idx++;
				line++;
				column=0;
				tokenLength=0;
				if (chr == '\u000D' && s[idx] == '\u000A')
					idx++;
				continue;
			} else if (chr == '\u0000')
				goto eof;
			idx++;
			column++;
		}
		eof: 
		s = s[idx..$];
		return Token(Type.Error,"Expected end of MultiLineComment before eof");
	}
	Token lexSingleLineComment()
	{
		size_t idx = 0;
		for (;;)
		{
			dchar chr = s[idx];
			if (chr.isLineTerminator())
			{
				idx++;
				newLine=true;
				auto tok = Token(Type.SingleLineComment,s[0..idx-1]);
				if (chr == '\u000D' && s[idx] == '\u000A')
					idx++;
				s = s[idx..$];
				return tok;
			} else if (chr == '\u0000')
				break;
			idx++;
			tokenLength++;
		}
		auto tok = Token(Type.SingleLineComment,s[0..idx]);
		s = s[idx..$];
		return tok;
	}
	Token lexToken(Goal goal = Goal.All)
	{
		popWhitespace();
		auto chr = s.front();

		switch(chr)
		{
			case '\u000A':
			case '\u2028':
			case '\u2029':
			case '\u000D':
				newLine=true;
				s.popFront();
				if (chr == '\u000D')
					s.popNextIf('\u000A');
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
					auto regex = lookAheadRegex;
					if (!regex.empty)
						return Token(Type.Regex,regex.dup);					
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
				if (s.front() == '\u0000')
					return Token(Type.EndOfFile);
				return Token(Type.Error,"Not expecting NumericLiteral to start with 0");
			case '1': .. case '9':
				return lexDecimalLiteral;
			case '`':
				tokenLength++;
				s.popFront();
				return lexTemplateLiteral();
			case '\u0000':
				return Token(Type.EndOfFile);
			default:
				return lexIdentifier();
		}
	}
}
auto createLexer(string s)
{
	auto input = s ~ "\u0000";
	return new Lexer!(string)(input);
}
@("lexIdentifier")
unittest
{
	// TODO: need to test unicode escape sequence stuff
	auto lexer = createLexer("abcde");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"abcde"));
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
	lexer.lookAheadRegex.shouldEqual("/abcd/");
	lexer = createLexer(`/ab\/ab/`);
	lexer.lookAheadRegex.shouldEqual(`/ab\/ab/`);
	lexer = createLexer(`/also no
		regex`);
	lexer.scanToken.shouldEqual(Token(Type.Division));
	lexer.empty.shouldBeFalse;
	lexer.column.shouldEqual(0);
	lexer = createLexer(`/regex with modifiers/gi;`);
	lexer.lookAheadRegex.shouldEqual("/regex with modifiers/gi");
	lexer.scanToken.shouldEqual(Token(Type.Semicolon));
	lexer = createLexer("/ab[^/]cd/");
	lexer.lookAheadRegex.shouldEqual("/ab[^/]cd/");
	lexer = createLexer(`/ab[^/\\]cd/`);
	lexer.lookAheadRegex.shouldEqual(`/ab[^/\\]cd/`);
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
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"escaped \'string\'"));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
	lexer = createLexer(`"\xaa\xb5"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`\xaa\xb5`));
	lexer.scanToken().shouldEqual(Token(Type.EndOfFile));
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
}
@("lexHexLiteral")
unittest
{
	auto lexer = createLexer("04af");
	lexer.lexHexLiteral().shouldEqual(Token(Type.HexLiteral,"04af"));
	lexer = createLexer("04afg");
	lexer.lexHexLiteral().shouldEqual(Token(Type.Error,"Invalid char (g) in NumericLiteral"));
}
@("lexTemplateLiteral")
unittest
{
	auto lexer = createLexer("some\n$template\\\u0060\rstring\u0060");
	lexer.lexTemplateLiteral.shouldEqual(Token(Type.Template,"some\n$template\\\u0060\rstring"));
	lexer = createLexer("some\n$template\\\u0060\rstring${identifier}\u0060");
	lexer.lexTemplateLiteral.shouldEqual(Token(Type.TemplateHead,"some\n$template\\\u0060\rstring"));
	lexer.s.shouldEqual("identifier}\u0060\u0000");

	lexer = createLexer("\u0060some\n$template\\\u0060\rstring${identifier}\u0060");
	lexer.lexToken().shouldEqual(Token(Type.TemplateHead,"some\n$template\\\u0060\rstring"));
	lexer.lexToken().shouldEqual(Token(Type.Identifier,"identifier"));
	lexer.lexToken().shouldEqual(Token(Type.TemplateTail,""));
}
@("comments")
unittest
{
	auto lexer = createLexer("// comment \nidentifier");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment," comment "));
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"identifier"));

	lexer = createLexer("// comment");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment," comment"));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("//");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));
	
	lexer = createLexer("//\r\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));
	
	lexer = createLexer("//\n");
	lexer.lexToken.shouldEqual(Token(Type.SingleLineComment,""));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/* multi \n line \r\n\n comment */ identifier");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment," multi \n line \r\n\n comment "));
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"identifier"));

	lexer = createLexer("/**/");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment,""));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/*a*b*/");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment,"a*b"));
	lexer.s.shouldEqual("\u0000");
	lexer.lexToken.shouldEqual(Token(Type.EndOfFile));

	lexer = createLexer("/*");
	lexer.lexToken.shouldEqual(Token(Type.Error,"Expected end of MultiLineComment before eof"));
	lexer.s.shouldEqual("\u0000");
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
	lexer2.column.shouldEqual(12);
	lexer.scanToken().shouldEqual(Token(Type.LineTerminator));
	lexer.scanToken().shouldEqual(Token(Type.MultiLineComment," comment "));
	lexer.scanToken().shouldEqual(Token(Type.Comma));
	lexer.column.shouldEqual(12);
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
	isReservedKeyword("await").shouldBeTrue;
	isReservedKeyword("default").shouldBeTrue;
	isReservedKeyword("default2").shouldBeFalse;
	isReservedKeyword("yield").shouldBeTrue;
	isReservedKeyword("").shouldBeFalse;
	isReservedKeyword("a").shouldBeFalse;
}
@("scanToken")
unittest
{
	auto lexer = createLexer(".5");
	lexer.scanToken().shouldEqual(Token(Type.DecimalLiteral,".5"));
}