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
module es6.tokens;

import es6.bench;

@safe:

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
}

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
    static Keyword get()(const(ubyte)[] word) nothrow
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

enum Type {
	Identifier,
	SheBang,
	StringLiteral,
	Error,
	BinaryLiteral,
	OctalLiteral,
	DecimalLiteral,
	HexLiteral,
	TemplateHead,
	TemplateMiddle,
	Template,
	TemplateTail,
	EndOfFile,
	LineTerminator,
	OpenCurlyBrace,
	CloseCurlyBrace,
	OpenParenthesis,
	CloseParenthesis,
	OpenSquareBrackets,
	CloseSquareBrackets,
	Dot,
	SpreadOperator,
	Semicolon,
	Comma,
	LeftShiftAssignment,
	LeftShift,
	LessOrEqual,
	LessThan,
	TripleRightShiftAssignment,
	TripleRightSift,
	RightShiftAssignment,
	RightShift,
	GreaterOrEqual,
	GreaterThan,
	StrictEqual,
	Equal,
	Arrow,
	Assignment,
	StrictNotEqual,
	NotEqual,
	Negation,
	Increment,
	AdditiveAssignment,
	Add,
	Decrement,
	DecrementalAssignment,
	Minus,
	MultiplicativeAssignment,
	Multiply,
	DivisionAssignment,
	Division,
	ModAssignment,
	Mod,
	LogicalAnd,
	BitwiseAndAssignment,
	BitwiseAnd,
	LogicalOr,
	BitwiseOrAssignment,
	BitwiseOr,
	BitwiseXorAssignment,
	BitwiseXor,
	Tilde,
	QuestionMark,
	Colon,
	Regex,
	SingleLineComment,
	MultiLineComment,
	ExponentIndicator,
	UnicodeEscapeSequence,
	StartIdentifier,
	TailIdentifier,
	EndIdentifier,
	InvalidUTF8
}

struct Token
{
	Type type;
	Keyword keyword = Keyword.Unknown;
	const (ubyte)[] match;
	this(Type type) nothrow
	{
		this.type = type;
	}
	this(Type type, string match) nothrow
	{
		this(type, cast(const(ubyte)[])match);
	}
	this(Type type, const (ubyte)[] match) nothrow
	{
		this.type = type;
		this.match = cast(const(ubyte)[])match;
		if (this.type == Type.Identifier)
			keyword = matchKeyword(match);
	}
	string toString()
	{
		import std.format : format;
		return format("Token(%s,%s)",type,cast(const(char)[])match);
	}
}

bool matches(Token tok, Type t) {
	return tok.type == t;
}
bool matches(Token tok, Type t, const (ubyte)[] m) {
	return tok.type == t && tok.match == m;
}
bool matches(Token tok, Type t, string m) {
	return tok.type == t && tok.match == cast(const(ubyte)[])m;
}