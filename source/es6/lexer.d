module es6.lexer;

import std.range : empty, front, popFront, save, take, drop;
import std.array : Appender, appender;
import es6.tokens;

version (unittest)
{
	import unit_threaded;
}
enum State
{
	TokensRemaining,
	EndOfFile,
	LexingTemplateLiteral
}

bool isWhitespace(Char)(Char c)
{
	return (c == '\u0009' || c == '\u000B' || c == '\u000C' || c == '\u0020' || c == '\u00A0' || c == '\uFEFF' || (c >= '\u02B0' && c <= '\u02FF'));
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
	import std.format : format;
	assert(!s.empty,format("Invalid popNextIf from %s @ %s",file,line));
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
	return (s == '\u000A' || s == '\u2028' || s == '\u2029' || s == '\u000D');
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
	return chr == '\u2118' || chr == '\u212e' || (chr >= '\u309b' && chr <= '\u309c') || (chr >= '\u0030' && chr <= '\u0039') || 
		(chr >= '\u0041' && chr <= '\u005a') || chr == '\u005f' || (chr >= '\u0061' && chr <= '\u007a') || chr == '\u00aa' || 
		chr == '\u00b5' || chr == '\u00b7' || chr == '\u00ba' || (chr >= '\u00c0' && chr <= '\u00d6') || (chr >= '\u00d8' && chr <= '\u00f6') ||
		chr == '\u00b7' || chr == '\u0387' || (chr >= '\u1369' && chr <= '\u1371') || chr == '\u19da' || chr == '$' || chr == '_' || chr == '\u200C' ||
		chr == '\u200D';
}
auto coerceToSingleQuotedString(string str)
{
	if (str.length == 0)
		return str;
	import std.regex;
	auto escapedDouble = ctRegex!(`\\"`);
	auto unescapedSingle = ctRegex!(`(?<!\\)'`);
	//unescape "
	str = replaceAll(str,escapedDouble,"\"");
	//escape '
	str = replaceAll(str,unescapedSingle,"\\\'");
	return str;
}
unittest
{
	assert("".coerceToSingleQuotedString == "");
	assert(`isn't it nice`.coerceToSingleQuotedString == `isn\'t it nice`);
	assert(`a 'good' way to go`.coerceToSingleQuotedString == `a \'good\' way to go`);
	"a \\\"good\\\" way to go".coerceToSingleQuotedString.shouldEqual(`a "good" way to go`);
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
	auto scanToken(in string file = __FILE__, in size_t l = __LINE__)
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
		token = lexToken();
		//import std.stdio;
		//writeln("Scan: ",token," ",file,"@",l);
		return token;
	}
	Token lexUnicodeEscapeSequence(Sink)(ref Sink sink)
	{
		import std.format : format;
		foreach(_; 0..4)
		{
			if (s.empty)
				return Token(Type.Error,"Found eof before finishing parsing UnicodeEscapeSequence");
			auto chr = s.next();
			tokenLength++;
			if ((chr <= '0' || chr >= '9') && (chr <= 'a' && chr >= 'z') && (chr <= 'A' && chr >= 'Z'))
			{
				// TODO: if chr is lineterminator (as well as the \r\n one) we need to reset column and line++
				return Token(Type.Error,format("Invalid hexdigit %s for UnicodeEscapeSequence",chr));
			}
			sink.put(chr);
		}
		return Token(Type.UnicodeEscapeSequence);
	}
	Token lexStartIdentifier(Sink)(ref Sink sink)
	{
		import std.format : format;
		auto chr = s.next();
		tokenLength++;
		if (chr == '\\')
		{
			if (s.empty)
				return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
			chr = s.next();
			tokenLength++;
			if (chr != 'u')
				return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr));
			return lexUnicodeEscapeSequence(sink);
		}

		if (chr.isStartIdentifier)
		{
			sink.put(chr);
			return Token(Type.StartIdentifier);
		}
		return Token(Type.Error,format("Invalid character %s to start identifier",chr));
	}
	Token lexTailIdentifier(Sink)(ref Sink sink)
	{
		import std.format : format;
		if (s.empty)
			return Token(Type.EndIdentifier);
		auto chr = s.front();
		if (chr == '\\')
		{
			s.popFront();
			tokenLength++;
			if (s.empty)
				return Token(Type.Error,"Invalid eof at UnicodeEscapeSequence");
			chr = s.next();
			tokenLength++;
			if (chr != 'u')
				return Token(Type.Error,format("Invalid escaped char (%s) at UnicodeEscapeSequence",chr));
			return lexUnicodeEscapeSequence(sink);
		}
		if (chr.isTailIdentifier)
		{
			s.popFront();
			tokenLength++;
			sink.put(chr);
			return Token(Type.TailIdentifier);
		}
		return Token(Type.EndIdentifier);
	}
	Token lexIdentifier()
	{
		import std.array : appender;
		auto str = appender!string;
		lexStartIdentifier(str);
		while(1)
		{
			auto t = lexTailIdentifier(str);
			if (t.type == Type.Error)
				return t;
			if (t.type == Type.EndIdentifier)
				return Token(Type.Identifier,str.data);
		}
	}
	auto popWhitespace()
	{
		while (!s.empty)
		{
			if (!s.front.isWhitespace)
				return State.TokensRemaining;
			column++;
			s.popFront();
		}
		return State.EndOfFile;
	}
	auto lookAheadRegex()
	{
		import std.array : appender;
		auto str = appender!string;
		auto cpy = s.save();
		str.put('/');
		cpy.popFront();
		auto incr = 1;
		if (cpy.front == '*' || cpy.front == '/')
			return s[0..0];
		while (!cpy.empty)
		{
			auto chr = cpy.next();
			if (chr.isLineTerminator)
				return s[0..0];
			incr++;
			if (chr == '\\')
			{
				if (cpy.empty)
					return s[0..0];
				str.put(chr);
				chr = cpy.next;
				incr++;
				if (chr.isLineTerminator)
					return s[0..0];
				str.put(chr);
			} else if (chr == '/')
			{
				str.put(chr);
				while (!cpy.empty)
				{
					chr = cpy.front();
					if (chr.isTailIdentifier)
					{
						str.put(chr);
						cpy.popFront();
						incr++;
					} else
						break;
				}
				column += incr;
				s = s.drop(str.data.length);
				return str.data;
			} else
				str.put(chr);
		}
		return s[0..0];
	}
	Token lexString()
	{
		import std.array : appender;
		auto type = s.front();	// either " or '
		s.popFront();
		tokenLength++;
		auto str = appender!string;
		while (!s.empty)
		{
			auto chr = s.next();
			tokenLength++;
			if (chr == type)
			{
				if (type == '"')
					return Token(Type.StringLiteral,str.data.coerceToSingleQuotedString);
				return Token(Type.StringLiteral,str.data);
			}
			switch (chr)
			{
				case '\\': 
					if (s.empty)
						goto eof;
					chr = s.next();
					tokenLength++;
					str.put(chr);
					if (chr == '\x0D' && s.popNextIf('\x0A'))
					{
						newLine=true;
						str.put('\x0A');
					}
					break;
				default:
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
		while (!s.empty)
		{
			auto chr = s.front();
			if (chr == '.' && isFloat)
			{
				if (fraction)
					return Token(Type.Error,format("Already processed fractional part"));
				if (exponent)
					return Token(Type.Error,format("Currently processing exponential part"));
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
					break;
				case 'A': .. case 'F':
				case 'a': .. case 'f':
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
							if (s.empty)
								return Token(Type.Error,format("Expected exponent part before eof"));
							chr = s.front();
							if (chr == '+' || chr == '-')
							{
								str.put(chr);
								tokenLength++;
								s.popFront();
								if (s.empty)
									return Token(Type.Error,format("Expected exponent part before eof"));
							}
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
		while (!s.empty)
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
				if (s.empty)
					goto eof;
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
			str.put(chr);
			if (chr == '\\')
			{
				tokenLength++;
				str.put(s.next);
			}
		}
		eof: return Token(Type.Error,"Found eof before finishing lexing template");
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
					version(LDC) break;
				default:
					return false;
			}
		}
	}
	Token lexMultiLineComment()
	{
		auto str = appender!string;
		while (!s.empty)
		{
			auto chr = s.front();
			if (chr == '*')
			{
				s.popFront();
				column++;
				if (s.empty)
					break;
				chr = s.front();
				if (chr == '/')
				{
					s.popFront();
					return Token(Type.MultiLineComment,str.data);
				}
				str.put('*');
			}
			if (chr.isLineTerminator())
			{
				str.put(chr);
				s.popFront();
				line++;
				column=0;
				tokenLength=0;

				if (!s.empty && chr == '\u000D' && s.popNextIf('\u000A'))
					str.put('\u000A');
				//s.popFront();
				continue;
			}
			str.put(chr);
			s.popFront();
			column++;
		}
		// TODO eat chars until */ or eof
		return Token(Type.Error,"Expected end of MultiLineComment before eof");
	}
	Token lexSingleLineComment()
	{
		auto str = appender!string;
		while (!s.empty)
		{
			auto chr = s.front();
			if (chr.isLineTerminator())
			{
				s.popFront();
				newLine=true;
				if (!s.empty && chr == '\u000D')
					s.popNextIf('\u000A');
				break;
			}
			str.put(chr);
			s.popFront();
			tokenLength++;
		}
		return Token(Type.SingleLineComment,str.data);
	}
	Token lexToken()
	{
		if (popWhitespace() == State.EndOfFile)
			return Token(Type.EndOfFile);
		auto chr = s.front();

		switch(chr)
		{
			case '\u000A':
			case '\u2028':
			case '\u2029':
			case '\u000D':
				newLine=true;
				s.popFront();
				if (!s.empty && chr == '\u000D')
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
				auto regex = lookAheadRegex;
				if (!regex.empty)
					return Token(Type.Regex,regex.dup);
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
				if (s.empty)
					return Token(Type.DecimalLiteral,"0");
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
			default:
				return lexIdentifier();
		}
	}
}
auto createLexer(Source)(Source s)
{
	return new Lexer!Source(s);
}
@("lexIdentifier")
unittest
{
	auto lexer = createLexer("abcde");
	lexer.lexIdentifier.shouldEqual(Token(Type.Identifier,"abcde"));
}
@("popWhitespace")
unittest
{
	auto lexer = createLexer("  \u0009abc");
	lexer.popWhitespace.shouldEqual(State.TokensRemaining);
	lexer.column.shouldEqual(3);
	lexer = createLexer("  ");
	lexer.popWhitespace.shouldEqual(State.EndOfFile);
	lexer.column.shouldEqual(2);
}
@("lookAheadRegex")
unittest
{
	auto lexer = createLexer("/abcd/");
	lexer.lookAheadRegex.shouldEqual("/abcd/");
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer("no regex");
	lexer.lookAheadRegex.empty.shouldBeTrue();
	lexer.s.shouldEqual("no regex");
	lexer = createLexer(`/ab\/ab/`);
	lexer.lookAheadRegex.shouldEqual(`/ab\/ab/`);
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer(`/also no
		regex`);
	lexer.lookAheadRegex.empty.shouldBeTrue();
	lexer.empty.shouldBeFalse;
	lexer.column.shouldEqual(0);
	lexer = createLexer(`/regex with modifiers/gi;`);
	lexer.lookAheadRegex.shouldEqual("/regex with modifiers/gi");
	lexer.s.shouldEqual(";");
}
@("lexString")
unittest
{
	auto lexer = createLexer(`"a string"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"a string"));
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer(`"a stringin'"`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"a stringin\\'"));
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer(`'another string'`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"another string"));
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer(`"escaped \"string\""`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,`escaped "string"`));
	lexer.s.empty.shouldBeTrue;
	lexer = createLexer(`'escaped \'string\''`);
	lexer.lexString.shouldEqual(Token(Type.StringLiteral,"escaped \'string\'"));
	lexer.s.empty.shouldBeTrue;
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
	lexer.s.shouldEqual("identifier}\u0060");

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

	lexer = createLexer("/* multi \n line \r\n\n comment */ identifier");
	lexer.lexToken.shouldEqual(Token(Type.MultiLineComment," multi \n line \r\n\n comment "));
	lexer.lexToken.shouldEqual(Token(Type.Identifier,"identifier"));
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