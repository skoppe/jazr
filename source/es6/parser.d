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
module es6.parser;

@safe:

import std.format : format;

import es6.lexer;
import es6.tokens;
import es6.nodes;
import es6.keywords;
import es6.allocator;
import std.array : appender;
import es6.bench;

version = customallocator;

version(chatty)
{
	version = tracing;
	import std.stdio;
}
version(tracing)
{
	import std.datetime : StopWatch;
	import es6.bench;
}
version (unittest)
{
	import es6.testhelpers;
	import unit_threaded;
	import es6.reporter;
	import std.stdio;
	import std.conv : text;
	auto parseNode(alias parseFunction, Type = ModuleNode, int flags = Parser.Flags.None, bool prettyError = true)(string r, bool exhaustInput = true, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r, flags);
		parser.scanToken();
		auto n = __traits(getMember, parser, parseFunction)();
		if (n.type == NodeType.ErrorNode)
		{
			auto err = n.shouldBeOfType!(ErrorNode);
			static if (prettyError)
				throw new UnitTestException([generateErrorMessage(err,r,1)],file,line);
			else
				throw new UnitTestException(err.value.text,file,line);
		}
		auto errs = n.collectErrors();
		if (errs.length > 0)
		{
			static if (prettyError)
				throw new UnitTestException([generateErrorMessage(errs[0],r,1)],file,line);
			else
				throw new UnitTestException(errs[0].value.text,file,line);
		}
		if (exhaustInput)
		{
			if (n.type != NodeType.ErrorNode && !parser.lexer.empty)
				throw new UnitTestException([format("Expected input to be empty, got %s",cast(const(ubyte)[])parser.lexer.s)],file,line);
		} else {
			if (n.type != NodeType.ErrorNode && parser.lexer.empty)
				throw new UnitTestException([format("Expected input not to be empty")],file,line);
		}
		return shouldBeOfType!(Type)(n,file,line);
	}
	alias parseModule = parseNode!("parseModule",ModuleNode);
}
enum Attribute
{
	None = 0,
	In = 1,
	Yield = 1 << 1,
	Return = 1 << 2,
	Default = 1 << 3,
	NoRegex = 1 << 4
}
private int orAttributes(Ts...)()
{
	int r = 0;
	foreach(T; Ts)
		r |= T;
	return r;
}
int mask(Ts...)(int attr)
{
	enum mask = orAttributes!Ts;
	return attr & mask;
}
int filter(Ts...)(int attr)
{
	enum mask = orAttributes!Ts;
	return attr & ~mask;
}
private bool has(alias a)(int attribute)
{
	return (attribute & a) == a;
}
@("Attribute")
unittest
{
	int a = Goal.In | Goal.Yield | Goal.Return | Goal.Default;
	a.has!(Goal.In).shouldBeTrue;
	a.has!(Goal.Yield).shouldBeTrue;
	a.has!(Goal.Return).shouldBeTrue;
	a.has!(Goal.Default).shouldBeTrue;
	orAttributes!(Goal.In,Goal.Yield).shouldEqual(Goal.In | Goal.Yield);
	a.mask!(Goal.In).has!(Goal.In).shouldBeTrue;
	a.mask!(Goal.In).has!(Goal.Yield).shouldBeFalse;
	a.mask!(Goal.In,Goal.Yield).has!(Goal.Yield).shouldBeTrue;
	a.mask!(Goal.In,Goal.Yield).has!(Goal.Return).shouldBeFalse;
	a.mask!(Goal.In,Goal.Yield,Goal.Return).has!(Goal.Return).shouldBeTrue;
}
final class Parser
{
	enum Flags {
		None = 0,
		Node = 1<<0
	}
	private {
		Lexer lexer;
		Token token;
		int flags;
		int rootAttributes;
		ulong _nodeCnt;
		version(customallocator)
		{
			pragma(msg, "Using Custom Allocator");
			PointerBumpAllocator allocator;
		}
		auto make(T, Args...)(auto ref Args args)
		{
			version (chatty) {
				static if (is(T : ErrorNode))
				{
					writefln("%sError: %s",traceIndent, args[0]);
				} else
					writefln("%sCreate %s",traceIndent, T.stringof);
			}
			//return measure!("Construct Node",(){
				version(customallocator)
				{
					return allocator.make!(T)(args);
				} else {
					return new T(args);
				}
			//});
		}
	}
	version (tracing) {
		size_t traceDepth;
		string traceIndent() {
			import std.range : repeat;
			import std.conv : to;
			import std.array : array;
			return ' '.repeat(traceDepth*2).array.to!string;
		}
		auto traceEnter(string fun)
		{
			version (chatty) { writeln(traceIndent,fun); }
			traceDepth++;
			return startCounter(fun);
		}
		void traceExit(string fun, long a)
		{
			stopCounter(fun, a);
			version (chatty) { writefln("%s%s: (%susecs)",traceIndent,fun,a/10); }
			traceDepth--;
		}
	}
	template traceFunction(string fun)
	{
		version (tracing) {
			import std.datetime : StopWatch;
			enum traceFunction = 
				`auto start = traceEnter("` ~ fun ~ `");
				scope(exit) {
					traceExit("`~fun~`",start);
				}`;
		} else enum traceFunction = "";
	}
	@property ulong nodeCnt() { return _nodeCnt; }
	this(const (ubyte)[] s, int f = Flags.None)
	{
		lexer = Lexer(s);
		flags = f;
		if (flags & Flags.Node)
			rootAttributes = rootAttributes | Goal.Return;
	}
	version (unittest)
	{
		Token scanToken(int goal = Goal.None, in size_t at = __LINE__)
		{
			token = lexer.scanToken(cast(Goal)goal,__FILE__,at);
			return token;
		}
	} else
	{
		Token scanToken(int goal = Goal.None)
		{
			token = lexer.scanToken(cast(Goal)goal);
			return token;
		}
	}
	Node error(const(char)[] message, in size_t at = __LINE__)
	{
		string debugMessage;
		version(unittest)
		{
			debugMessage = format("in parser.d @ %s",at);
		}
		return make!(ErrorNode)(message,lexer.line,lexer.column,debugMessage,__FILE__,at);
	}
	Node error(Node[] children, const(char)[] message, in size_t at = __LINE__)
	{
		string debugMessage;
		version(unittest)
		{
			debugMessage = format("in parser.d @ %s",at);
		}
		auto err = make!(ErrorNode)(message,lexer.line,lexer.column,debugMessage,__FILE__,at);
		err.children = children;
		return err;
	}
	Node error(const(ubyte)[] message, in size_t at = __LINE__) @trusted
	{
		return error(cast(const(char)[])message,at);
	}
	Node parseModule()
	{
		mixin(traceFunction!(__FUNCTION__));
		auto children = appender!(Node[]);
		bool needsSeparator = false;
		outer: while(token.type != Type.EndOfFile)
		{
			switch(token.type)
			{
				case Type.Identifier:
					if (token.match == "import")
						children.put(parseImportDeclaration());
					else if (token.match == "export")
						children.put(parseExportDeclaration());
					else
						goto default;
					needsSeparator = true;
					break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
				case Type.Semicolon:
					scanAndSkipCommentsAndTerminators();
					needsSeparator = false;
					break;
				case Type.SheBang:
					if (children.data.length == 0)
					{
						children.put(new SheBangNode(token.match));
					}
					else
					{
						children.put(error("SheBang line can only be at the first line"));
					}
					scanToken();
					break;
				case Type.InvalidUTF8:
					children.put(error("Invalid UTF8"));
					break outer;
				default:
					// TODO: we can avoid many calls to requireSeparator by assuming we need one. Then when we get here and needsSeparator and !recentSeparator are still true, only then call requiresSeparator on last child
					if (needsSeparator && !lexer.recentSeparator)
						children.put(error("Expected newline or semicolon"));
					auto item = parseStatementListItem(rootAttributes);
					children.put(item);
					needsSeparator = item.requiresSeparator();
					break;
			}
		}
		return make!(ModuleNode)(children.data);
	}
	Node parseImportDeclaration()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.matches(Type.Identifier,"import"));
		scanAndSkipCommentsAndTerminators();
		Node decl;
		if (token.type == Type.StringLiteral)
		{
			auto strLiteral = make!(StringLiteralNode)(token.match);
			decl = make!(ImportDeclarationNode)(strLiteral);
			scanAndSkipCommentsAndTerminators();
		} else
		{
			Node clause;
			if (token.type == Type.Multiply)
			{
				clause = parseNameSpaceImport();
			} else if (token.type == Type.OpenCurlyBrace)
			{
				clause = parseNamedImports();
			} else if (token.type == Type.Identifier)
			{
				clause = parseIdentifier();
				if (token.type == Type.Comma)
				{
					scanAndSkipCommentsAndTerminators();
					if (token.type == Type.Multiply)
						clause = make!(ImportClauseNode)(clause,parseNameSpaceImport());
					else
						clause = make!(ImportClauseNode)(clause,parseNamedImports());
				}
			} else
				return error(format("Unexpected %s as part of ImportDeclaration",token.type));

			if (token.type != Type.Identifier && token.match != "from")
				return error("Expected from as part of ImportDeclaration");
			
			scanAndSkipCommentsAndTerminators();
			if (token.type != Type.StringLiteral)
				return error("Expected StringLiteral as part of ImportDeclaration");
			auto strLiteral = make!(StringLiteralNode)(token.match);
			decl = make!(ImportDeclarationNode)(clause,strLiteral);
			scanAndSkipCommentsAndTerminators();
		}
		if (token.type == Type.Semicolon)
			scanAndSkipCommentsAndTerminators();
		else
			skipCommentsAndLineTerminators();

		return decl;
	}
	Node parseNameSpaceImport()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Multiply);
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.Identifier || token.match != "as")
			return error("Expected NameSpaceImport");
		scanAndSkipCommentsAndTerminators();
		return make!(NameSpaceImportNode)(parseIdentifier());
	}
	Node parseNamedImports()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		Node[] imports;
		while (1) // this while loop can't run forever
		{
			auto name = parseIdentifierName();
			if (token.type == Type.Identifier && token.match == "as")
			{	
				scanAndSkipCommentsAndTerminators();
				imports ~= make!(ImportSpecifierNode)(name,parseIdentifier());
			} else
			{
				auto iden = cast(IdentifierNameNode)name;
				assert(iden !is null);
				if (isIdentifierReservedKeyword(iden.identifier))
					return error(format("ImportBinding cannot contain reserved keyword %s",iden.identifier));
				imports ~= iden;
			}
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanAndSkipCommentsAndTerminators();
		return make!(NamedImportsNode)(imports);
	}
	Node parseExportDeclaration()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "export");
		scanAndSkipCommentsAndTerminators();
		Node decl;
		if (token.type == Type.OpenCurlyBrace || token.type == Type.Multiply)
		{
			if (token.type == Type.OpenCurlyBrace)
			{
				auto clause = parseExportClause();
				if (token.match == "from")
				{
					scanAndSkipCommentsAndTerminators();

					if (token.type != Type.StringLiteral)
						return error("Expected StringLiteral as part of ExportDeclaration");
					auto specifier = make!(StringLiteralNode)(token.match);
					scanAndSkipCommentsAndTerminators();
					decl = make!(ExportDeclarationNode)(clause,specifier);
				} else
					decl = make!(ExportDeclarationNode)(clause);
			}
			else 
			{
				scanAndSkipCommentsAndTerminators();
				if (token.match != "from")
					return error("Expected from as part of ExportDeclaration");
				scanAndSkipCommentsAndTerminators();
				if (token.type != Type.StringLiteral)
					return error("Expected StringLiteral as part of ExportDeclaration");
				auto specifier = make!(StringLiteralNode)(token.match);
				scanAndSkipCommentsAndTerminators();
				auto exprOp = make!(ExpressionOperatorNode)(ExpressionOperator.Multiply);
				decl = make!(ExportDeclarationNode)(exprOp,specifier);
			}
		} else if (token.type == Type.Identifier && token.match == "default")
		{
			scanAndSkipCommentsAndTerminators();
			switch(token.keyword)
			{
				case Keyword.Function:
					decl = make!(ExportDefaultDeclarationNode)(parseFunctionDeclaration(Goal.Default));
					break;
				case Keyword.Class:
					decl = make!(ExportDefaultDeclarationNode)(parseClassDeclaration(Goal.Default));
					break;
				default:
					decl = make!(ExportDefaultDeclarationNode)(parseAssignmentExpression(Goal.In));
					break;
			}
		} else if (token.match == "var")
		{
			decl = make!(ExportDeclarationNode)(parseVariableStatement());
		} else if (token.match == "function")
			decl = make!(ExportDeclarationNode)(parseFunctionDeclaration(Goal.Default));
		else if (token.match == "class")
			decl = make!(ExportDeclarationNode)(parseClassDeclaration(Goal.Default));
		else if (token.match == "let" || token.match == "const")
			decl = make!(ExportDeclarationNode)(parseLexicalDeclaration(Goal.In));
		else
			return error(format("Invalid %s as part of ExportDeclaration",token.type));

		if (token.type == Type.Semicolon)
			scanAndSkipCommentsAndTerminators();
		else
			skipCommentsAndLineTerminators();

		return decl;
	}
	Node parseExportClause()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
		{
			Node[] nodes;
			return make!(ExportClauseNode)(nodes);
		}
		Node[] children;
		while(1)
		{
			auto name = parseIdentifierName();
			if (token.match == "as")
			{
				scanAndSkipCommentsAndTerminators();
				auto as = parseIdentifier();
				children ~= make!(ExportSpecifierNode)(name,as);
			} else
				children ~= name;
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace as part of ExportClause");
		scanAndSkipCommentsAndTerminators();
		return make!(ExportClauseNode)(children);
	}
	Node parseFunctionExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "function");
		scanAndSkipCommentsAndTerminators();
		bool generator = false;
		if (token.type == Type.Multiply)
		{
			generator =  true;
			scanAndSkipCommentsAndTerminators();
		}
		Node name = null;
		if (token.type == Type.Identifier)
		{
			name = parseIdentifier(attributes);
			skipCommentsAndLineTerminators();
		}
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of function declaration");
		scanToken(attributes);
		auto params = parseFormalParameterList();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of function declaration");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		int attr = Goal.Return;
		if (generator)
			attr |= Goal.Yield;
		auto funcBody = parseFunctionBody(attr);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanToken();
		if (generator)
			return make!(GeneratorExpressionNode)(name,params,funcBody);
		return make!(FunctionExpressionNode)(name,params,funcBody);
	}
	Node parseClassExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		return parseClassDeclaration(attributes);
	}
	Node parseArrayLiteral(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenSquareBrackets);
		ArrayBuilder!Node children;
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.Comma)
		{
			children.put(parseElision());
		}
		if (token.type == Type.CloseSquareBrackets)
		{
			scanAndSkipCommentsAndTerminators();
			return make!(ArrayLiteralNode)(children.data);
		}

		while (1) // this loop can't run forever
		{
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children.put(make!(SpreadElementNode)(parseAssignmentExpression(Goal.In | attributes)));
			} else
				children.put(parseAssignmentExpression(Goal.In | attributes));
			if (token.type != Type.Comma)
				break;

			scanAndSkipCommentsAndTerminators();

			if (token.type == Type.CloseSquareBrackets)
				break;

			if (token.type == Type.Comma)
			{
				children.put(parseElision());
				skipCommentsAndLineTerminators();
			}
		}

		if (token.type != Type.CloseSquareBrackets)
			return error("Unexpected eof in ArrayLiteral");

		scanAndSkipCommentsAndTerminators(attributes);
		return make!(ArrayLiteralNode)(children.data);
	}
	Node parseElision()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Comma);
		int cnt = 0;
		while(token.type == Type.Comma)
		{
			cnt++;
			scanAndSkipCommentsAndTerminators();
		}
		return make!(ElisionNode)(cnt);
	}
	Node parseObjectLiteral(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
		{
			scanAndSkipCommentsAndTerminators();
			Node[] nodes;
			return make!(ObjectLiteralNode)(nodes);
		}
		ArrayBuilder!Node children;
		enum staticAttr = false;
		while(1)
		{
			switch(token.type)
			{
				case Type.OpenSquareBrackets:
				case Type.StringLiteral:
				case Type.DecimalLiteral:
				case Type.OctalLiteral:
				case Type.HexLiteral:
				case Type.BinaryLiteral:
					auto name = parsePropertyName();
					skipCommentsAndLineTerminators();
					if (token.type == Type.OpenParenthesis)
					{
						children.put(parseClassMethod(staticAttr,attributes.mask!(Goal.Yield),name));
						break;
					}
					if (token.type != Type.Colon)
						return error("Expected colon as part of PropertyDefinition");
					scanAndSkipCommentsAndTerminators();
					auto expr = parseAssignmentExpression(Goal.In | attributes);
					children.put(make!(PropertyDefinitionNode)(name,expr));
					break;
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Set:
							if (lexer.lookAheadForAny!(Type.Comma,Type.CloseCurlyBrace,Type.Colon,Type.OpenParenthesis))
								goto default;
							children.put(parseClassSetter(staticAttr,attributes.mask!(Goal.Yield)));
							break;
						case Keyword.Get:
							if (lexer.lookAheadForAny!(Type.Comma,Type.CloseCurlyBrace,Type.Colon,Type.OpenParenthesis))
								goto default;
							children.put(parseClassGetter(staticAttr,attributes.mask!(Goal.Yield)));
							break;
						default:
							auto name = parsePropertyName(attributes);
							skipCommentsAndLineTerminators();
							if (token.type == Type.Colon)
							{
								scanAndSkipCommentsAndTerminators();
								auto expr = parseAssignmentExpression(Goal.In | attributes);
								children.put(make!(PropertyDefinitionNode)(name,expr));
								break;
							}
							if (token.type == Type.OpenParenthesis)
							{
								children.put(parseClassMethod(staticAttr,attributes.mask!(Goal.Yield),name));
								break;
							}
							auto iden = cast(IdentifierNameNode)name;
							assert(iden !is null);
							if (isIdentifierReservedKeyword(iden.identifier))
								return error(format("Unexpected keyword %s",iden.identifier));
							if (token.type == Type.Assignment)
							{
								scanAndSkipCommentsAndTerminators();
								auto init = parseAssignmentExpression(Goal.In | attributes);
								children.put(make!(CoverInitializedName)(iden,init));
								break;
							}
							children.put(iden);
							break;
					}
					break;
				case Type.Multiply:
					children.put(parseClassGeneratorMethod(staticAttr,attributes.mask!(Goal.Yield)));
					break;
				case Type.CloseCurlyBrace:
					goto end;
				default:
					return error("Expected a PropertyDefinition");
			}
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error(format("Expected closing curly brace before %s",token.type));
		end: scanAndSkipCommentsAndTerminators();
		return make!(ObjectLiteralNode)(children.data);
	}
	Node parseTemplateTail(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node[] children = [make!(TemplateNode)(token.match)];
				scanAndSkipCommentsAndTerminators();

		while (1) // this loop can't run forever
		{
			auto expr = parseExpression(Goal.In | attributes);
			// TODO: do something with errornode here
			children ~= expr;
			if (expr.type == NodeType.ErrorNode)
			{
				children ~= error(token.match);
				scanAndSkipCommentsAndTerminators();
				return make!(TemplateLiteralNode)(children);
			}
			children ~= make!(TemplateNode)(token.match);
			if (token.type == Type.TemplateTail)
			{
				scanAndSkipCommentsAndTerminators();
				return make!(TemplateLiteralNode)(children);
			}
			scanAndSkipCommentsAndTerminators();
		}
	}
	Node parseParenthesisExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenParenthesis);
		scanAndSkipCommentsAndTerminators(attributes.filter!(Goal.NoRegex));
		if (token.type == Type.CloseParenthesis)
		{
			if (attributes & Goal.NoEmptyParen)
			{
				auto node = error("Expected AssignmentExpression");
				scanToken();
				return node;
			}
			auto node = make!(ParenthesisNode)();
			scanToken();
			return node;
		}
		if (token.type == Type.SpreadOperator)
		{
			scanToken(attributes);
			skipCommentsAndLineTerminators();
			auto spreadNode = make!(SpreadElementNode)(parseIdentifier(attributes));
			auto node = make!(ParenthesisNode)(spreadNode);
			skipCommentsAndLineTerminators();
			if (token.type != Type.CloseParenthesis)
				return error("Expected closing parenthesis");
			scanAndSkipCommentsAndTerminators();
			return node;
		}

		attributes = attributes.mask!(Goal.Yield,Goal.In).filter!(Goal.NoRegex);
		bool comma = false;
		Node[] children;
		while(1)
		{
			if (token.type == Type.EndOfFile)
			{
				if (comma)
					return error("Expected AssignmentExpression before eof");
				if (children.length == 0)
				{
					scanToken(attributes);
					return error("Expected Expression before eof");
				}
				if (children.length == 1)
					return make!(ParenthesisNode)(children[0]);
				auto exprNode = make!(ExpressionNode)(children);
				return make!(ParenthesisNode)(exprNode);
			}
			switch (token.type)
			{
				case Type.Comma:
					if (comma)
						return error("Expected AssignmentExpression instead got comma");
					comma = true;
					scanToken(attributes);
					break;
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken(attributes);
					break;
				case Type.SpreadOperator:
					scanAndSkipCommentsAndTerminators();
					auto spread = make!(SpreadElementNode)(parseIdentifier(attributes));
					if (token.type != Type.CloseParenthesis)
						return error("Expected closing parenthesis");
					scanAndSkipCommentsAndTerminators();
					auto exprNode = make!(ExpressionNode)(children);
					return make!(ParenthesisNode)([exprNode,spread]);
				case Type.CloseParenthesis:
					scanAndSkipCommentsAndTerminators(Goal.NoRegex);
					if (children.length == 1)
						return make!(ParenthesisNode)(children[0]);
					auto exprNode = make!(ExpressionNode)(children);
					return make!(ParenthesisNode)(exprNode);
				default:
					if (children.length != 0 && comma == false)
					{
						children ~= error("Expected comma in parenthesis expression");
					}
					comma = false;
					auto expr = parseAssignmentExpression(Goal.In | attributes);
					if (expr.type == NodeType.ErrorNode)
						scanToken();
					children ~= expr;
					break;
			}
		}
	}
	// TODO: this can actually be inlined in the parseLeftHandSideExpression
	Node parsePrimaryExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		attributes = attributes.mask!(Goal.Yield | Goal.NoRegex);
		switch (token.type)
		{
			/* Note: this Type.Identifier is inlined in the parseLeftHandSideExpression */
			case Type.Identifier:
				switch(token.keyword)
				{
					case Keyword.This: scanToken(); return make!(KeywordNode)(Keyword.This);
					case Keyword.Null: scanToken(); return make!(KeywordNode)(Keyword.Null);
					case Keyword.True: scanToken(); return make!(BooleanNode)(true);
					case Keyword.False: scanToken(); return make!(BooleanNode)(false);
					case Keyword.Function: return parseFunctionExpression();
					case Keyword.Class: return parseClassExpression(attributes);
					default:
						auto node = parseIdentifier(attributes);
						return node;
				}
			case Type.StringLiteral: auto node = make!(StringLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.BinaryLiteral: auto node = make!(BinaryLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.OctalLiteral: auto node = make!(OctalLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.DecimalLiteral: auto node = make!(DecimalLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.HexLiteral: auto node = make!(HexLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.OpenSquareBrackets: return parseArrayLiteral(attributes);
			case Type.OpenCurlyBrace: return parseObjectLiteral(attributes);
			case Type.TemplateHead: return parseTemplateTail(attributes.mask!(Goal.Yield));
			case Type.Template: //todo there is also a possible yield attribute here
				auto tmplNode = make!(TemplateNode)(token.match);
				auto node = make!(TemplateLiteralNode)(tmplNode); scanToken(attributes); return node;
			case Type.Regex: auto node = make!(RegexLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.OpenParenthesis: return parseParenthesisExpression(attributes);
			default:
				auto node = error(format("unexpected %s token (%s)",token.type,cast(const(char)[])token.match));
				// TODO: resync primary expression
				scanToken(attributes);
				return node;
		}
	}
	bool isEndOfExpression()
	{
		return (token.type == Type.Semicolon ||
			token.type == Type.CloseParenthesis ||
			token.type == Type.EndOfFile ||
			token.type == Type.CloseSquareBrackets ||
			token.type == Type.CloseCurlyBrace ||
			token.type == Type.Colon);/* ||
			(token.type == Type.Identifier && (token.match == "in" || token.match == "of")));*/
	}
	Node parseExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		attributes = attributes.mask!(Goal.Yield,Goal.In);
		bool comma = true;
		Node[] children;
		while(1)
		{
			if (isEndOfExpression || token.type == Type.TemplateTail)
			{
				if (comma)
					return error("Expected AssignmentExpression");
				if (children.length == 0)
				{
					scanToken();
					return error(format("Expected Expression, instead of %s",token));
				}
				if (children.length == 1)
					return children[0];
				return make!(ExpressionNode)(children);
			}
			switch (token.type)
			{
				case Type.Comma:
					if (comma)
						return error("Expected AssignmentExpression instead got comma");
					comma = true;
					scanToken(attributes);
					break;
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken(attributes);
					break;
				default:
					if (!comma) {
						if (children.length == 1)
							return children[0];
						return make!(ExpressionNode)(children);
					}
					comma = false;
					auto expr = parseAssignmentExpression(attributes);
					if (expr.type == NodeType.ErrorNode)
						scanToken();
					children ~= expr;
					break;
			}
		}
	}
	Node parseArrowFunctionBody(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		if (token.type == Type.OpenCurlyBrace)
		{
			scanToken(attributes);
			auto funcBody = parseFunctionBody();
			if (token.type != Type.CloseCurlyBrace)
				return error("Expected closing curly brace");
			scanAndSkipCommentsAndTerminators();
			return funcBody;
		}
		return parseAssignmentExpression(attributes);
	}
	Node parseAssignmentExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node cond;
		if (token.match == "yield" && attributes.has!(Goal.Yield))
		{
			return parseYieldExpression(attributes.filter!(Goal.Yield));
		} else
		{
			cond = parseConditionalExpression(attributes);
			if (cond.type == NodeType.ConditionalExpressionNode)
				return cond;
			skipCommentsAndLineTerminators();
			if (cond.type == NodeType.IdentifierReferenceNode && token.type == Type.Arrow)
			{
				scanAndSkipCommentsAndTerminators();
				return make!(ArrowFunctionNode)(cond,parseArrowFunctionBody(attributes.mask!(Goal.In)));
			}
			if (cond.type == NodeType.ParenthesisNode && token.type == Type.Arrow)
			{
				scanAndSkipCommentsAndTerminators();
				return make!(ArrowFunctionNode)(cond,parseArrowFunctionBody(attributes.mask!(Goal.In)));
			}
		}
		if (cond.type == NodeType.ErrorNode)
			return cond;
		if (cond.type == NodeType.UnaryExpressionNode || cond.type == NodeType.BinaryExpressionNode)
			return cond;

		if (lexer.empty)
			return cond;

		ArrayBuilder!Node children;
		while (!lexer.empty) // TODO: this one might loop forever with invalid input, not sure, need to check
		{
			Node child;
			switch (token.type)
			{
				case Type.LeftShiftAssignment: 			child = make!(AssignmentOperatorNode)(Assignment.LeftShiftAssignment); break;
				case Type.TripleRightShiftAssignment: 	child = make!(AssignmentOperatorNode)(Assignment.TripleRightShiftAssignment); break;
				case Type.RightShiftAssignment: 		child = make!(AssignmentOperatorNode)(Assignment.RightShiftAssignment); break;
				case Type.Assignment: 					child = make!(AssignmentOperatorNode)(Assignment.Assignment); break;
				case Type.AdditiveAssignment: 			child = make!(AssignmentOperatorNode)(Assignment.AdditiveAssignment); break;
				case Type.DecrementalAssignment: 		child = make!(AssignmentOperatorNode)(Assignment.DecrementalAssignment); break;
				case Type.MultiplicativeAssignment: 	child = make!(AssignmentOperatorNode)(Assignment.MultiplicativeAssignment); break;
				case Type.DivisionAssignment: 			child = make!(AssignmentOperatorNode)(Assignment.DivisionAssignment); break;
				case Type.ModAssignment: 				child = make!(AssignmentOperatorNode)(Assignment.ModAssignment); break;
				case Type.BitwiseAndAssignment: 		child = make!(AssignmentOperatorNode)(Assignment.BitwiseAndAssignment); break;
				case Type.BitwiseOrAssignment: 			child = make!(AssignmentOperatorNode)(Assignment.BitwiseOrAssignment); break;
				case Type.BitwiseXorAssignment: 		child = make!(AssignmentOperatorNode)(Assignment.BitwiseXorAssignment); break;
				default:
					if (children.length == 0)
						return cond;
					return make!(AssignmentExpressionNode)(children.data);
			}
			if (children.length == 0)
			{
				children.put(cond);
			}
			children.put(child);
			scanToken(attributes);
			if (token.match == "yield" && attributes.has!(Goal.Yield))
			{
				children.put(parseYieldExpression(attributes.filter!(Goal.Yield)));
				break;
			}
			cond = parseConditionalExpression(attributes);
			if (cond.type == NodeType.ConditionalExpressionNode)
			{
				children.put(cond);
				return make!(AssignmentExpressionNode)(children.data);
			}
			if (cond.type == NodeType.IdentifierReferenceNode && token.type == Type.Arrow)
			{
				scanAndSkipCommentsAndTerminators(attributes);
				children.put(make!(ArrowFunctionNode)(cond,parseArrowFunctionBody(attributes.mask!(Goal.In))));
				return make!(AssignmentExpressionNode)(children.data);
			}
			// TODO this idea in the if below is correct, but there might be Comments between the parenthesis and the arrow (which are allowed, as long as the arrow is on the same line as the closing parenthesis)
			if (cond.type == NodeType.ParenthesisNode && token.type == Type.Arrow)
			{
				scanAndSkipCommentsAndTerminators(attributes);
				children.put(make!(ArrowFunctionNode)(cond,parseArrowFunctionBody(attributes.mask!(Goal.In))));
				return make!(AssignmentExpressionNode)(children.data);
			}
			children.put(cond);
			if (cond.type == NodeType.UnaryExpressionNode || cond.type == NodeType.BinaryExpressionNode)
				return make!(AssignmentExpressionNode)(children.data);
		}
		return make!(AssignmentExpressionNode)(children.data);
	}
	Node parseYieldExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.match == "yield");
		bool _delegate = false;
		scanToken();
		while(1)
		{
			switch(token.type) {
				case Type.Multiply:
					if (_delegate)
					{
						scanToken();
						return error("Expected AssignmentExpression after first *");
					}
					_delegate = true;
					break;
				case Type.SingleLineComment:
				case Type.LineTerminator:
					if (_delegate)
						break;
					return make!(YieldExpressionNode)(null,_delegate);
				case Type.MultiLineComment:
					if (!_delegate)
					{
						// TODO: if multi line comment has line terminator, we are done
					}
					break;
				case Type.EndOfFile:
					if (_delegate)
						return error("Expected AssignmentExpression after first *");
					return make!(YieldExpressionNode)(null,_delegate);
				default:
					auto assign = parseAssignmentExpression();
					return make!(YieldExpressionNode)(assign,_delegate);
			}
			if (_delegate)
				scanAndSkipCommentsAndTerminators();
			else
				scanToken();
		}
	}
	Node parseConditionalExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		//auto rhs = parseRightHandSideExpression(attributes);
		auto rhs = parseRightHandSideExpressionBottomUp(attributes);
		if (token.type != Type.QuestionMark)
			return rhs;
		scanToken(attributes);
		if (lexer.empty)
			return error(format("Expected AssignmentExpression as part of an ConditionalExpression before eof"));
		auto yeah = parseAssignmentExpression(Goal.In | attributes);
		if (token.type != Type.Colon)
			return error([rhs,yeah],format("Expected colon as part of ConditionalExpression, instead got %s token %s",token.type,cast(const(ubyte)[])token.match));
		scanToken(attributes);
		if (lexer.empty)
			return error(format("Expected AssignmentExpression as part of an ConditionalExpression before eof"));
		auto nay = parseAssignmentExpression(attributes);
		return make!(ConditionalExpressionNode)(rhs,yeah,nay);
	}
	Node parseRightHandSideExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		ArrayBuilder!Node children;
		while (1) // this loop won't run forever
		{
			Node unary = parseUnaryExpression(attributes.mask!(Goal.Yield | Goal.NoRegex));
			Node child;
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Instanceof:
							child = make!(ExpressionOperatorNode)(ExpressionOperator.InstanceOf);
							break;
						case Keyword.In:
							if (!attributes.has!(Goal.In))
							{
								if (children.length == 0)
									return unary;
								children.put(unary);
								return make!(BinaryExpressionNode)(children.data);
							}
							child = make!(ExpressionOperatorNode)(ExpressionOperator.In);
							break;
						default:
							if (children.length == 0)
								return unary;
							children.put(unary);
							return make!(BinaryExpressionNode)(children.data);
					}
					children.put(unary);
					children.put(child);
					scanToken(attributes);
					continue;
				case Type.LogicalAnd: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.LogicalAnd); break;
				case Type.LogicalOr: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.LogicalOr); break;
				case Type.BitwiseAnd: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseAnd); break;
				case Type.BitwiseOr: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseOr); break;
				case Type.BitwiseXor: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseXor); break;
				case Type.StrictEqual: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.StrictEqual); break;
				case Type.Equal: 			child = make!(ExpressionOperatorNode)(ExpressionOperator.Equal); break;
				case Type.StrictNotEqual: 	child = make!(ExpressionOperatorNode)(ExpressionOperator.StrictNotEqual); break;
				case Type.NotEqual: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.NotEqual); break;
				case Type.LessOrEqual: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.LessOrEqual); break;
				case Type.LessThan: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.LessThan); break;
				case Type.GreaterOrEqual: 	child = make!(ExpressionOperatorNode)(ExpressionOperator.GreaterOrEqual); break;
				case Type.GreaterThan: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.GreaterThan); break;
				case Type.LeftShift: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.LeftShift); break;
				case Type.TripleRightSift: 	child = make!(ExpressionOperatorNode)(ExpressionOperator.TripleRightSift); break;
				case Type.RightShift: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.RightShift); break;
				case Type.Add:		 		child = make!(ExpressionOperatorNode)(ExpressionOperator.Add); break;
				case Type.Minus: 			child = make!(ExpressionOperatorNode)(ExpressionOperator.Minus); break;
				case Type.Multiply: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.Multiply); break;
				case Type.Division: 		child = make!(ExpressionOperatorNode)(ExpressionOperator.Division); break;
				case Type.Mod: 				child = make!(ExpressionOperatorNode)(ExpressionOperator.Mod); break;
				default:
					if (children.length == 0)
						return unary;
					children.put(unary);
					return make!(BinaryExpressionNode)(children.data);
			}
			children.put(unary);
			children.put(child);
			attributes &= ~Goal.NoRegex;
			scanToken(attributes);
		}
	}
	Node parseUnaryExpression(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node[] prefixExprs; // TODO: should we make this into a ArrayBuilder!(Node,2) ?? 
		while(token.type != Type.EndOfFile)
		{
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Delete: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Delete); break;
						case Keyword.Void: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Void); break;
						case Keyword.Typeof: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Typeof); break;
						default:
							goto doLHS;
					}
					break;
				case Type.Increment: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Increment); break;
				case Type.Decrement: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Decrement); break;
				case Type.Add: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Positive); break;
				case Type.Minus: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Negative); break;
				case Type.Tilde: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Tilde); break;
				case Type.Negation: prefixExprs ~= make!(PrefixExpressionNode)(Prefix.Negation); break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					break;
				default:
					goto doLHS;
			}
			scanToken(attributes);			
		}
		return error("Found end of file before parsing UnaryExpression");
		doLHS:
		attributes |= Goal.NoRegex;
		auto lineNr = lexer.line;
		auto lhsexpr = parseLeftHandSideExpression(attributes);
		bool postFixOnSameLine = lineNr == lexer.line;
		if (postFixOnSameLine && token.type == Type.Increment)
		{
			auto node = make!(UnaryExpressionNode)(prefixExprs,lhsexpr);
			node.postfix = Postfix.Increment;
			scanToken(attributes);
			return node;
		} else if (postFixOnSameLine && token.type == Type.Decrement)
		{
			auto node = make!(UnaryExpressionNode)(prefixExprs,lhsexpr);
			node.postfix = Postfix.Decrement;
			scanToken(attributes);
			return node;
		} else if (prefixExprs.length > 0)
			return make!(UnaryExpressionNode)(prefixExprs,lhsexpr);
		return lhsexpr;
	}
	Node parseAccessor(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Dot);
		scanToken(attributes);

		while (1) // won't run forever
		{
			switch (token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					scanToken(attributes);
					break;
				case Type.Identifier:
					auto node = make!(AccessorNode)(token.match);
					attributes |= Goal.NoRegex;
					scanAndSkipCommentsAndTerminators(attributes);
					return node;
				default:
					auto node = error(format("Expected identifer got %s",token.type));
					scanToken();
					return node;
			}
		}
	}
	Node parseRightHandSideExpressionBottomUp(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		ArrayBuilder!(Node,4) prefixs;
		Node primary;
		Node lhs;
		Node unary;
		Node binChild;
		ArrayBuilder!(Node,4) lhsChildren;
		ArrayBuilder!(Node,4) binaryChildren;
		Postfix postfix = Postfix.None;
		int unaryAttr = attributes.mask!(Goal.Yield | Goal.NoRegex);
		int lhsAttr = unaryAttr | Goal.NoRegex;
		int primAttr = lhsAttr.mask!(Goal.Yield | Goal.NoRegex);
		int news, args;
		bool hadNewLine = false;
		lhsChildren.put(null); // preserve space for primary later on
	unary:
		while(1)
		{
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Delete: prefixs.put(make!(PrefixExpressionNode)(Prefix.Delete)); break;
						case Keyword.Void: prefixs.put(make!(PrefixExpressionNode)(Prefix.Void)); break;
						case Keyword.Typeof: prefixs.put(make!(PrefixExpressionNode)(Prefix.Typeof)); break;
						case Keyword.This: scanToken(); primary = make!(KeywordNode)(Keyword.This); goto member;
						case Keyword.Null: scanToken(); primary = make!(KeywordNode)(Keyword.Null); goto member;
						case Keyword.True: scanToken(); primary = make!(BooleanNode)(true); goto member;
						case Keyword.False: scanToken(); primary = make!(BooleanNode)(false); goto member;
						case Keyword.Function: primary = parseFunctionExpression(); goto member;
						case Keyword.Class: primary = parseClassExpression(primAttr); goto member;
						case Keyword.Super:
					unarySuper:
							if (lexer.lookAheadForAny!(Type.OpenParenthesis))
							{
								primary = new IdentifierReferenceNode(cast(const(ubyte)[])"super");
								args++;
								scanAndSkipCommentsAndTerminators();
								lhsChildren.put(parseArguments(lhsAttr.mask!(~Goal.NoRegex)));
								goto member;
							}
							primary = parseSuperProperty(lhsAttr);
							goto member;
						case Keyword.New:
							news++;
							scanToken(lhsAttr);
							while (token.type == Type.Identifier && token.keyword == Keyword.New)
							{
								news++;
								scanToken(lhsAttr);
							}
							if (token.type == Type.Dot)
							{
								scanToken(lhsAttr);
								if (token.type == Type.Identifier && token.match == "target")
								{
									news--;
									primary = make!(NewTargetNode)();
									scanToken(lhsAttr);
								} else
								{
									primary = error("The only valid meta property for new is new.target");
									scanToken(lhsAttr);
								}
								goto member;
							}
							if (token.type == Type.Identifier && token.keyword == Keyword.Super)
								goto unarySuper;
							primary = parseLeftHandSideExpression(lhsAttr,news);
							news = 0;
							goto member;
						default:
							auto lineNr = lexer.line;
							primary = parseIdentifier(primAttr);
							hadNewLine = lineNr != lexer.line;
							goto member;
					}
					break;
				case Type.Increment: prefixs.put(make!(PrefixExpressionNode)(Prefix.Increment)); break;
				case Type.Decrement: prefixs.put(make!(PrefixExpressionNode)(Prefix.Decrement)); break;
				case Type.Add: prefixs.put(make!(PrefixExpressionNode)(Prefix.Positive)); break;
				case Type.Minus: prefixs.put(make!(PrefixExpressionNode)(Prefix.Negative)); break;
				case Type.Tilde: prefixs.put(make!(PrefixExpressionNode)(Prefix.Tilde)); break;
				case Type.Negation: prefixs.put(make!(PrefixExpressionNode)(Prefix.Negation)); break;
				case Type.StringLiteral: primary = make!(StringLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.BinaryLiteral: primary = make!(BinaryLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.OctalLiteral: primary = make!(OctalLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.DecimalLiteral: primary = make!(DecimalLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.HexLiteral: primary = make!(HexLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.OpenSquareBrackets: primary = parseArrayLiteral(primAttr); goto member;
				case Type.OpenCurlyBrace: primary = parseObjectLiteral(primAttr); goto member;
				case Type.TemplateHead: primary = parseTemplateTail(primAttr.mask!(Goal.Yield)); goto member;
				case Type.Template: //todo there is also a possible yield attribute here
					auto tmplNode = make!(TemplateNode)(token.match);
					primary = make!(TemplateLiteralNode)(tmplNode); scanToken(primAttr); goto member;
				case Type.Regex: primary = make!(RegexLiteralNode)(token.match); scanToken(primAttr); goto member;
				case Type.OpenParenthesis:
					if (binaryChildren.length > 0)
						primary = parseParenthesisExpression(primAttr | Goal.NoEmptyParen);
					else
						primary = parseParenthesisExpression(primAttr);
					goto member;
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					// TODO: multiline comment might not have had newline
					hadNewLine = true;
					scanToken(unaryAttr);
					continue;
				default:
					unary = primary = error(format("unexpected %s token (%s)",token.type,cast(const(char)[])token.match));
					scanToken(primAttr);
					goto end;
				case Type.EndOfFile:
					unary = primary = error(format("expected PrimaryExpression before eof"));
					goto end;
			}
			hadNewLine = false;
			scanToken(unaryAttr);
		}
	member:
		while(1)
		{
			switch (token.type)
			{
				case Type.Dot:
					args++;
					auto child = parseAccessor();
					lhsChildren.put(child);
					/*if (child.type == NodeType.AccessorNode && child.as!(AccessorNode).identifier == "target")
					{
						// assert lhsChildren is empty // else error
						goto binary;
					}*/
					break;
				case Type.Template:
					args++;
					auto tmplNode = make!(TemplateNode)(token.match);
					auto child = make!(TemplateLiteralNode)(tmplNode);
					lhsChildren.put(child);
					scanToken(lhsAttr);
					break;
				case Type.TemplateHead:
					args++;
					auto child = parseTemplateTail(lhsAttr);
					lhsChildren.put(child);
					break;
				case Type.OpenParenthesis:
					args++;
					auto child = parseArguments(lhsAttr.mask!(~Goal.NoRegex));
					lhsChildren.put(child);
					break;
				case Type.OpenSquareBrackets:
					args++;
					auto child = parseArrayIndexing(Goal.In | unaryAttr);
					lhsChildren.put(child);
					break;
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					// TODO: multiline comment might not have newline
					hadNewLine = true;
					scanToken(unaryAttr);
					continue;
				case Type.Increment:
					if (hadNewLine)
						goto binary;
					postfix = Postfix.Increment;
					scanToken(lhsAttr);
					goto binary;
				case Type.Decrement:
					if (hadNewLine)
						goto binary;
					postfix = Postfix.Decrement;
					scanToken(lhsAttr);
					goto binary;
				case Type.EndOfFile:
					goto binary;
				default:
					goto binary;
			}
			hadNewLine = false;
		}
	binary:
		if (news == 0 && args == 0)
		{
			lhs = primary;
		} else 
		{
			auto children = lhsChildren.data;
			children[0] = primary;
			if (news > args)
			{
				lhs = make!(NewExpressionNode)(news,children);
				lhsChildren.clear();
				lhsChildren.put(null);
			} else
			{
				lhs = make!(CallExpressionNode)(news,children);
				lhsChildren.clear();
				lhsChildren.put(null);
			}
		}
		news = 0;
		args = 0;
		if (prefixs.length > 0 || postfix != Postfix.None)
		{
			unary = make!(UnaryExpressionNode)(prefixs.data,lhs,postfix);
			postfix = Postfix.None;
			prefixs.clear();
		} else
		{
			unary = lhs;
		}
	onnewline:
		switch (token.type)
		{
			case Type.Identifier:
				switch (token.keyword)
				{
					case Keyword.Instanceof:
						binChild = make!(ExpressionOperatorNode)(ExpressionOperator.InstanceOf);
						break;
					case Keyword.In:
						if (!attributes.has!(Goal.In))
							goto end;
						binChild = make!(ExpressionOperatorNode)(ExpressionOperator.In);
						break;
					default:
						goto end;
				}
				break;
			case Type.LogicalAnd: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.LogicalAnd); break;
			case Type.LogicalOr: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.LogicalOr); break;
			case Type.BitwiseAnd: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseAnd); break;
			case Type.BitwiseOr: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseOr); break;
			case Type.BitwiseXor: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.BitwiseXor); break;
			case Type.StrictEqual: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.StrictEqual); break;
			case Type.Equal: 			binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Equal); break;
			case Type.StrictNotEqual: 	binChild = make!(ExpressionOperatorNode)(ExpressionOperator.StrictNotEqual); break;
			case Type.NotEqual: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.NotEqual); break;
			case Type.LessOrEqual: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.LessOrEqual); break;
			case Type.LessThan: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.LessThan); break;
			case Type.GreaterOrEqual: 	binChild = make!(ExpressionOperatorNode)(ExpressionOperator.GreaterOrEqual); break;
			case Type.GreaterThan: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.GreaterThan); break;
			case Type.LeftShift: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.LeftShift); break;
			case Type.TripleRightSift: 	binChild = make!(ExpressionOperatorNode)(ExpressionOperator.TripleRightSift); break;
			case Type.RightShift: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.RightShift); break;
			case Type.Add:		 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Add); break;
			case Type.Minus: 			binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Minus); break;
			case Type.Multiply: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Multiply); break;
			case Type.Division: 		binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Division); break;
			case Type.Mod: 				binChild = make!(ExpressionOperatorNode)(ExpressionOperator.Mod); break;
			case Type.LineTerminator:
			case Type.SingleLineComment:
			case Type.MultiLineComment:
				scanToken(attributes);
				goto onnewline;
			default:
				goto end;
		}
		binaryChildren.put(unary);
		binaryChildren.put(binChild);
		unaryAttr &= ~Goal.NoRegex;
		scanToken(unaryAttr);
		goto unary;
	end:
		if (unary is null)
			return primary;
		if (binaryChildren.length > 0)
		{
			binaryChildren.put(unary);
			return make!(BinaryExpressionNode)(binaryChildren.data);
		}
		return unary;
	}
	Node parseLeftHandSideExpression(int attributes = 0, size_t news = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		size_t args = 0;
		Node content;
		ArrayBuilder!Node calls;
		while (1)  // TODO: this one might loop forever with invalid input, not sure, need to check
		{
			switch (token.type)
			{
				case Type.Identifier:
					if (content !is null)
						goto end;
					switch (token.keyword)
					{
						case Keyword.Super:
							if (lexer.lookAheadForAny!(Type.OpenParenthesis))
							{
								content = new IdentifierReferenceNode(cast(const(ubyte)[])"super");
								args++;
								if (calls.length == 0)
									calls.put(content);
								scanAndSkipCommentsAndTerminators();
								calls.put(parseArguments(attributes.mask!(~Goal.NoRegex)));
								break;								
							}
							content = parseSuperProperty(attributes);
							break;
						case Keyword.New:
							news++;
							scanToken(attributes);
							break;
				 		case Keyword.This: scanToken(); content = make!(KeywordNode)(Keyword.This); break;
						case Keyword.Null: scanToken(); content = make!(KeywordNode)(Keyword.Null); break;
						case Keyword.True: scanToken(); content = make!(BooleanNode)(true); break;
						case Keyword.False: scanToken(); content = make!(BooleanNode)(false); break;
						case Keyword.Function: content = parseFunctionExpression(); break;
						case Keyword.Class: content = parseClassExpression(attributes.mask!(Goal.Yield | Goal.NoRegex)); break;
						default: content = parseIdentifier(attributes.mask!(Goal.Yield | Goal.NoRegex)); break;
					}
					break;
				case Type.Dot:
					if (content !is null)
					{
						args++;
						if (calls.length == 0)
							calls.put(content);
						calls.put(parseAccessor());
						break;
					}
					if (news == 0)
						return error("Invalid dot in LeftHandSideExpression");
					scanToken(attributes);
					if (token.type != Type.Identifier || token.match != "target")
						return error("The only valid meta property for new is new.target");
					content = make!(NewTargetNode)();
					scanToken(attributes);
					break;
				case Type.Template:
					if (content is null)
						goto default;
					args++;
					if (calls.length == 0)
						calls.put(content);
					auto tmplNode = make!(TemplateNode)(token.match);
					calls.put(make!(TemplateLiteralNode)(tmplNode));
					scanToken(attributes);
					break;
				case Type.TemplateHead:
					if (content is null)
						goto default;
					args++;
					if (calls.length == 0)
						calls.put(content);
					calls.put(parseTemplateTail(attributes));
					break;
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					scanToken(attributes);
					break;
				case Type.OpenParenthesis:
					if (content is null)
						goto default;
					args++;
					if (calls.length == 0)
						calls.put(content);
					calls.put(parseArguments(attributes.mask!(~Goal.NoRegex)));
					break;
				case Type.OpenSquareBrackets:
					if (content is null)
						goto default;
					args++;
					if (calls.length == 0)
						calls.put(content);
					calls.put(parseArrayIndexing(Goal.In | attributes));
					break;
				case Type.EndOfFile:
					if (content is null)
						return error("Expected LeftHandSideExpression before end of file");
					goto end;
				default:
					if (content !is null)
						goto end;
					content = parsePrimaryExpression(attributes);
					break;
			}
		}
		end: if (news == 0 && args == 0)
			return content;
		if (calls.length == 0)
			calls.put(content);
		else
			calls.data[0] = content;
		if (news > 0 && news > args)
		{
			return make!(NewExpressionNode)(news,calls.data);
		}
		assert(args > 0);
		return make!(CallExpressionNode)(news,calls.data);
	}
	bool isIdentifierReservedKeyword(const(ubyte)[] identifier)
	{
		return identifier.isReservedKeyword();
	}
	Node parseIdentifier(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier);

		IdentifierReferenceNode n = make!(IdentifierReferenceNode)(token.match);
		auto keyword = token.keyword;
		if (keyword == Keyword.Yield)
		{
			if (!attributes.has!(Goal.Yield))
				return error("keyword yield cannot be used in this context");

		} else if (keyword.isReservedKeyword)
			return error(format("Invalid IdentifierReference %s",cast(const(char)[])token.match));

		scanAndSkipCommentsAndTerminators(attributes);
		return n;
	}
	Node parseIdentifierName()
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier);
		auto n = make!(IdentifierNameNode)(token.match);
		scanAndSkipCommentsAndTerminators();
		return n;
	}
	Node parseSuperProperty(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "super");
		scanToken(attributes);

		if (token.type == Type.Dot)
		{
			scanToken(attributes);
			if (token.type != Type.Identifier)
				return error("Expected Identifier after .");
			return make!(SuperPropertyNode)(parseIdentifierName());
		} else if (token.type == Type.OpenSquareBrackets)
		{
			return make!(SuperPropertyNode)(parseArrayIndexing(Goal.In | attributes));
		}
		return error("Expected . or array index after super keyword");
	}
	Node parseArguments(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenParenthesis);
		scanToken(attributes);

		ArrayBuilder!(Node,4) args;
		if (token.type == Type.Comma)
			return error("Expected argument before comma");

		if (token.type == Type.Error)
			return error(token.match);
		while (1) // this while loop is ok, it can't run forever
		{
			if (token.type == Type.MultiLineComment ||
				token.type == Type.SingleLineComment ||
				token.type == Type.LineTerminator)
			{
				scanToken(attributes);
				continue;
			}
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				args.put(make!(SpreadElementNode)(parseAssignmentExpression(Goal.In | attributes)));
				if (token.type != Type.CloseParenthesis)
					return error("Expected closing parenthesis");
				scanAndSkipCommentsAndTerminators();
				return make!(ArgumentsNode)(args.data);
			}
			if (token.type == Type.CloseParenthesis)
			{
				goto end;
			}
			if (token.type == Type.Comma)
			{
				scanAndSkipCommentsAndTerminators(attributes);
				if (token.type == Type.CloseParenthesis)
					goto end;
				args.put(parseAssignmentExpression(Goal.In | attributes));
				// TODO what if parse fails ?
			} else {
				if (args.length != 0)
				{
					return error("Expected Comma or CloseParenthesis");
				}
				auto assign = parseAssignmentExpression(Goal.In | attributes);
				args.put(assign);
				if (assign.type == NodeType.ErrorNode)
					goto end;
			}
		}
		end: auto node = make!(ArgumentsNode)(args.data);
		attributes |= Goal.NoRegex;
		scanToken(attributes);
		return node;
	}
	Node parseArrayIndexing(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenSquareBrackets);
		scanAndSkipCommentsAndTerminators();

		auto expr = parseExpression(attributes);
		if (token.type != Type.CloseSquareBrackets)
			return error("Expected closing square bracket");

		//scanAndSkipCommentsAndTerminators(Goal.NoRegex);
		scanToken(Goal.NoRegex);
		return make!(ArrayIndexNode)(expr);
	}
	Node parseStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node node;
		switch (token.type)
		{
			case Type.OpenCurlyBrace:
				return parseBlockStatement(attributes);
			case Type.Identifier:
				switch (token.keyword)
				{
					case Keyword.Var: node = parseVariableStatement(attributes.mask!(Goal.Yield)); break;
					case Keyword.If: node = parseIfStatement(attributes); break;
					case Keyword.Switch: node = parseSwitchStatement(attributes); break;
					case Keyword.Do: node = parseDoWhileStatement(attributes); break;
					case Keyword.While: node = parseWhileStatement(attributes); break;
					case Keyword.For: node = parseForStatement(attributes); break;
					case Keyword.Continue: 
						scanToken(attributes);
						const(ubyte)[] label;
						if (token.type == Type.Identifier)
						{
							label = token.match;
							scanToken(attributes);
						}
						node = make!(ContinueStatementNode)(label);
						break;
					case Keyword.Break: 
						scanToken(attributes);
						const(ubyte)[] label;
						if (token.type == Type.Identifier)
						{
							label = token.match;
							scanToken(attributes);
						}
						node = make!(BreakStatementNode)(label);
						break;
					case Keyword.With: node = parseWithStatement(attributes); break;
					case Keyword.Throw: node = parseThrowStatement(attributes); break;
					case Keyword.Try: node = parseTryStatement(attributes); break;
					case Keyword.Debugger: node = parseDebuggerStatement(); break;
					case Keyword.Return:
						if (!attributes.has!(Goal.Return))
						{
							scanAndSkipCommentsAndTerminators();
							return error("return keyword not allowed in this context");
						}
						node = parseReturnStatement(attributes.mask!(Goal.Yield));
						break;
					default: node = parseExpression(Goal.In | attributes.mask!(Goal.Yield,Goal.Return)); break;
				}
				break;
			case Type.Semicolon:
				scanAndSkipCommentsAndTerminators();
				return make!(EmptyStatementNode)();
			default: node = parseExpression(Goal.In | attributes.mask!(Goal.Yield,Goal.Return)); break;
		}
		if (token.type == Type.Semicolon || node.type == NodeType.ErrorNode)
		{
			scanToken(attributes);
			return node;
		}
		// this while loop can't run forever
		while (!isEndOfExpression || (node.type == NodeType.IdentifierReferenceNode && token.type == Type.Colon))
		{
			switch (token.type)
			{
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken(attributes);
					break;
				case Type.Colon:
					if (node.type == NodeType.IdentifierReferenceNode)
					{
						auto idNode = cast(IdentifierReferenceNode)node;
						assert(idNode !is null);
						scanAndSkipCommentsAndTerminators(attributes);
						Node child;
						if (token.type == Type.Identifier && token.match == "function")
							child = parseFunctionDeclaration(attributes);
						else
							child = parseStatementListItem(attributes);
						return make!(LabelledStatementNode)(idNode.identifier, child);
					}
					return error("Unexpected colon in Statement");
				case Type.Semicolon:
					scanToken(attributes);
					return node;
				default: return node;
			}
		}
		return node;
	}
	Node parseReturnStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.match == "return");
		scanToken();
		if (token.type == Type.LineTerminator || isEndOfExpression || token.type == Type.SingleLineComment || lexer.tokenLines != 0)
			return make!(ReturnStatementNode)();
		auto expr = parseExpression(Goal.In | attributes);
		return make!(ReturnStatementNode)(expr);
	}
	Node parseBlockStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		Node[] children = parseStatementList(attributes);
		if (token.type != Type.CloseCurlyBrace)
			return error(children, "Expected closing curly brace");
		scanAndSkipCommentsAndTerminators();
		return make!(BlockStatementNode)(children);
	}
	Node[] parseStatementList(int attributes = 0)
	{
		auto children = appender!(Node[]);
		bool needsSeparator = false;
		while(!isEndOfExpression || token.type == Type.Semicolon)
		{
			if (token.type == Type.Identifier && (token.match == "case" || token.match == "default"))
				return children.data;
			switch(token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.Semicolon:
					scanAndSkipCommentsAndTerminators();
					needsSeparator = false;
					continue;
				case Type.InvalidUTF8:
					children.put(error("InvalidUTF8"));
					return children.data;
				default:
					if (needsSeparator && !lexer.recentSeparator)
						children.put(error("Expected newline or semicolon"));
					auto item = parseStatementListItem(attributes);
					children.put(item);
					needsSeparator = item.requiresSeparator();
					break;
			}
		}
		return children.data;
	}
	Node parseStatementListItem(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node node;
		if (token.type == Type.Identifier)
		{
			switch(token.keyword)
			{
				case Keyword.Class:
					node = parseClassDeclaration(attributes.mask!(Goal.Yield));
					break;
				case Keyword.Function:
					node = parseFunctionDeclaration(attributes.mask!(Goal.Yield));
					break;
				case Keyword.Let:
				case Keyword.Const:
					if (lexer.lookAheadForAny!(Type.Identifier,Type.OpenSquareBrackets,Type.OpenCurlyBrace))
						node = parseLexicalDeclaration(Goal.In | attributes.mask!(Goal.Yield));
					break;
				default:
					break;
			}
		}
		if (node is null)
			node = parseStatement(attributes);
		if (token.type == Type.Semicolon)
			scanAndSkipCommentsAndTerminators();
		return node;
	}
	version (unittest)
	{
		void skipCommentsAndLineTerminators(int attributes = 0, in size_t line = __LINE__)
		{
			while(token.type == Type.MultiLineComment || token.type == Type.SingleLineComment || token.type == Type.LineTerminator)
				scanToken(attributes, line);
		}
		void scanAndSkipCommentsAndTerminators(int attributes = 0, in size_t line = __LINE__)
		{
			scanToken(attributes, line);
			skipCommentsAndLineTerminators(attributes, line);
		}
	} else
	{
		void skipCommentsAndLineTerminators(int attributes = 0)
		{
			while(token.type == Type.MultiLineComment || token.type == Type.SingleLineComment || token.type == Type.LineTerminator)
				scanToken(attributes);
		}
		void scanAndSkipCommentsAndTerminators(int attributes = 0)
		{
			scanToken(attributes);
			skipCommentsAndLineTerminators(attributes);
		}
	}
	Node parseVariableStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "var");
		scanAndSkipCommentsAndTerminators();
		return parseVariableDeclarationList(Goal.In | attributes);
	}
	Node parseVariableDeclarationList(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		ArrayBuilder!(Node,4) children;
		while (!isEndOfExpression) // this while loop can't run forever
		{
			Node init = null;
			Node lhs = null;
			switch (token.type)
			{
				case Type.OpenSquareBrackets: lhs = parseArrayBindingPattern(attributes.mask!(Goal.Yield)); break;
				case Type.OpenCurlyBrace: lhs = parseObjectBindingPattern(attributes.mask!(Goal.Yield)); break;
				case Type.Identifier: lhs = parseIdentifier(attributes.mask!(Goal.Yield)); break;
				default:
					return error(format("Expected ArrayBindingPattern, ObjectBindingPattern or Identifier, instead got %s",token.type));
			}
			skipCommentsAndLineTerminators();
			if (token.type == Type.Assignment)
			{
				scanAndSkipCommentsAndTerminators();
				init = parseAssignmentExpression(attributes.mask!(Goal.In,Goal.Yield));
			}
			children.put(make!(VariableDeclarationNode)(lhs,init));
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (children.length == 0)
			return error("Expected variable declarations");
		return make!(VariableStatementNode)(children.data);
	}
	Node parseArrayBindingPattern(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenSquareBrackets);
		scanAndSkipCommentsAndTerminators();
		Node[] children;
		if (token.type == Type.Comma)
		{
			children ~= parseElision();
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children ~= make!(RestElementNode)(parseIdentifier(attributes));
				if (token.type != Type.CloseSquareBrackets)
					return error("Expected closing square brace");
				scanAndSkipCommentsAndTerminators();
				return make!(ArrayBindingPatternNode)(children);
			}
		}
		while (!isEndOfExpression) // this while loop won't run forever
		{
			children ~= parseBindingElement();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
			if (token.type == Type.Comma)
			{
				children ~= parseElision();
			}
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children ~= make!(RestElementNode)(parseIdentifier(attributes));
				break;
			}
		}
		if (token.type != Type.CloseSquareBrackets)
			return error("Expected closing square brace");
		scanAndSkipCommentsAndTerminators();
		return make!(ArrayBindingPatternNode)(children);
	}
	Node parseObjectBindingPattern(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
		{
			scanAndSkipCommentsAndTerminators();
			Node[] nodes;
			return make!(ObjectBindingPatternNode)(nodes);
		}
		Node[] children;
		while(!isEndOfExpression)
		{
			auto name = parsePropertyName(attributes);
			if (token.type == Type.Colon)
			{
				scanAndSkipCommentsAndTerminators();
				auto elem = parseBindingElement(attributes);
				children ~= make!(BindingPropertyNode)(name,elem);
			} else
			{
				auto iden = cast(IdentifierNameNode)name;
				if (iden is null)
					return error(format("Expected identifier, got %s",name));
				if (isIdentifierReservedKeyword(iden.identifier))
					return error(format("Invalid IdentifierReference %s",cast(const(char)[])iden.identifier));
				if (token.type != Type.Assignment)
					children ~= iden;
				else
				{
					scanAndSkipCommentsAndTerminators();
					auto init = parseAssignmentExpression(Goal.In | attributes);
					children ~= make!(SingleNameBindingNode)(iden,init);
				}
			}
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}

		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");

		scanAndSkipCommentsAndTerminators();
		return make!(ObjectBindingPatternNode)(children);
	}
	Node parseIfStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "if");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of IfStatement");
		scanAndSkipCommentsAndTerminators();
		Node cond = parseExpression(Goal.In | attributes.mask!(Goal.Yield));
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of IfStatement");
		scanAndSkipCommentsAndTerminators();
		Node truth = parseStatement(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.Identifier || token.match != "else")
			return make!(IfStatementNode)(cond,truth);
		scanAndSkipCommentsAndTerminators();
		Node elsePath = parseStatement(attributes);
		return make!(IfStatementNode)(cond,truth,elsePath);
	}
	Node parseSwitchStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "switch");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of SwitchStatement");
		scanToken(attributes);
		Node[] children = [parseExpression(Goal.In | attributes.mask!(Goal.Yield))];
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of SwitchStatement");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening curly brace as part of SwitchStatement");
		scanToken(attributes);
		while(1)
		{
			switch(token.type)
			{
				case Type.Identifier:
					switch(token.keyword)
					{
						case Keyword.Case:
							scanToken(attributes);
							Node condition = parseExpression(Goal.In | attributes.mask!(Goal.Yield));
							Node[] caseChildren = [];
							if (token.type != Type.Colon)
								return error("Expected colon");
							scanAndSkipCommentsAndTerminators();
							// parseStatemenList should hold on "case" identifier
							caseChildren ~= parseStatementList(attributes);
							auto bodyNode = make!(CaseBodyNode)(caseChildren);
							children ~= make!(CaseNode)(condition,bodyNode);
							break;
						case Keyword.Default:
							Node[] caseChildren = [];
							scanAndSkipCommentsAndTerminators();
							if (token.type != Type.Colon)
								return error("Expected colon");
							scanAndSkipCommentsAndTerminators();
							caseChildren ~= parseStatementList(attributes);
							auto bodyNode = make!(CaseBodyNode)(caseChildren);
							children ~= make!(DefaultNode)(bodyNode);
							break;
						default:
							scanToken(attributes);
							return error("Expected case keyword");
					}
					break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					scanToken(attributes);
					break;
				case Type.CloseCurlyBrace:
					scanToken(attributes);
					return make!(SwitchStatementNode)(children);
				default:
					return error(children,format("Unexpected token %s as part of SwitchStatement",token.type));
			}
		}
	}
	Node parseDoWhileStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "do");
		scanAndSkipCommentsAndTerminators();
		Node[] children = [parseStatement(attributes.mask!(Goal.Yield,Goal.Return))];
		skipCommentsAndLineTerminators();
		if (token.type != Type.Identifier || token.match != "while")
			return error("Expected while keyword");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of DoWhileStatement");
		scanAndSkipCommentsAndTerminators();
		children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of DoWhileStatement");
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.Semicolon)
			scanAndSkipCommentsAndTerminators();
		return make!(DoWhileStatementNode)(children);
	}
	Node parseWhileStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "while");
		// todo eat lineterminators/comments
		scanToken(attributes);
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of DoWhileStatement");
		scanToken(attributes);
		Node expr = parseExpression(Goal.In | attributes.mask!(Goal.Yield));
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of DoWhileStatement");

		scanAndSkipCommentsAndTerminators(attributes);
		Node stmt = parseStatement(attributes);

		if (token.type == Type.Semicolon)
			scanToken(attributes);
		return make!(WhileStatementNode)([expr,stmt]);
	}
	Node parseForStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		Node parseOldSchoolForStatement(Node firstExpr, int attributes = 0)
		{
			Node[] children;
			children.reserve(6);
			if (firstExpr !is null)
				children ~= firstExpr;
			if (token.type != Type.Semicolon)
				return error(format("Expected semicolon, got %s",token.type));
			scanAndSkipCommentsAndTerminators();
			children ~= make!(SemicolonNode)();
			if (token.type != Type.Semicolon)
			{
				children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
			}
			if (token.type != Type.Semicolon)
				return error(format("Expected semicolon, got %s",token.type));
			scanAndSkipCommentsAndTerminators();
			children ~= make!(SemicolonNode)();
			if (token.type != Type.CloseParenthesis)
			{
				children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
				if (token.type != Type.CloseParenthesis)
					return error("Expected closing parenthesis");
			}
			scanAndSkipCommentsAndTerminators();
			children ~= parseStatement(attributes.mask!(Goal.Return,Goal.Yield));
			return make!(ForStatementNode)(ForLoop.ExprCStyle,children);
		}
		assert(token.type == Type.Identifier && token.match == "for");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected parenthesis after for keyword");

		scanAndSkipCommentsAndTerminators();

		while(1)
		{
			switch(token.type)
			{
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Var:
							scanAndSkipCommentsAndTerminators();
							auto varStmt = parseVariableDeclarationList(attributes.mask!(Goal.Yield));
							// todo what abour error
							if (varStmt.children.length == 1 && varStmt.children[0].children.length == 1 && token.type == Type.Identifier)
							{
								if (token.match == "in")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [varStmt.children[0].children[0],parseExpression(Goal.In | attributes.mask!(Goal.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
									return make!(ForStatementNode)(ForLoop.VarIn,children);
								} else if (token.match == "of")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [varStmt.children[0].children[0],parseAssignmentExpression(Goal.In | attributes.mask!(Goal.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
									return make!(ForStatementNode)(ForLoop.VarOf,children);
								}
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							Node[] children = [varStmt,make!(SemicolonNode)()];
							if (token.type != Type.Semicolon)
							{
								children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							children ~= make!(SemicolonNode)();
							if (token.type != Type.CloseParenthesis)
							{
								children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
								if (token.type != Type.CloseParenthesis)
									return error("Expected closing parenthesis");
							}
							scanAndSkipCommentsAndTerminators();
							children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
							return make!(ForStatementNode)(ForLoop.VarCStyle,children);
						case Keyword.Let:
						case Keyword.Const:
							bool constDecl = token.match == "const";
							auto lexDecl = parseLexicalDeclaration(attributes.mask!(Goal.Yield));
							if (lexDecl.children.length == 1 && lexDecl.children[0].children.length == 1 && token.type == Type.Identifier)
							{
								if (token.match == "in")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [lexDecl.children[0].children[0],parseExpression(Goal.In | attributes.mask!(Goal.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
									auto loopType = constDecl ? ForLoop.ConstIn : ForLoop.LetIn;
									return make!(ForStatementNode)(loopType,children);
								} else if (token.match == "of")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [lexDecl.children[0].children[0],parseAssignmentExpression(Goal.In | attributes.mask!(Goal.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
									auto loopType = constDecl ? ForLoop.ConstOf : ForLoop.LetOf;
									return make!(ForStatementNode)(loopType,children);
								}
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							Node[] children = [lexDecl,make!(SemicolonNode)()];
							if (token.type != Type.Semicolon)
							{
								children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							children ~= make!(SemicolonNode)();
							if (token.type != Type.CloseParenthesis)
							{
								children ~= parseExpression(Goal.In | attributes.mask!(Goal.Yield));
								if (token.type != Type.CloseParenthesis)
									return error("Expected closing parenthesis");
							}
							scanAndSkipCommentsAndTerminators();
							children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
							auto loopType = constDecl ? ForLoop.ConstCStyle : ForLoop.LetCStyle;
							return make!(ForStatementNode)(loopType,children);
						default:
							break;//return error(format("Invalid identifier %s in IterationStatement",token.match));
					}
					goto default;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					scanAndSkipCommentsAndTerminators();
					break;
				case Type.Semicolon:
					return parseOldSchoolForStatement(null,attributes);
				default:
					auto expr = parseExpression(attributes.mask!(Goal.Yield));
					skipCommentsAndLineTerminators();
					if (token.type == Type.Identifier)
					{
						if (expr.isEitherA!(NodeType.UnaryExpressionNode,NodeType.BinaryExpressionNode,NodeType.ConditionalExpressionNode,NodeType.ExpressionNode,NodeType.AssignmentExpressionNode))
							return error(format("Expected LeftHandSideExpression, instead got %s",expr.type));
						if (token.match == "in")
						{
							scanAndSkipCommentsAndTerminators();
							Node[] children = [expr,parseExpression(Goal.In | attributes.mask!(Goal.Yield))];
							//todo eat comments and lineterminators
							if (token.type != Type.CloseParenthesis)
								return error("Expected closing parenthesis");
							scanAndSkipCommentsAndTerminators();
							//todo eat comments and lineterminators
							children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
							return make!(ForStatementNode)(ForLoop.ExprIn,children);
						} else if (token.match == "of")
						{
							scanAndSkipCommentsAndTerminators();
							Node[] children = [expr,parseAssignmentExpression(Goal.In | attributes.mask!(Goal.Yield))];
							//todo eat comments and lineterminators
							if (token.type != Type.CloseParenthesis)
								return error("Expected closing parenthesis");
							scanAndSkipCommentsAndTerminators();
							//todo eat comments and lineterminators
							children ~= parseStatement(attributes.mask!(Goal.Yield,Goal.Return));
							return make!(ForStatementNode)(ForLoop.ExprOf,children);
						} else
							return error("Expected either 'of' or 'in' keyword");
					}
					return parseOldSchoolForStatement(expr,attributes);
			}
		}
		assert(0);
		//return null;
	}
	Node parseLexicalDeclaration(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier);
		LexicalDeclaration decl;
		if (token.match == "let")
			decl = LexicalDeclaration.Let;
		else if (token.match == "const")
			decl = LexicalDeclaration.Const;
		else return error("Expected let or const as part of LexicalDeclaration");

		scanAndSkipCommentsAndTerminators();
		Node[] children;
		while (!isEndOfExpression) // this while loop can't run forever
		{
			Node init = null;
			Node lhs = null;
			bool requiresAssignment = false;
			switch (token.type)
			{
				case Type.OpenSquareBrackets: requiresAssignment = true; lhs = parseArrayBindingPattern(Goal.Yield); break;
				case Type.OpenCurlyBrace: requiresAssignment = true; lhs = parseObjectBindingPattern(Goal.Yield); break;
				case Type.Identifier: lhs = parseIdentifier(attributes.mask!(Goal.Yield)); break;
				default:
					return error(format("Expected ArrayBindingPattern, ObjectBindingPattern or Identifier, instead got %s",token.type));
			}
			if (token.type == Type.Assignment)
			{
				scanAndSkipCommentsAndTerminators();
				init = parseAssignmentExpression(attributes.mask!(Goal.In,Goal.Yield));
			} /*else if (requiresAssignment)
			{
				scanAndSkipCommentsAndTerminators();
				return error("BindingPattern requires initializer");
			}*/
			children ~= make!(LexicalDeclarationItemNode)(lhs,init);
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators(attributes);
		}
		if (children.length == 0)
			return error("Expected at least on lexical declaration");

		return make!(LexicalDeclarationNode)(decl,children);
	}
	Node parseWithStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "with");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected parenthesis");
		scanAndSkipCommentsAndTerminators();
		Node[] children = [parseExpression(Goal.In | attributes.mask!(Goal.Yield))];
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis");
		scanAndSkipCommentsAndTerminators();
		children ~= parseStatement(attributes);
		return make!(WithStatementNode)(children);
	}
	Node parseThrowStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "throw");
		scanToken(attributes);
		return make!(ThrowStatementNode)(parseExpression(Goal.In | attributes.mask!(Goal.Yield)));
	}
	Node parseTryStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "try");
		scanAndSkipCommentsAndTerminators();
		Node[] children = [parseBlockStatement(attributes)];

		if (token.type == Type.Identifier && token.match == "catch")
		{
			scanAndSkipCommentsAndTerminators();
			if (token.type != Type.OpenParenthesis)
				return error("Expected parenthesis");
			scanAndSkipCommentsAndTerminators();
			Node[] catchChildren;
			switch(token.type)
			{
				case Type.OpenCurlyBrace:
					catchChildren ~= parseObjectBindingPattern(attributes.mask!(Goal.Yield));
					break;
				case Type.OpenSquareBrackets:
					catchChildren ~= parseArrayBindingPattern(attributes.mask!(Goal.Yield));
					break;
				case Type.Identifier:
					catchChildren ~= parseIdentifier(attributes.mask!(Goal.Yield));
					break;
				default:
					return error("Expected ObjectBindingPattern, ArrayBindingPattern or Identifier");
			}
			if (token.type != Type.CloseParenthesis)
				return error("Expected closing parenthesis");
			scanAndSkipCommentsAndTerminators();
			catchChildren ~= parseBlockStatement(attributes);
			children ~= make!(CatchStatementNode)(catchChildren);
		}
		if (token.type == Type.Identifier && token.match == "finally")
		{
			scanAndSkipCommentsAndTerminators();
			children ~= make!(FinallyStatementNode)(parseBlockStatement(attributes));
		}
		return make!(TryStatementNode)(children);
	}
	Node parseDebuggerStatement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "debugger");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.Semicolon)
			return error("Expected semicolon after debugger keyword");
		return make!(DebuggerStatementNode)();
	}
	Node parseClassDeclaration(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "class");
		scanAndSkipCommentsAndTerminators();
		Node name;
		if (token.type == Type.Identifier)
			name = parseIdentifier();
		else if (!attributes.has!(Goal.Default))
			return error("Expected Identifier as part of ClassDeclaration");
		Node base;
		if (token.type == Type.Identifier && token.match == "extends")
		{
			scanAndSkipCommentsAndTerminators();
			base = parseLeftHandSideExpression(attributes.mask!(Goal.Yield));
		}
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace as part of class declaration");
		scanAndSkipCommentsAndTerminators();
		Node[] methods; // TODO: candidate for ArrayBuilder
		bool staticAttr = false;
		while(!isEndOfExpression || token.type == Type.Semicolon)
		{
			switch(token.type)
			{
				case Type.Identifier:
					switch (token.keyword)
					{
						case Keyword.Static:
							if (staticAttr)
								return error("Expected class method after static");
							staticAttr = true;
							scanAndSkipCommentsAndTerminators();
							continue;
						case Keyword.Set:
							if (lexer.lookAheadForAny!(Type.OpenParenthesis))
								goto default;
							methods ~= parseClassSetter(staticAttr,attributes.mask!(Goal.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
						case Keyword.Get:
							if (lexer.lookAheadForAny!(Type.OpenParenthesis))
								goto default;
							methods ~= parseClassGetter(staticAttr,attributes.mask!(Goal.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
						default:
							methods ~= parseClassMethod(staticAttr,attributes.mask!(Goal.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
					}
					staticAttr = false;
					break;
				case Type.Multiply:
					methods ~= parseClassGeneratorMethod(staticAttr,attributes.mask!(Goal.Yield));
					staticAttr = false;
					break;
				case Type.Semicolon:
					if (staticAttr)
						return error("Expected class method after static");
					scanAndSkipCommentsAndTerminators();
					continue;
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanAndSkipCommentsAndTerminators();
					continue;
				case Type.StringLiteral:
				case Type.OpenSquareBrackets:
					methods ~= parseClassMethod(staticAttr,attributes.mask!(Goal.Yield));
					if (methods[$-1].type == NodeType.ErrorNode)
						return methods[$-1];
					break;
				default:
					return error("Expected keyword static, class method, class generator, setter or getter");
			}
		}
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace as part of class declaration");
		scanAndSkipCommentsAndTerminators();
		return make!(ClassDeclarationNode)(name,base,methods);

	}
	Node parseClassGetter(bool isStatic, int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "get");
		scanAndSkipCommentsAndTerminators();
		auto name = parsePropertyName(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected empty parenthesis as part of class getter");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected empty parenthesis as part of class getter");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanAndSkipCommentsAndTerminators(attributes);
		return make!(ClassGetterNode)(isStatic,name,funcBody);
	}
	Node parseClassSetter(bool isStatic, int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "set");
		scanAndSkipCommentsAndTerminators();
		auto name = parsePropertyName(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of class setter");
		scanToken(attributes);
		auto param = parseBindingElement();
		if (param.type == NodeType.ErrorNode)
			return param;
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of class setter");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanToken(attributes);
		return make!(ClassSetterNode)(isStatic,name,param,funcBody);
	}
	Node parseBindingElement(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		skipCommentsAndLineTerminators();
		switch(token.type)
		{
			case Type.OpenCurlyBrace: 
				auto pattern = parseObjectBindingPattern(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return pattern;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Goal.In | attributes);
				return make!(BindingElementNode)(pattern,expr);
			case Type.OpenSquareBrackets:
				auto pattern = parseArrayBindingPattern(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return pattern;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Goal.In | attributes);
				return make!(BindingElementNode)(pattern,expr);
			case Type.Identifier:
				auto name = parseIdentifier(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return name;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Goal.In | attributes);
				return make!(SingleNameBindingNode)(name,expr);
			default:
				return error(format("Expected BindingElement, instead got %s",token));
		}
	}
	Node parseClassMethod(bool isStatic, int attributes = 0, Node name = null)
	{
		mixin(traceFunction!(__FUNCTION__));
		if (name is null)
		{
			name = parsePropertyName(attributes);
			skipCommentsAndLineTerminators();
		}
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of class method");
		scanAndSkipCommentsAndTerminators();
		auto params = parseFormalParameterList();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of class method");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanToken(attributes);
		return make!(ClassMethodNode)(isStatic,name,params,funcBody);
	}
	Node parseFormalParameterList(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		ArrayBuilder!Node children;
		skipCommentsAndLineTerminators();
		while(!isEndOfExpression)
		{
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children.put(make!(RestElementNode)(parseIdentifier(attributes)));
				break;
			}
			children.put(parseBindingElement(attributes));
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		return make!(FormalParameterListNode)(children.data);
	}
	Node parseClassGeneratorMethod(bool isStatic, int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Multiply);
		scanAndSkipCommentsAndTerminators();
		auto name = parsePropertyName(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of class method");
		scanToken(attributes);
		auto params = parseFormalParameterList(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of class method");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody(Goal.Yield);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanToken(attributes);
		return make!(ClassGeneratorMethodNode)(isStatic,name,params,funcBody);
	}
	Node parsePropertyName(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		if (token.type == Type.OpenSquareBrackets)
		{
			scanToken(attributes);
			skipCommentsAndLineTerminators();
			auto expr = parseAssignmentExpression(Goal.In | attributes.mask!(Goal.Yield));
			if (token.type != Type.CloseSquareBrackets)
				return error("Expected closing square brace after ComputedPropertyName");
			scanAndSkipCommentsAndTerminators();
			return make!(ComputedPropertyNameNode)(expr);
		}
		switch(token.type)
		{
			case Type.Identifier: return parseIdentifierName();
			case Type.StringLiteral: auto node = make!(StringLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.BinaryLiteral: auto node = make!(BinaryLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.OctalLiteral: auto node = make!(OctalLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.DecimalLiteral: auto node = make!(DecimalLiteralNode)(token.match); scanToken(attributes); return node;
			case Type.HexLiteral: auto node = make!(HexLiteralNode)(token.match); scanToken(attributes); return node;
			default:
				return error(format("Unexpected token %s in PropertyName",token.type));
		}
	}
	Node parseFunctionDeclaration(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		assert(token.type == Type.Identifier && token.match == "function");
		scanAndSkipCommentsAndTerminators();
		bool generator = false;
		if (token.type == Type.Multiply)
		{
			generator =  true;
			scanAndSkipCommentsAndTerminators();
		}
		Node name = null;
		if (attributes.has!(Goal.Default))
		{
			if (token.type != Type.OpenParenthesis)
			{
				name = parseIdentifier(attributes);
			}
		} else
		{
			if (token.type != Type.Identifier)
				return error("Expected Identifier as part of a FunctionDeclaration");
			name = parseIdentifier(attributes);
		}
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of function declaration");
		scanAndSkipCommentsAndTerminators();
		auto params = parseFormalParameterList();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of function declaration");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = generator ? parseFunctionBody(Goal.Yield) : parseFunctionBody();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error([funcBody],"Expected closing curly brace");
		scanAndSkipCommentsAndTerminators();
		if (generator)
			return make!(GeneratorDeclarationNode)(name,params,funcBody);
		return make!(FunctionDeclarationNode)(name,params,funcBody);
	}
	Node parseFunctionBody(int attributes = 0)
	{
		mixin(traceFunction!(__FUNCTION__));
		return make!(FunctionBodyNode)(parseStatementList(Goal.Return | attributes));
	}
}

auto parser(const(ubyte)[] i, bool hasSentinal = false, int flags = Parser.Flags.None)
{
	if (hasSentinal)
		return new Parser(i, flags);
	static const(ubyte)[16] sentinal = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
	const(ubyte)[] input = i ~ sentinal;
	return new Parser(input, flags);
}

auto parser(string i, int flags = Parser.Flags.None)
{
	return parser(cast(const(ubyte)[])i, false, flags);
}

@("parsePrimaryExpression")
unittest
{
	import std.range : empty;
	import std.stdio;
	auto assertNodeType(Type)(string r)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parsePrimaryExpression();
		parser.lexer.s[0].shouldEqual(0);
		return n.shouldBeOfType!(Type);
	}
	assertNodeType!HexLiteralNode("0x0123");
	assertNodeType!StringLiteralNode(`"abcd"`);
	assertNodeType!BinaryLiteralNode(`0b0101`);
	assertNodeType!OctalLiteralNode(`0o777`);
	assertNodeType!DecimalLiteralNode(`1234`);
	assertNodeType!DecimalLiteralNode(`0`);
	assertNodeType!DecimalLiteralNode(`0.1`);
	assertNodeType!HexLiteralNode(`0xfaf0`);
	assertNodeType!TemplateLiteralNode("`template`");
	assertNodeType!RegexLiteralNode("/abc/gi");
	assertNodeType!IdentifierReferenceNode("identifier");
	assertNodeType!ParenthesisNode("()");
	assertNodeType!KeywordNode("this").keyword.shouldEqual(Keyword.This);
	assertNodeType!KeywordNode("null").keyword.shouldEqual(Keyword.Null);
	assertNodeType!BooleanNode("true").value.shouldBeTrue();
	assertNodeType!BooleanNode("false").value.shouldBeFalse();
	assertNodeType!TemplateLiteralNode("`${double(2)} != null ? ${double(2)} : {}`");
}
@("parseUnaryExpression")
unittest
{
	alias parseUnaryExpression(Type = UnaryExpressionNode) = parseNode!("parseUnaryExpression",Type);

	void assertUnaryExpressionPrefix(bool remains = true)(string r, Prefix[] prefixs, Postfix postfix = Postfix.None, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto unExpr = parseUnaryExpression(r,remains,file,line);
		unExpr.prefixs.length.shouldEqual(prefixs.length,file,line);
		foreach(idx, prefix; prefixs)
			unExpr.prefixs[idx].shouldBeOfType!(PrefixExpressionNode).prefix.shouldEqual(prefix);
		unExpr.postfix.shouldEqual(postfix);
	}
	assertUnaryExpressionPrefix("delete abc",[Prefix.Delete]);
	assertUnaryExpressionPrefix("void abc",[Prefix.Void]);
	assertUnaryExpressionPrefix("void 0",[Prefix.Void]);
	assertUnaryExpressionPrefix("typeof abc",[Prefix.Typeof]);
	assertUnaryExpressionPrefix("++abc",[Prefix.Increment]);
	assertUnaryExpressionPrefix("--abc",[Prefix.Decrement]);
	assertUnaryExpressionPrefix("+abc",[Prefix.Positive]);
	assertUnaryExpressionPrefix("-abc",[Prefix.Negative]);
	assertUnaryExpressionPrefix("~abc",[Prefix.Tilde]);
	assertUnaryExpressionPrefix("!abc",[Prefix.Negation]);
	assertUnaryExpressionPrefix("delete void++!abc",[Prefix.Delete,Prefix.Void,Prefix.Increment,Prefix.Negation]);
	assertUnaryExpressionPrefix("typeof !abc",[Prefix.Typeof,Prefix.Negation]);
	assertUnaryExpressionPrefix("typeof !abc++",[Prefix.Typeof,Prefix.Negation],Postfix.Increment);
	assertUnaryExpressionPrefix("typeof /**/abc /**/--",[Prefix.Typeof],Postfix.Decrement);
	assertUnaryExpressionPrefix("typeof /*\n*/abc /**/--",[Prefix.Typeof],Postfix.Decrement);
	assertUnaryExpressionPrefix!(false)("typeof /*\n*/abc /*\n*/--",[Prefix.Typeof]);
	assertUnaryExpressionPrefix!(false)("typeof //comment \n!\nabc /* multi \n line */\n--",[Prefix.Typeof,Prefix.Negation]);
	parseUnaryExpression!(BooleanNode)("true").value.shouldBeTrue();
}
@("parseLeftHandSideExpression")
unittest
{
	import std.range : empty;
	import std.format : format;
	import std.stdio;
	auto assertNodeType(Type)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseLeftHandSideExpression();
		if (parser.lexer.s[0] != 0)
			throw new UnitTestException([format("Expected input to be empty, got %s",cast(const(ubyte)[])parser.lexer.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}
	//TODO: find a way to test super, new, NewTarget, arguments, literals, arrayindexes and accessors with comments interspered

	auto callNode = assertNodeType!CallExpressionNode("0x0123 //comment \n()");
	callNode.children.length.shouldEqual(2);
	callNode.children[0].shouldBeOfType!HexLiteralNode;
	callNode.children[1].shouldBeOfType!ArgumentsNode;

	callNode = assertNodeType!CallExpressionNode("\"abcd\" /* multi \n line \r\n\n comment */ . //comment \n identifier");
	callNode.children.length.shouldEqual(2);
	callNode.children[0].shouldBeOfType!StringLiteralNode;
	callNode.children[1].shouldBeOfType!(AccessorNode).identifier.shouldEqual("identifier");

	auto newNode = assertNodeType!NewExpressionNode("new new 0b0101");
	newNode.news.shouldEqual(2);
	newNode.children.length.shouldEqual(1);
	newNode.children[0].shouldBeOfType!BinaryLiteralNode;

	callNode = assertNodeType!CallExpressionNode("new 0o777()");
	callNode.news.shouldEqual(1);
	callNode.children.length.shouldEqual(2);
	callNode.children[0].shouldBeOfType!OctalLiteralNode;
	callNode.children[1].shouldBeOfType!ArgumentsNode;

	newNode = assertNodeType!NewExpressionNode("new new 1234()");
	newNode.news.shouldEqual(2);
	newNode.children.length.shouldEqual(2);
	newNode.children[0].shouldBeOfType!DecimalLiteralNode;
	newNode.children[1].shouldBeOfType!ArgumentsNode;

	callNode = assertNodeType!CallExpressionNode("new /* comment */ new // comment \n0xfaf0 /* cooment \r\n\n */\n\n()//comment\n\n()");
	callNode.news.shouldEqual(2);
	callNode.children.length.shouldEqual(3);
	callNode.children[0].shouldBeOfType!HexLiteralNode;
	callNode.children[1].shouldBeOfType!ArgumentsNode;
	callNode.children[2].shouldBeOfType!ArgumentsNode;

	assertNodeType!TemplateLiteralNode("`template`");
	assertNodeType!RegexLiteralNode("/abc/gi");
	assertNodeType!IdentifierReferenceNode("identifier");
	assertNodeType!ParenthesisNode("()");
	assertNodeType!KeywordNode("this").keyword.shouldEqual(Keyword.This);
	assertNodeType!KeywordNode("null").keyword.shouldEqual(Keyword.Null);
	assertNodeType!BooleanNode("true").value.shouldBeTrue();
	assertNodeType!BooleanNode("false").value.shouldBeFalse();

	auto parser = parser("noop\nnew ReaddirReq(path, cb)");
	parser.scanToken();
	auto n = parser.parseLeftHandSideExpression();
	n.shouldBeOfType!IdentifierReferenceNode;
}
@("parseArguments")
unittest
{
	alias parseArguments = parseNode!("parseArguments",ArgumentsNode);
	alias parseArgumentsError = parseNode!("parseArguments",ArgumentsNode,Parser.Flags.None,false);
	parseArguments(`()`);
	parseArguments(`(a)`).children.length.shouldEqual(1);
	parseArguments(`(a,b,c)`).children.length.shouldEqual(3);
	parseArguments(`(a,b,c,)`).children.length.shouldEqual(3);
	parseArguments("(a\n,\nb\n,\nc\n,\n)").children.length.shouldEqual(3);
	parseArgumentsError("(،ystem');").shouldThrowSaying("Invalid character 216 to start identifier");
}
@("parseRightHandSideExpression")
unittest
{
	BinaryExpressionNode assertBinaryExpression(in string r, ExpressionOperator[] ops, in string file = __FILE__, in size_t line = __LINE__) @trusted
	{
		import std.range : lockstep, stride, drop;
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseRightHandSideExpression(Goal.In);
		if (!parser.lexer.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",cast(const(ubyte)[])parser.lexer.s)],file,line);
		auto t = shouldBeOfType!(BinaryExpressionNode)(n,file,line);
		t.children.length.shouldEqual(ops.length*2 + 1);
		foreach(node,op; t.children.drop(1).stride(2).lockstep(ops))
		{
			auto exop = node.shouldBeOfType!(ExpressionOperatorNode);
			exop.operator.shouldEqual(op);
		}
		return t;
	}
	assertBinaryExpression("a instanceof b",[ExpressionOperator.InstanceOf]);
	assertBinaryExpression("a in b",[ExpressionOperator.In]);
	assertBinaryExpression("a && b",[ExpressionOperator.LogicalAnd]);
	assertBinaryExpression("a || b",[ExpressionOperator.LogicalOr]);
	assertBinaryExpression("a & b",[ExpressionOperator.BitwiseAnd]);
	assertBinaryExpression("a | b",[ExpressionOperator.BitwiseOr]);
	assertBinaryExpression("a ^ b",[ExpressionOperator.BitwiseXor]);
	assertBinaryExpression("a === b",[ExpressionOperator.StrictEqual]);
	assertBinaryExpression("a == b",[ExpressionOperator.Equal]);
	assertBinaryExpression("a !== b",[ExpressionOperator.StrictNotEqual]);
	assertBinaryExpression("a != b",[ExpressionOperator.NotEqual]);
	assertBinaryExpression("a <= b",[ExpressionOperator.LessOrEqual]);
	assertBinaryExpression("a < b",[ExpressionOperator.LessThan]);
	assertBinaryExpression("a >= b",[ExpressionOperator.GreaterOrEqual]);
	assertBinaryExpression("a > b",[ExpressionOperator.GreaterThan]);
	assertBinaryExpression("a << b",[ExpressionOperator.LeftShift]);
	assertBinaryExpression("a >>> b",[ExpressionOperator.TripleRightSift]);
	assertBinaryExpression("a >> b",[ExpressionOperator.RightShift]);
	assertBinaryExpression("a + b",[ExpressionOperator.Add]);
	assertBinaryExpression("a - b",[ExpressionOperator.Minus]);
	assertBinaryExpression("a * b",[ExpressionOperator.Multiply]);
	assertBinaryExpression("a / b",[ExpressionOperator.Division]);
	assertBinaryExpression("a % b",[ExpressionOperator.Mod]);

	assertBinaryExpression("a & b || c !== d",[ExpressionOperator.BitwiseAnd,ExpressionOperator.LogicalOr,ExpressionOperator.StrictNotEqual]);

	parseNode!("parseRightHandSideExpression",BooleanNode)("true").value.shouldEqual(true);
	parseNode!("parseRightHandSideExpression",BooleanNode)("false").value.shouldEqual(false);
}
@("parseConditionalExpression")
unittest
{
	alias parseConditionalExpression(Type = ConditionalExpressionNode) = parseNode!("parseConditionalExpression",Type);

	parseConditionalExpression!(IdentifierReferenceNode)("abc");
	parseConditionalExpression("abc ? 6 : 7");
	parseConditionalExpression("abc\n?\n6\n:\n7");
	parseConditionalExpression("abc ?").shouldThrow();
	parseConditionalExpression("abc ? 6").shouldThrow();
	parseConditionalExpression("abc ? 6 :").shouldThrow();
}
@("parseAssignmentExpression")
unittest
{
	alias parseAssignmentExpression(Type = AssignmentExpressionNode) = parseNode!("parseAssignmentExpression",Type);

	parseAssignmentExpression!(IdentifierReferenceNode)("abc");
	auto assign = parseAssignmentExpression("abc12 = def");
	assign.children.length.shouldEqual(3);
	assign.children[0].shouldBeOfType!IdentifierReferenceNode;
	assign.children[1].shouldBeOfType!AssignmentOperatorNode;
	assign.children[2].shouldBeOfType!IdentifierReferenceNode;

	parseAssignmentExpression!(ConditionalExpressionNode)("abc ? 6 : 7");
	parseAssignmentExpression!(UnaryExpressionNode)("!bla");
	parseAssignmentExpression!(BinaryExpressionNode)("bla & 7");

	parseAssignmentExpression("abc <<= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.LeftShiftAssignment);
	parseAssignmentExpression("abc >>>= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.TripleRightShiftAssignment);
	parseAssignmentExpression("abc >>= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.RightShiftAssignment);
	parseAssignmentExpression("abc = 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.Assignment);
	parseAssignmentExpression("abc += 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.AdditiveAssignment);
	parseAssignmentExpression("abc -= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.DecrementalAssignment);
	parseAssignmentExpression("abc *= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.MultiplicativeAssignment);
	parseAssignmentExpression("abc /= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.DivisionAssignment);
	parseAssignmentExpression("abc %= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.ModAssignment);
	parseAssignmentExpression("abc &= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.BitwiseAndAssignment);
	parseAssignmentExpression("abc |= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.BitwiseOrAssignment);
	parseAssignmentExpression("abc ^= 7").children[1].shouldBeOfType!(AssignmentOperatorNode).assignment.shouldEqual(Assignment.BitwiseXorAssignment);

	parseAssignmentExpression("abc = def ? 6 : 7").children[2].shouldBeOfType!ConditionalExpressionNode;
	parseAssignmentExpression("abc = !bla").children[2].shouldBeOfType!UnaryExpressionNode;
	parseAssignmentExpression("abc = bla & 7").children[2].shouldBeOfType!BinaryExpressionNode;
	parseAssignmentExpression("abc = def *= 7");
	parseAssignmentExpression("abc.b = () => { }").children[2].shouldBeOfType!ArrowFunctionNode;
	parseAssignmentExpression("abc.b = (\na,\nb,\nc) => { }").children[2].shouldBeOfType!ArrowFunctionNode;
	parseAssignmentExpression("abc.b = (\na,\nb,\nc) => \n { b = {}; }").children[2].shouldBeOfType!ArrowFunctionNode;
}
@("parseExpression")
unittest
{
	alias parseExpression(Type = ExpressionNode) = parseNode!("parseExpression",Type);
	parseExpression!(IdentifierReferenceNode)("abc");
	parseExpression!(ConditionalExpressionNode)("abc ? 7 : 6");
	parseExpression!(UnaryExpressionNode)("!bla");
	parseExpression!(BinaryExpressionNode)("bla & 7");
	parseExpression("abc() // \n, /* multi \n line \r\n comment */ \n \r\n def = ghi");
	parseExpression(",").shouldThrowSaying("Expected AssignmentExpression instead got comma");
	parseExpression("bla,,").shouldThrowSaying("Expected AssignmentExpression instead got comma");
	parseExpression("").shouldThrowSaying("Expected AssignmentExpression");

	auto expr = parseExpression("abc, def");
	expr.children.length.shouldEqual(2);
	expr.children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("abc");
	expr.children[1].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("def");

	expr = parseExpression("abc(), def = ghi");
	expr.children.length.shouldEqual(2);
	expr.children[0].shouldBeOfType!(CallExpressionNode);
	expr.children[1].shouldBeOfType!(AssignmentExpressionNode);

	auto str = parseExpression!StringLiteralNode(`"string ' with quotes '"`);
	str.value.shouldEqual("string \\' with quotes \\'");
	str = parseExpression!StringLiteralNode(`'string " with quotes "'`);
	str.value.shouldEqual("string \" with quotes \"");
}
@("parseStatement")
unittest
{
	alias parseStatement(Type) = parseNode!("parseStatement", Type);

	parseStatement!(IdentifierReferenceNode)("abc").identifier.shouldEqual("abc");
	parseStatement!(IdentifierReferenceNode)("abc;");
	parseStatement!(IdentifierReferenceNode)("abc  \n\r\n;");
	parseStatement!(IdentifierReferenceNode)("abc  \n\r\n // \n;");
	parseStatement!(IdentifierReferenceNode)("abc  \n\r\n /* multi \n line \r\n comment */ \n;");
	parseStatement!(LabelledStatementNode)("abc  \n\r\n /* multi \n line \r\n comment */ \n:;");
	parseStatement!(ContinueStatementNode)("continue label;").label.shouldEqual("label");
	parseStatement!(BreakStatementNode)("break label;").label.shouldEqual("label");
	parseStatement!(LabelledStatementNode)("bla: var a = 0;").children[0].shouldBeOfType!(VariableStatementNode);
	parseStatement!(LabelledStatementNode)("bla\n:\nvar a = 0;").children[0].shouldBeOfType!(VariableStatementNode);
	parseStatement!(LabelledStatementNode)("bla\n:\n{ var a = 0; }").children[0].shouldBeOfType!(BlockStatementNode);


	parseStatement!(BinaryExpressionNode)("a in a;");
	parseStatement!(BinaryExpressionNode)(`a.bla in hup;`).children[$-1].shouldBeOfType!(IdentifierReferenceNode);
}
@("parseVariableStatement")
unittest
{
	alias parseVariableStatement = parseNode!("parseVariableStatement",VariableStatementNode);

	parseVariableStatement("var ").shouldThrow();
	parseVariableStatement("var a").children[0].shouldBeOfType!(VariableDeclarationNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseVariableStatement("var a,b").children[1].shouldBeOfType!(VariableDeclarationNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("b");
	parseVariableStatement("var a,\nb").children[1].shouldBeOfType!(VariableDeclarationNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("b");
	parseVariableStatement("var a = 77").children[0].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("77");
	parseVariableStatement("var a = b = 77").children[0].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(AssignmentExpressionNode);
	parseVariableStatement("var a = b = 77, c = 44").children[1].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("44");
	parseVariableStatement("var /* multi \r\n line \n comment */ \r\n // \n \n a");
}
@("parseLexicalDeclaration")
unittest
{
	alias parseLexicalDeclaration = parseNode!("parseLexicalDeclaration",LexicalDeclarationNode);

	parseLexicalDeclaration("let ").shouldThrow();
	parseLexicalDeclaration("let a").children[0].shouldBeOfType!(LexicalDeclarationItemNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseLexicalDeclaration("let a,b").children[1].shouldBeOfType!(LexicalDeclarationItemNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("b");
	parseLexicalDeclaration("let a,\nb").children[1].shouldBeOfType!(LexicalDeclarationItemNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("b");
	parseLexicalDeclaration("let a = 77").children[0].shouldBeOfType!(LexicalDeclarationItemNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("77");
	parseLexicalDeclaration("let a = b = 77").children[0].shouldBeOfType!(LexicalDeclarationItemNode).children[1].shouldBeOfType!(AssignmentExpressionNode);
	parseLexicalDeclaration("let a = b = 77, c = 44").children[1].shouldBeOfType!(LexicalDeclarationItemNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("44");
	parseLexicalDeclaration("let /* multi \r\n line \n comment */ \r\n // \n \n a");
}
@("parseYieldExpression")
unittest
{
	alias parseYieldExpression = parseNode!("parseYieldExpression",YieldExpressionNode);

	parseYieldExpression(`yield`);
	parseYieldExpression(`yield a`).children.length.shouldEqual(1);
	parseYieldExpression(`yield a`).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseYieldExpression(`yield /* comment without newline is ok */ a`).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseYieldExpression("yield \n a", false).assignExpr.shouldBeNull();
	// TODO:
	//parseYieldExpression("yield /* when it has a newline \n parsing stops */ a").assignExpr.shouldBeNull();
	parseYieldExpression(`yield * a`).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseYieldExpression(`yield * a`)._delegate.shouldBeTrue();
	parseYieldExpression("yield * // a comment here is ok \n a")._delegate.shouldBeTrue();
	parseYieldExpression("yield * // a comment here is ok \n a").children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseYieldExpression("yield * /* also a multiline \n with newlines */ a").children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
	parseYieldExpression("yield * \n a").children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("a");
}
@("yield")
unittest
{
	parseModule(`(function * () {
		var a = require('a');
		var b = yield require('c')(a);
	})();`);
}
@("parseForStatement")
unittest
{
	alias parseForStatement = parseNode!("parseForStatement",ForStatementNode);

	parseForStatement("for (var i = 0; i < 10; i++) {}").loopType.shouldEqual(ForLoop.VarCStyle);
	parseForStatement("for (let i = 0; i < 10; i++) {}").loopType.shouldEqual(ForLoop.LetCStyle);
	parseForStatement("for (const i = 0; i < 10; i++) {}").loopType.shouldEqual(ForLoop.ConstCStyle);
	parseForStatement("for (i = 0; i < 10; i++) {}").loopType.shouldEqual(ForLoop.ExprCStyle);
	parseForStatement("for (let a in b) {}").loopType.shouldEqual(ForLoop.LetIn);
	parseForStatement("for (let a of b) {}").loopType.shouldEqual(ForLoop.LetOf);
	parseForStatement("for (const a in b) {}").loopType.shouldEqual(ForLoop.ConstIn);
	parseForStatement("for (const a of b) {}").loopType.shouldEqual(ForLoop.ConstOf);
	parseForStatement("for (var a in b) {}").loopType.shouldEqual(ForLoop.VarIn);
	parseForStatement("for (var a of b) {}").loopType.shouldEqual(ForLoop.VarOf);
	parseForStatement("for (a in b) {}").loopType.shouldEqual(ForLoop.ExprIn);
	parseForStatement("for (a of b) {}").loopType.shouldEqual(ForLoop.ExprOf);
	//parseForStatement("for (let in b) {}").loopType.shouldEqual(ForLoop.ExprIn); //TODO: these are valid (except maybe "of")
	//parseForStatement("for (let of b) {}").loopType.shouldEqual(ForLoop.ExprOf);
	//parseForStatement("for (const in b) {}").loopType.shouldEqual(ForLoop.ExprIn);
	//parseForStatement("for (const of b) {}").loopType.shouldEqual(ForLoop.ExprOf);
}
@("parseObjectLiteral")
unittest
{
	alias parseObjectLiteral = parseNode!("parseObjectLiteral",ObjectLiteralNode);

	parseObjectLiteral("{}");
	parseObjectLiteral(`{obj:obj}`);
	parseObjectLiteral(`{obj}`);
	parseObjectLiteral(`{get,set}`);
	parseObjectLiteral(`{get:4,set:3}`);
	parseObjectLiteral(`{get a(){return 4},set a(a){}}`);
	parseObjectLiteral("{a(){}}");
	parseObjectLiteral("{*a(){}}");
	parseObjectLiteral("{\nobj:obj//comment\n,/*multiline \n*/a,b}");
	parseObjectLiteral(`{"string":4,123:decimal,0o123:octal,0xffa:hex,0b01:binary}`);
	parseObjectLiteral("{[a+b]:c}");
	parseObjectLiteral("{[a+b]:c}");
	parseObjectLiteral("{a:1,b:2}");
	parseObjectLiteral("{a:1,b:2,}");
	parseObjectLiteral("{c=6}");
	parseObjectLiteral(`{"a"() { f() } }`);
	parseObjectLiteral("{c=6").shouldThrowSaying("Expected closing curly brace before EndOfFile");
	parseObjectLiteral(`{"abc"}`).shouldThrowSaying("Expected colon as part of PropertyDefinition");
	//parseObjectLiteral(`{function}`).shouldThrowSaying("Unexpected keyword function");
	parseObjectLiteral(`{,}`).shouldThrowSaying("Expected a PropertyDefinition");
	parseObjectLiteral(`{}`).shouldThrowSaying("Here we test whether shouldThrowSaying fails when the expr doesn't throw").shouldThrow;

}
@("parseArrayLiteral")
unittest
{
	alias parseArrayLiteral = parseNode!("parseArrayLiteral",ArrayLiteralNode);

	parseArrayLiteral(`[]`);
	parseArrayLiteral(`[a]`);
	parseArrayLiteral(`[a,b]`);
	parseArrayLiteral(`[a,b,]`);
}
@("parseFunctionExpression")
unittest
{
	alias parseFunctionExpression = parseNode!("parseFunctionExpression",FunctionExpressionNode);

	parseFunctionExpression("function(){}");
	parseFunctionExpression("function(){;}");
	parseFunctionExpression("function(){return}");
	parseFunctionExpression("function(){return 4}");
}
@("parseFunctionDeclaration")
unittest
{
	alias parseFunctionDeclaration = parseNode!("parseFunctionDeclaration",FunctionDeclarationNode);

	parseFunctionDeclaration("function a(){return}");
}
@("parseObjectBindingPattern")
unittest
{
	alias parseObjectBindingPattern = parseNode!("parseObjectBindingPattern",ObjectBindingPatternNode);

	parseObjectBindingPattern(`{h}`);
	parseObjectBindingPattern(`{o:bla}`);
	parseObjectBindingPattern(`{j=4}`);
	parseObjectBindingPattern(`{[p]:g}`);
	parseObjectBindingPattern(`{[p+"45"]:g}`);
	parseObjectBindingPattern(`{h,o:bla,j=4,[p+"54"]:g}`);
}
@("parseArrayBindingPattern")
unittest
{
	alias parseArrayBindingPattern = parseNode!("parseArrayBindingPattern",ArrayBindingPatternNode);

	parseArrayBindingPattern(`[,,[a,b],l]`);
	parseArrayBindingPattern(`[l,,m,k=5]`);
	parseArrayBindingPattern(`[,,[a,b],l,,,m,k=5,{h:p},...rest]`);
}
@("parseDoWhileStatement")
unittest
{
	alias parseDoWhileStatement = parseNode!("parseDoWhileStatement",DoWhileStatementNode);

	parseDoWhileStatement("do a;while(true)");
	parseDoWhileStatement("do\na;\nwhile\n(true)");
}
@("parseStatementList")
unittest
{
	auto parseStatementList(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		return parser.parseStatementList();
	}

	parseStatementList("").length.shouldEqual(0);
	parseStatementList(";").length.shouldEqual(0);
}
@("parseIdentifier")
unittest
{
	alias parseIdentifier = parseNode!("parseIdentifier",IdentifierReferenceNode);

	parseIdentifier("name");
	parseIdentifier("name_");
	parseIdentifier("_name");
}
@("parseClassDeclaration")
unittest
{
	alias parseClassDeclaration = parseNode!("parseClassDeclaration",ClassDeclarationNode);

	parseClassDeclaration("class abc{}").name.shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("abc");
	parseClassDeclaration("class abc extends def{}").base.shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("def");
	parseClassDeclaration("class abc{m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{m(){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{get(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{get(abc){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("get");
	parseClassDeclaration("class abc{set(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{set(abc){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("set");
	parseClassDeclaration("class abc{*m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{*m(){}}").methods[0].shouldBeOfType!(ClassGeneratorMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{set m(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{set m(abc){}}").methods[0].shouldBeOfType!(ClassSetterNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{get m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{get m(){}}").methods[0].shouldBeOfType!(ClassGetterNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static m(){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static *m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static *m(){}}").methods[0].shouldBeOfType!(ClassGeneratorMethodNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static set m(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static set m(abc){}}").methods[0].shouldBeOfType!(ClassSetterNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static get m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static get m(){}}").methods[0].shouldBeOfType!(ClassGetterNode).children[0].shouldBeOfType!(IdentifierNameNode).identifier.shouldEqual("m");
	parseClassDeclaration(`class abc{"bla"(){}}`).methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(StringLiteralNode).value.shouldEqual("bla");
	parseClassDeclaration(`class abc{[bla](){}}`).methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(ComputedPropertyNameNode).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("bla");
	parseClassDeclaration("class name { a(){}/*comment*/b(){}\nc(){} // comment \nd(){}; ;; }");
	parseClassDeclaration("class {}").shouldThrowSaying("Expected Identifier as part of ClassDeclaration");
	parseClassDeclaration("class name").shouldThrowSaying("Expected opening brace as part of class declaration");
	parseClassDeclaration("class name { static static }").shouldThrowSaying("Expected class method after static");
	parseClassDeclaration("class name { static ; }").shouldThrowSaying("Expected class method after static");
	parseClassDeclaration("class name { + }").shouldThrowSaying("Expected keyword static, class method, class generator, setter or getter");
	parseClassDeclaration("class abc{").shouldThrowSaying("Expected closing curly brace as part of class declaration");
	parseClassDeclaration("class abc{get m").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m(").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m(a)").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m()}").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{get m(){").shouldThrowSaying("Expected closing curly brace");
	parseClassDeclaration("class abc{set m").shouldThrowSaying("Expected opening parenthesis as part of class setter");
	parseClassDeclaration("class abc{set m(){}").shouldThrowSaying("Expected BindingElement");
	parseClassDeclaration("class abc{set m(a,b){}").shouldThrowSaying("Expected closing parenthesis as part of class setter");
	parseClassDeclaration("class abc{set m(a)").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{set m(a){").shouldThrowSaying("Expected closing curly brace");
	parseClassDeclaration("class abc{m").shouldThrowSaying("Expected opening parenthesis as part of class method");
	parseClassDeclaration("class abc{m(").shouldThrowSaying("Expected closing parenthesis as part of class method");
	parseClassDeclaration("class abc{m()").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{m(){").shouldThrowSaying("Expected closing curly brace");
}

// TODO: This kills the parser/lexer
	//assertCombineNestedIfs(
	//	`if (b()) if (c()) d(); else e()`,
	//	`if (b()) if (c()) d(); else e()`
	//);
@("OptionalParens")
unittest
{
	parseModule(`function asd(searchInput) {

  if ('number' === typeof searchInput) return names[searchInput]

  var search = String(searchInput)

}`); // should contain no errors

}
@("parseModule")
unittest
{
	parseModule(`a = 100 / b[4]
/huphup/.match(b)
`).findFirst(NodeType.RegexLiteralNode).shouldNotBeNull;
	parseModule(`class ParameterDefinition extends Definition { constructor(name, node, index, rest) { super(Variable.Parameter, name, node, null, index, null); } }`)
		.findFirst(NodeType.ClassDeclarationNode).shouldNotBeNull;
	parseModule("id \n ++c").findFirst(NodeType.UnaryExpressionNode).as!(UnaryExpressionNode).children[0].as!(IdentifierReferenceNode).identifier.shouldEqual(cast(const(ubyte)[])"c");
	parseModule(`function * abc() { for (var i = 0; i < 5; i++) { yield i; } }`);
	parseModule(`function * abc() { for (var i = 0; i < 5; i++) { yield i; } }`);
	parseModule(`if ( event.target.bla === 3 ) {}`);

	parseModule("function isIdentifierStart(code, astral) { if (b) return code >= 1 && 3\n return 4; }")
		.findFirst(NodeType.BinaryExpressionNode).expect!((e){
			e.children.length.shouldEqual(5);
			e.children[$-1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("3");
		});

}
@("Node flag")
unittest
{
	alias parseModule = parseNode!("parseModule",ModuleNode, Parser.Flags.Node);
	parseModule(`return 66;`).findFirst(NodeType.ReturnStatementNode).shouldNotBeNull;
	parseModule("return //bla bla\nvar a = 6;");
	parseModule("return /*bla bla\n*/var a = 6;");
}
@("parseRightHandSideExpressionBottomUp")
unittest
{
	alias parseRHSBottomUp(Type, bool prettyError = true) = parseNode!("parseRightHandSideExpressionBottomUp",Type, Parser.Flags.Node, prettyError);
	alias parseKeyword = parseRHSBottomUp!(KeywordNode);
	alias parseBoolean = parseRHSBottomUp!(BooleanNode);
	alias parseIdentifier = parseRHSBottomUp!(IdentifierReferenceNode);
	alias parseFunction = parseRHSBottomUp!(FunctionExpressionNode);
	alias parseGenerator = parseRHSBottomUp!(GeneratorExpressionNode);
	alias parseClass = parseRHSBottomUp!(ClassDeclarationNode);
	alias parseStringLiteral = parseRHSBottomUp!(StringLiteralNode);
	alias parseBinaryLiteral = parseRHSBottomUp!(BinaryLiteralNode);
	alias parseOctalLiteral = parseRHSBottomUp!(OctalLiteralNode);
	alias parseDecimalLiteral = parseRHSBottomUp!(DecimalLiteralNode);
	alias parseHexLiteral = parseRHSBottomUp!(HexLiteralNode);
	alias parseArrayLiteral = parseRHSBottomUp!(ArrayLiteralNode);
	alias parseObjectLiteral = parseRHSBottomUp!(ObjectLiteralNode);
	alias parseTemplateLiteral = parseRHSBottomUp!(TemplateLiteralNode);
	alias parseRegexLiteral = parseRHSBottomUp!(RegexLiteralNode);
	alias parseParenthesis = parseRHSBottomUp!(ParenthesisNode);
	alias parseSuperProperty = parseRHSBottomUp!(SuperPropertyNode);
	alias parseNewTarget = parseRHSBottomUp!(NewTargetNode);
	alias parseNewExpression = parseRHSBottomUp!(NewExpressionNode);
	alias parseCallExpression = parseRHSBottomUp!(CallExpressionNode);
	alias parseUnaryExpression = parseRHSBottomUp!(UnaryExpressionNode);
	alias parseBinaryExpression = parseRHSBottomUp!(BinaryExpressionNode);

	parseKeyword(`this`).keyword.shouldEqual(Keyword.This);
	parseKeyword(`null`).keyword.shouldEqual(Keyword.Null);
	parseBoolean(`true`).value.shouldBeTrue;
	parseBoolean(`false`).value.shouldBeFalse;
	parseIdentifier(`identifier`).identifier.shouldEqual("identifier");
	parseFunction(`function fun(){}`).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("fun");
	parseGenerator(`function* fun(){}`).children[0].shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("fun");
	parseClass(`class bla extends hup {}`).name.shouldBeOfType!(IdentifierReferenceNode).identifier.shouldEqual("bla");
	parseStringLiteral(`"string"`).value.shouldEqual("string");
	parseStringLiteral(`'string'`).value.shouldEqual("string");
	parseBinaryLiteral(`0b0011`).value.shouldEqual("0011");
	parseOctalLiteral(`0o0011`).value.shouldEqual("0011");
	parseDecimalLiteral(`567`).value.shouldEqual("567");
	parseHexLiteral(`0x0011`).value.shouldEqual("0011");
	parseArrayLiteral(`[a]`).children.length.shouldEqual(1);
	parseObjectLiteral(`{a: 5}`).children.length.shouldEqual(1);
	parseTemplateLiteral("`templ ${identifier}`").children.length.shouldEqual(3);
	parseRegexLiteral(`/regex/`);
	parseParenthesis(`(subExpr)`).children.length.shouldEqual(1);
	parseSuperProperty("super[expr]");
	parseSuperProperty("super.bla");
	parseNewTarget("new.target");
	parseCallExpression("identifier[expr]");
	parseCallExpression("identifier[expr][snd]");
	parseCallExpression("identifier.target.snd");
	parseCallExpression("identifier(expr)(snd)");
	parseCallExpression("identifier`tmpl``tt`");

	parseCallExpression("super[expr][expr]");
	parseCallExpression("super.bla[expr]");
	parseCallExpression("new.target[expr]");
	parseCallExpression("identifier.id");
	parseCallExpression("super[expr].id");
	parseCallExpression("super.bla.id");
	parseCallExpression("new.target.id");
	parseCallExpression("identifier`tmpl`");
	parseCallExpression("super[expr]`tmpl`");
	parseCallExpression("super.bla`tmpl`");
	parseCallExpression("new.target`tmpl`");
	parseCallExpression("identifier[expr].id`tmpl`");
	parseCallExpression("super[expr][expr].id`tmpl`");
	parseCallExpression("super.bla[expr].id`tmpl`");
	parseCallExpression("new.target[expr].id`tmpl`");
	parseCallExpression("new identifier()");	//shouldn't this be a new expression??
	parseCallExpression("new super[expr]()");	// shouldn't this be a new expression??
	parseCallExpression("new super.bla()"); // shouldn't this be a new expression??
	parseCallExpression("new new.target()");
	parseCallExpression("new identifier[expr]()");
	parseCallExpression("new super[expr][expr]()");
	parseCallExpression("new super.bla[expr]()");
	parseCallExpression("new new.target[expr]()");
	parseCallExpression("new identifier.id()");
	parseCallExpression("new super[expr].id()");
	parseCallExpression("new super.bla.id()");
	parseCallExpression("new new.target.id()");
	parseCallExpression("new identifier`tmpl`()");
	parseCallExpression("new super[expr]`tmpl`()");
	parseCallExpression("new super.bla`tmpl`()");
	parseCallExpression("new new.target`tmpl`()");
	parseCallExpression("new identifier[expr].id`tmpl`()");
	parseCallExpression("new super[expr][expr].id`tmpl`()");
	parseCallExpression("new super.bla[expr].id`tmpl`()");
	parseCallExpression("new new.target[expr].id`tmpl`()");
	parseNewExpression("new new identifier()");	//shouldn't this be a new expression??
	parseNewExpression("new new super[expr]()");	// shouldn't this be a new expression??
	parseNewExpression("new new super.bla()"); // shouldn't this be a new expression??
	parseNewExpression("new new new.target()");
	parseCallExpression("new new identifier[expr]()");	// shouldn't this be a new expression??
	parseCallExpression("new new super[expr][expr]()");	// shouldn't this be a new expression??
	parseCallExpression("new new super.bla[expr]()");	// shouldn't this be a new expression??
	parseCallExpression("new new new.target[expr]()");	// shouldn't this be a new expression??
	parseCallExpression("new new identifier.id()");	// shouldn't this be a new expression??
	parseCallExpression("new new super[expr].id()");	// shouldn't this be a new expression??
	parseCallExpression("new new super.bla.id()");	// shouldn't this be a new expression??
	parseCallExpression("new new new.target.id()");	// shouldn't this be a new expression??
	parseCallExpression("new new identifier`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new super[expr]`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new super.bla`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new new.target`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new identifier[expr].id`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new super[expr][expr].id`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new super.bla[expr].id`tmpl`()");	// shouldn't this be a new expression??
	parseCallExpression("new new new.target[expr].id`tmpl`()");	// shouldn't this be a new expression??
	
	parseUnaryExpression(`delete a`);
	parseUnaryExpression(`void a`);
	parseUnaryExpression(`typeof a`);
	parseUnaryExpression(`-- a`);
	parseUnaryExpression(`++ a`);
	parseUnaryExpression(`+ a`);
	parseUnaryExpression(`- a`);
	parseUnaryExpression(`~ a`);
	parseUnaryExpression(`! a`);
	parseUnaryExpression(`a++`).postfix.shouldEqual(Postfix.Increment);
	parseUnaryExpression(`a--`);

	parseUnaryExpression(`!delete a`);
	parseUnaryExpression(`!void a`);
	parseUnaryExpression(`!typeof a`);
	parseUnaryExpression(`!-- a`);
	parseUnaryExpression(`!++ a`);
	parseUnaryExpression(`!+ a`);
	parseUnaryExpression(`!- a`);
	parseUnaryExpression(`!~ a`);
	parseUnaryExpression(`!! a`);
	parseUnaryExpression(`!a++`);
	parseUnaryExpression(`!a--`);

	parseBinaryExpression(`a instanceof a`);
	parseBinaryExpression(`a && a`);
	parseBinaryExpression(`a || a`);
	parseBinaryExpression(`a & a`);
	parseBinaryExpression(`a | a`);
	parseBinaryExpression(`a ^ a`);
	parseBinaryExpression(`a === a`);
	parseBinaryExpression(`a == a`);
	parseBinaryExpression(`a !== a`);
	parseBinaryExpression(`a != a`);
	parseBinaryExpression(`a <= a`);
	parseBinaryExpression(`a < a`);
	parseBinaryExpression(`a >= a`);
	parseBinaryExpression(`a > a`);
	parseBinaryExpression(`a << a`);
	parseBinaryExpression(`a >>> a`);
	parseBinaryExpression(`a >> a`);
	parseBinaryExpression(`a + a`);
	parseBinaryExpression(`a - a`);
	parseBinaryExpression(`a * a`);
	parseBinaryExpression(`a / a`);
	parseBinaryExpression(`a % a`);

	parseBinaryExpression(`a && a || b`);
	parseBinaryExpression(`event.target.bla === 3`);

	parseBinaryExpression(`code >= 1 && 3`).expect!((e){
		e.children.length.shouldEqual(5);
		e.children[$-1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("3");
	});

	parseBinaryExpression(`code >= 1 && return`).shouldThrowSaying("Invalid IdentifierReference return");

	alias parseBinaryExpressionError = parseRHSBottomUp!(BinaryExpressionNode,false);

	parseBinaryExpressionError(`fals%`).shouldThrowSaying("expected PrimaryExpression before eof");
}
@("parseImportDeclaration")
unittest
{
	void assertImportDeclarationError(Type = ImportDeclarationNode)(string r, string error, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanAndSkipCommentsAndTerminators();
		auto n = parser.parseImportDeclaration();
		n.type.shouldEqual(NodeType.ErrorNode);
		import std.range : repeat;
		auto err = n.shouldBeOfType!(ErrorNode);
		import std.string : lineSplitter;
		import std.algorithm : joiner, min, max;
		import std.array : array;
		import std.range : drop,take;
		import std.stdio;
		import std.uni : isWhite, byGrapheme;
		import std.utf : byDchar, byCodeUnit;
		import std.conv : to, text;
		auto start = max(0,(cast(int)err.line)-2);
		auto pre = (cast(int)err.line)-start+1;

		auto lines = r.lineSplitter().drop(start).take(5);
		auto lead = lines;
		auto tail = lines.drop(2);
		string s = tail.take(1).front.byCodeUnit.take(err.column).map!((c){
				return c.isWhite ? c : ' ';
			}).text;
		
		string code = format("%s\n%s^ %s\n%s",lead.take(pre).joiner("\n"),s,err.value,tail.drop(1).joiner("\n"));
		code.shouldEqual(error);
	}
	//assertImportDeclarationError(`
	//	import
	//	a from;
	//	var b
	//	= 6;`,
	//	`
	//	import
	//	a from;
	//	      ^ Expected StringLiteral as part of ImportDeclaration
	//	var b
	//	= 6;`
	//);
	//assertImportDeclarationError(`/*

	//	*/


	//	import
	//	a from;
	//	var b
	//	= 6;`,
	//	`
	//	import
	//	a from;
	//	      ^ Expected StringLiteral as part of ImportDeclaration
	//	var b
	//	= 6;`
	//);
	//assertImportDeclarationError(`
	//	import

	//	/* multi
	//	line
	//	comment

	//	*/
	//	-- from "file.js";
	//	var b
	//	= 6;`,
	//	`
	//	*/
	//	-- from "file.js";
	//	^ Unexpected Decrement as part of ImportDeclaration
	//	var b
	//	= 6;`
	//);
	//assertImportDeclarationError(`
	//	import
	//	// comment
	//	// comment 2
	//	b "file.js";
	//	var b
	//	= 6;`,
	//	`		// comment
	//	// comment 2
	//	b "file.js";
	//	  ^ Expected from as part of ImportDeclaration
	//	var b
	//	= 6;`
	//);
}
@("parseInvalidUTF8")
unittest
{
	alias parseModule = parseNode!("parseModule",ModuleNode, Parser.Flags.Node, false);

	parseModule("\xFF").shouldThrowSaying("Invalid UTF8");
	parseModule("\xF0").shouldThrowSaying("Invalid UTF8");
	parseModule("\xC0").shouldThrowSaying("Invalid UTF8");
	parseModule("\xE0").shouldThrowSaying("Invalid UTF8");
	parseModule("\x81").shouldThrowSaying("Invalid UTF8");
}



