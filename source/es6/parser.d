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

import std.format : format;

import es6.lexer;
import es6.tokens;
import es6.nodes;
import es6.keywords;

version (unittest)
{
	import es6.testhelpers;
	import unit_threaded;
	import es6.reporter;
	import std.stdio;
	auto parseNode(alias parseFunction, Type = ModuleNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = __traits(getMember, parser, parseFunction)();
		if (n.type == NodeType.ErrorNode)
		{
			auto err = n.shouldBeOfType!(ErrorNode);
			throw new UnitTestException([generateErrorMessage(err,r,1)],file,line);
		}
		auto errs = n.collectErrors();
		if (errs.length > 0)
			throw new UnitTestException([generateErrorMessage(errs[0],r,1)],file,line);
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}
	alias parseModule = parseNode!("parseModule",ModuleNode);
}
enum Attribute
{
	In = 1,
	Yield = 1 << 1,
	Return = 1 << 2,
	Default = 1 << 3
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
private bool has(alias a)(int attribute)
{
	return (attribute & a) == a;
}
@("Attribute")
unittest
{
	int a = Attribute.In | Attribute.Yield | Attribute.Return | Attribute.Default;
	a.has!(Attribute.In).shouldBeTrue;
	a.has!(Attribute.Yield).shouldBeTrue;
	a.has!(Attribute.Return).shouldBeTrue;
	a.has!(Attribute.Default).shouldBeTrue;
	orAttributes!(Attribute.In,Attribute.Yield).shouldEqual(Attribute.In | Attribute.Yield);
	a.mask!(Attribute.In).has!(Attribute.In).shouldBeTrue;
	a.mask!(Attribute.In).has!(Attribute.Yield).shouldBeFalse;
	a.mask!(Attribute.In,Attribute.Yield).has!(Attribute.Yield).shouldBeTrue;
	a.mask!(Attribute.In,Attribute.Yield).has!(Attribute.Return).shouldBeFalse;
	a.mask!(Attribute.In,Attribute.Yield,Attribute.Return).has!(Attribute.Return).shouldBeTrue;
}
final class Parser(Source) : Lexer!(Source)
{
	this(Source s)
	{
		super(s);
	}
	Node error(string message, in size_t at = __LINE__)
	{
		string m = message;
		string debugMessage;
		version(unittest)
		{
			debugMessage = format("in parser.d @ %s",at);
		}
		return new ErrorNode(message,line,column,debugMessage);
	}
	Node parseModule()
	{
		Node[] children;
		while(token.type != Type.EndOfFile)
		{
			switch(token.type)
			{
				case Type.Identifier:
					if (token.match == "import")
						children ~= parseImportDeclaration();
					else if (token.match == "export")
						children ~= parseExportDeclaration();
					else
						goto default;
					break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					scanAndSkipCommentsAndTerminators();
					break;
				default:
					children ~= parseStatementListItem();
					break;
			}
		}
		return new ModuleNode(children);
	}
	Node parseImportDeclaration()
	{
		assert(token.type == Type.Identifier && token.match == "import");
		scanAndSkipCommentsAndTerminators();
		Node decl;
		if (token.type == Type.StringLiteral)
		{
			decl = new ImportDeclarationNode(new StringLiteralNode(token.match));
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
						clause = new ImportClauseNode(clause,parseNameSpaceImport());
					else
						clause = new ImportClauseNode(clause,parseNamedImports());
				}
			} else
				return error(format("Unexpected %s as part of ImportDeclaration",token.type));

			if (token.type != Type.Identifier && token.match != "from")
				return error("Expected from as part of ImportDeclaration");
			
			scanAndSkipCommentsAndTerminators();
			if (token.type != Type.StringLiteral)
				return error("Expected StringLiteral as part of ImportDeclaration");
			
			decl = new ImportDeclarationNode(clause,new StringLiteralNode(token.match));
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
		assert(token.type == Type.Multiply);
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.Identifier || token.match != "as")
			return error("Expected NameSpaceImport");
		scanAndSkipCommentsAndTerminators();
		return new NameSpaceImportNode(parseIdentifier());
	}
	Node parseNamedImports()
	{
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		Node[] imports;
		while (1)
		{
			auto name = parseIdentifierName();
			if (token.type == Type.Identifier && token.match == "as")
			{	
				scanAndSkipCommentsAndTerminators();
				imports ~= new ImportSpecifierNode(name,parseIdentifier());
			} else
			{
				auto iden = cast(IdentifierNode)name;
				assert(iden !is null);
				if (isIdentifierReservedKeyword(iden))
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
		return new NamedImportsNode(imports);
	}
	Node parseExportDeclaration()
	{
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
					auto specifier = new StringLiteralNode(token.match);
					scanAndSkipCommentsAndTerminators();
					decl = new ExportDeclarationNode(clause,specifier);
				} else
					decl = new ExportDeclarationNode(clause);
			}
			else 
			{
				scanAndSkipCommentsAndTerminators();
				if (token.match != "from")
					return error("Expected from as part of ExportDeclaration");
				scanAndSkipCommentsAndTerminators();
				if (token.type != Type.StringLiteral)
					return error("Expected StringLiteral as part of ExportDeclaration");
				auto specifier = new StringLiteralNode(token.match);
				scanAndSkipCommentsAndTerminators();
				decl = new ExportDeclarationNode(new ExpressionOperatorNode(ExpressionOperator.Multiply),specifier);
			}
		} else if (token.type == Type.Identifier && token.match == "default")
		{
			scanAndSkipCommentsAndTerminators();
			switch(token.match)
			{
				case "function":
					decl = new ExportDefaultDeclarationNode(parseFunctionDeclaration(Attribute.Default));
					break;
				case "class":
					decl = new ExportDefaultDeclarationNode(parseClassDeclaration(Attribute.Default));
					break;
				default:
					decl = new ExportDefaultDeclarationNode(parseAssignmentExpression(Attribute.In));
					break;
			}
		} else if (token.match == "var")
		{
			decl = new ExportDeclarationNode(parseVariableStatement());
		} else if (token.match == "function")
			decl = new ExportDeclarationNode(parseFunctionDeclaration(Attribute.Default));
		else if (token.match == "class")
			decl = new ExportDeclarationNode(parseClassDeclaration(Attribute.Default));
		else if (token.match == "let" || token.match == "const")
			decl = new ExportDeclarationNode(parseLexicalDeclaration(Attribute.In));
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
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
			return new ExportClauseNode([]);
		Node[] children;
		while(1)
		{
			auto name = parseIdentifierName();
			if (token.match == "as")
			{
				scanAndSkipCommentsAndTerminators();
				auto as = parseIdentifierName();
				children ~= new ExportSpecifierNode(name,as);
			} else
				children ~= name;
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing brace as part of ExportClause");
		scanAndSkipCommentsAndTerminators();
		return new ExportClauseNode(children);
    }
	Node parseFunctionExpression(int attributes = 0)
	{
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
		scanToken();
		auto params = parseFormalParameterList();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of function declaration");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody(Attribute.Return);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing brace");
		scanToken();
		if (generator)
			return new GeneratorExpressionNode(name,params,funcBody);
		return new FunctionExpressionNode(name,params,funcBody);
	}
	Node parseClassExpression(int attributes = 0)
	{
		return parseClassDeclaration(attributes);
	}
	Node parseArrayLiteral(int attributes = 0)
	{
		assert(token.type == Type.OpenSquareBrackets);
		Node[] children;
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.Comma)
		{
			children ~= parseElision();
		}
		if (token.type == Type.CloseSquareBrackets)
		{
			scanAndSkipCommentsAndTerminators();
			return new ArrayLiteralNode(children);
		}

		while (1)
		{
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children ~= new SpreadElementNode(parseAssignmentExpression(Attribute.In | attributes));
			} else
				children ~= parseAssignmentExpression(Attribute.In | attributes);
			if (token.type != Type.Comma)
				break;

			scanAndSkipCommentsAndTerminators();
			if (token.type == Type.Comma)
			{
				children ~= parseElision();
				skipCommentsAndLineTerminators();
			}
		}

		if (token.type != Type.CloseSquareBrackets)
			return error("Unexpected eof in ArrayLiteral");

		scanAndSkipCommentsAndTerminators();
		return new ArrayLiteralNode(children);
	}
	Node parseElision()
	{
		assert(token.type == Type.Comma);
		int cnt = 0;
		while(token.type == Type.Comma)
		{
			cnt++;
			scanAndSkipCommentsAndTerminators();
		}
		return new ElisionNode(cnt);
	}
	Node parseObjectLiteral(int attributes = 0)
	{
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
		{
			scanAndSkipCommentsAndTerminators();
			return new ObjectLiteralNode([]);
		}
		Node[] children;
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
					if (token.type != Type.Colon)
						return error("Expected colon as part of PropertyDefinition");
					scanAndSkipCommentsAndTerminators();
					auto expr = parseAssignmentExpression(Attribute.In | attributes);
					children ~= new PropertyDefinitionNode(name,expr);
					break;
				case Type.Identifier:
					switch (token.match)
					{
						case "set":
							if (lookAheadForAny!(Type.Comma,Type.CloseCurlyBrace,Type.Colon))
								goto default;
							children ~= parseClassSetter(staticAttr,attributes.mask!(Attribute.Yield));
							break;
						case "get":
							if (lookAheadForAny!(Type.Comma,Type.CloseCurlyBrace,Type.Colon))
								goto default;
							children ~= parseClassGetter(staticAttr,attributes.mask!(Attribute.Yield));
							break;
						default:
							auto name = parsePropertyName(attributes);
							skipCommentsAndLineTerminators();
							if (token.type == Type.Colon)
							{
								scanAndSkipCommentsAndTerminators();
								auto expr = parseAssignmentExpression(Attribute.In | attributes);
								children ~= new PropertyDefinitionNode(name,expr);
								break;
							}
							if (token.type == Type.OpenParenthesis)
							{
								children ~= parseClassMethod(staticAttr,attributes.mask!(Attribute.Yield),name);
								break;
							}
							auto iden = cast(IdentifierNode)name;
							assert(iden !is null);
							if (isIdentifierReservedKeyword(iden))
								return error(format("Unexpected keyword %s",iden.identifier));
							if (token.type == Type.Assignment)
							{
								scanAndSkipCommentsAndTerminators();
								auto init = parseAssignmentExpression(Attribute.In | attributes);
								children ~= new CoverInitializedName(iden,init);
								break;
							}
							children ~= iden;
							break;
					}
					break;
				case Type.Multiply:
					children ~= parseClassGeneratorMethod(staticAttr,attributes.mask!(Attribute.Yield));
					break;
				default:
					return error("Expected a PropertyDefinition");
			}
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (token.type != Type.CloseCurlyBrace)
			return error(format("Expected closing curly brace before %s",token.type));
		scanAndSkipCommentsAndTerminators();
		return new ObjectLiteralNode(children);
	}
	Node parseTemplateTail(int attributes = 0)
	{
		Node[] children = [new TemplateNode(token.match)];
				scanAndSkipCommentsAndTerminators();

		while (1)
		{
			auto expr = parseExpression(Attribute.In | attributes);
			// TODO: do something with errornode here
			children ~= expr;
			if (token.type == Type.Error)
			{
				children ~= error(token.match);
				return new TemplateLiteralNode(children);
			}
			children ~= new TemplateNode(token.match);
			if (token.type == Type.TemplateTail)
			{
				scanAndSkipCommentsAndTerminators();
				return new TemplateLiteralNode(children);
			}
			scanAndSkipCommentsAndTerminators();
		}
	}
	Node parseParenthesisExpression(int attributes = 0)
	{
		assert(token.type == Type.OpenParenthesis);
		scanToken();
		if (token.type == Type.CloseParenthesis)
		{
			auto node = new ParenthesisNode();
			scanToken();
			return node;
		}
		if (token.type == Type.SpreadOperator)
		{
			scanToken();
			skipCommentsAndLineTerminators();
			auto node = new ParenthesisNode(new SpreadElementNode(parseIdentifier(attributes)));
			skipCommentsAndLineTerminators();
			if (token.type != Type.CloseParenthesis)
				return error("Expected closing parenthesis");
			scanAndSkipCommentsAndTerminators();
			return node;
		}

		attributes = attributes.mask!(Attribute.Yield,Attribute.In);
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
					scanToken();
					return error("Expected Expression before eof");
				}
				if (children.length == 1)
					return new ParenthesisNode(children[0]);
				return new ParenthesisNode(new ExpressionNode(children));
			}
			switch (token.type)
			{
				case Type.Comma:
					if (comma)
						return error("Expected AssignmentExpression instead got comma");
					comma = true;
					scanToken();
					break;
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken();
					break;
				case Type.SpreadOperator:
					scanAndSkipCommentsAndTerminators();
					auto spread = new SpreadElementNode(parseIdentifier(attributes));
					if (token.type != Type.CloseParenthesis)
						return error("Expected closing parenthesis");
					scanAndSkipCommentsAndTerminators();
					return new ParenthesisNode([new ExpressionNode(children),spread]);
				case Type.CloseParenthesis:
					scanAndSkipCommentsAndTerminators();
					return new ParenthesisNode(new ExpressionNode(children));
				default:
					comma = false;
					auto expr = parseAssignmentExpression(Attribute.In | attributes);
					children ~= expr;
					break;
			}
		}
	}
	Node parsePrimaryExpression(int attributes = 0)
	{
		attributes = attributes.mask!(Attribute.Yield);
		switch (token.type)
		{
			case Type.Identifier:
				switch(token.match)
				{
					case "this": scanToken(); return new KeywordNode(Keyword.This);
					case "null": scanToken(); return new KeywordNode(Keyword.Null);
					case "true": scanToken(); return new BooleanNode(true);
					case "false": scanToken(); return new BooleanNode(false);
					case "function": return parseFunctionExpression();
					case "class": return parseClassExpression(attributes);
					default:
						auto node = parseIdentifier(attributes);
						return node;
				}
			case Type.StringLiteral: auto node = new StringLiteralNode(token.match); scanToken(); return node;
			case Type.BinaryLiteral: auto node = new BinaryLiteralNode(token.match); scanToken(); return node;
			case Type.OctalLiteral: auto node = new OctalLiteralNode(token.match); scanToken(); return node;
			case Type.DecimalLiteral: auto node = new DecimalLiteralNode(token.match); scanToken(); return node;
			case Type.HexLiteral: auto node = new HexLiteralNode(token.match); scanToken(); return node;
			case Type.OpenSquareBrackets: return parseArrayLiteral(attributes);
			case Type.OpenCurlyBrace: return parseObjectLiteral(attributes);
			case Type.TemplateHead: return parseTemplateTail(attributes.mask!(Attribute.Yield));
			case Type.Template: //todo there is also a possible yield attribute here
				auto node = new TemplateLiteralNode(new TemplateNode(token.match)); scanToken(); return node;
			case Type.Regex: auto node = new RegexLiteralNode(token.match); scanToken(); return node;
			case Type.OpenParenthesis: return parseParenthesisExpression(attributes);
			default:
				auto node = error(format("unexpected %s token (%s)",token.type,token.match));
				// TODO: resync primary expression
				scanToken();
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
			token.type == Type.Colon ||
			(token.type == Type.Identifier && (token.match == "in" || token.match == "of")));
	}
	Node parseExpression(int attributes = 0)
	{
		attributes = attributes.mask!(Attribute.Yield,Attribute.In);
		bool comma = false;
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
				return new ExpressionNode(children);
			}
			switch (token.type)
			{
				case Type.Comma:
					if (comma)
						return error("Expected AssignmentExpression instead got comma");
					comma = true;
					scanToken();
					break;
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken();
					break;
				default:
					comma = false;
					auto expr = parseAssignmentExpression(attributes);
					children ~= expr;
					break;
			}
		}
	}
	Node parseArrowFunctionBody(int attributes = 0)
	{
		if (token.type == Type.OpenCurlyBrace)
		{
			scanToken();
			auto funcBody = parseFunctionBody();
			if (token.type != Type.CloseCurlyBrace)
				return error("Expected closing brace");
			scanAndSkipCommentsAndTerminators();
			return funcBody;
		}
		return parseAssignmentExpression(attributes);
	}
	Node parseAssignmentExpression(int attributes = 0)
	{
		Node cond = parseConditionalExpression(attributes);
		// todo what if cond in an errornode
		if (cond.type == NodeType.ConditionalExpressionNode)
			return cond;
		skipCommentsAndLineTerminators();
		if (cond.type == NodeType.IdentifierNode && token.type == Type.Arrow)
		{
			scanAndSkipCommentsAndTerminators();
			return new ArrowFunctionNode(cond,parseArrowFunctionBody(attributes.mask!(Attribute.In)));
		}
		skipCommentsAndLineTerminators();
		if (cond.type == NodeType.ParenthesisNode && token.type == Type.Arrow)
		{
			scanAndSkipCommentsAndTerminators();
			return new ArrowFunctionNode(cond,parseArrowFunctionBody(attributes.mask!(Attribute.In)));
		}
		if (cond.type == NodeType.UnaryExpressionNode || cond.type == NodeType.BinaryExpressionNode)
			return cond;

		if (empty)
			return cond;
		Node[] children = [cond];
		while (!empty)
		{
			switch (token.type)
			{
				case Type.LeftShiftAssignment: 			scanToken(); children ~= new AssignmentOperatorNode(Assignment.LeftShiftAssignment); break;
				case Type.TripleRightShiftAssignment: 	scanToken(); children ~= new AssignmentOperatorNode(Assignment.TripleRightShiftAssignment); break;
				case Type.RightShiftAssignment: 		scanToken(); children ~= new AssignmentOperatorNode(Assignment.RightShiftAssignment); break;
				case Type.Assignment: 					scanToken(); children ~= new AssignmentOperatorNode(Assignment.Assignment); break;
				case Type.AdditiveAssignment: 			scanToken(); children ~= new AssignmentOperatorNode(Assignment.AdditiveAssignment); break;
				case Type.DecrementalAssignment: 		scanToken(); children ~= new AssignmentOperatorNode(Assignment.DecrementalAssignment); break;
				case Type.MultiplicativeAssignment: 	scanToken(); children ~= new AssignmentOperatorNode(Assignment.MultiplicativeAssignment); break;
				case Type.DivisionAssignment: 			scanToken(); children ~= new AssignmentOperatorNode(Assignment.DivisionAssignment); break;
				case Type.ModAssignment: 				scanToken(); children ~= new AssignmentOperatorNode(Assignment.ModAssignment); break;
				case Type.BitwiseAndAssignment: 		scanToken(); children ~= new AssignmentOperatorNode(Assignment.BitwiseAndAssignment); break;
				case Type.BitwiseOrAssignment: 			scanToken(); children ~= new AssignmentOperatorNode(Assignment.BitwiseOrAssignment); break;
				case Type.BitwiseXorAssignment: 		scanToken(); children ~= new AssignmentOperatorNode(Assignment.BitwiseXorAssignment); break;
				default:
					if (children.length == 1)
						return children[0];
					return new AssignmentExpressionNode(children);
			}
			cond = parseConditionalExpression(attributes);
			if (cond.type == NodeType.ConditionalExpressionNode)
			{
				children ~= cond;
				return new AssignmentExpressionNode(children);
			}
			if (cond.type == NodeType.IdentifierNode && token.type == Type.Arrow)
			{
				scanToken();
				children ~= new ArrowFunctionNode(cond,parseArrowFunctionBody(attributes.mask!(Attribute.In)));
				return new AssignmentExpressionNode(children);
			}
			// TODO this idea in the if below is correct, but there might be Comments between the parenthesis and the arrow (which are allowed, as long as the arrow is on the same line as the closing parenthesis)
			if (cond.type == NodeType.ParenthesisNode && token.type == Type.Arrow)
			{
				scanToken();
				children ~= new ArrowFunctionNode(cond,parseArrowFunctionBody(attributes.mask!(Attribute.In)));
				return new AssignmentExpressionNode(children);
			}
			children ~= cond;
			if (cond.type == NodeType.UnaryExpressionNode || cond.type == NodeType.BinaryExpressionNode)
				return new AssignmentExpressionNode(children);
		}
		return new AssignmentExpressionNode(children);
	}
	Node parseConditionalExpression(int attributes = 0)
	{
		auto rhs = parseRightHandSideExpression(attributes);
		if (token.type != Type.QuestionMark)
			return rhs;
		scanToken();
		if (empty)
			return error(format("Expected AssignmentExpression as part of an ConditionalExpression before eof"));
		auto yeah = parseAssignmentExpression(Attribute.In | attributes);
		if (token.type != Type.Colon)
			return error(format("Expected colon as part of ConditionalExpression, instead got %s token %s",token.type,token.match));
		scanToken();
		if (empty)
			return error(format("Expected AssignmentExpression as part of an ConditionalExpression before eof"));
		auto nay = parseAssignmentExpression(attributes);
		return new ConditionalExpressionNode(rhs,yeah,nay);
	}
	Node parseRightHandSideExpression(int attributes = 0)
	{
		Node[] children;
		while (1)
		{
			children ~= parseUnaryExpression(attributes.mask!(Attribute.Yield));
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.match)
					{
						case "instanceof":
							children ~= new ExpressionOperatorNode(ExpressionOperator.InstanceOf);
							scanToken();
							break;
						case "in":
							if (!attributes.has!(Attribute.In))
							{
								if (children.length == 1)
									return children[0];
								return new BinaryExpressionNode(children);
							}
							children ~= new ExpressionOperatorNode(ExpressionOperator.In);
							scanToken();
							break;
						default:
							if (children.length == 1)
								return children[0];
							return new BinaryExpressionNode(children);
					}
					break;
				case Type.LogicalAnd: 		children ~= new ExpressionOperatorNode(ExpressionOperator.LogicalAnd); scanToken(); break;
				case Type.LogicalOr: 		children ~= new ExpressionOperatorNode(ExpressionOperator.LogicalOr); scanToken(); break;
				case Type.BitwiseAnd: 		children ~= new ExpressionOperatorNode(ExpressionOperator.BitwiseAnd); scanToken(); break;
				case Type.BitwiseOr: 		children ~= new ExpressionOperatorNode(ExpressionOperator.BitwiseOr); scanToken(); break;
				case Type.BitwiseXor: 		children ~= new ExpressionOperatorNode(ExpressionOperator.BitwiseXor); scanToken(); break;
				case Type.StrictEqual: 		children ~= new ExpressionOperatorNode(ExpressionOperator.StrictEqual); scanToken(); break;
				case Type.Equal: 			children ~= new ExpressionOperatorNode(ExpressionOperator.Equal); scanToken(); break;
				case Type.StrictNotEqual: 	children ~= new ExpressionOperatorNode(ExpressionOperator.StrictNotEqual); scanToken(); break;
				case Type.NotEqual: 		children ~= new ExpressionOperatorNode(ExpressionOperator.NotEqual); scanToken(); break;
				case Type.LessOrEqual: 		children ~= new ExpressionOperatorNode(ExpressionOperator.LessOrEqual); scanToken(); break;
				case Type.LessThan: 		children ~= new ExpressionOperatorNode(ExpressionOperator.LessThan); scanToken(); break;
				case Type.GreaterOrEqual: 	children ~= new ExpressionOperatorNode(ExpressionOperator.GreaterOrEqual); scanToken(); break;
				case Type.GreaterThan: 		children ~= new ExpressionOperatorNode(ExpressionOperator.GreaterThan); scanToken(); break;
				case Type.LeftShift: 		children ~= new ExpressionOperatorNode(ExpressionOperator.LeftShift); scanToken(); break;
				case Type.TripleRightSift: 	children ~= new ExpressionOperatorNode(ExpressionOperator.TripleRightSift); scanToken(); break;
				case Type.RightShift: 		children ~= new ExpressionOperatorNode(ExpressionOperator.RightShift); scanToken(); break;
				case Type.Add:		 		children ~= new ExpressionOperatorNode(ExpressionOperator.Add); scanToken(); break;
				case Type.Minus: 			children ~= new ExpressionOperatorNode(ExpressionOperator.Minus); scanToken(); break;
				case Type.Multiply: 		children ~= new ExpressionOperatorNode(ExpressionOperator.Multiply); scanToken(); break;
				case Type.Division: 		children ~= new ExpressionOperatorNode(ExpressionOperator.Division); scanToken(); break;
				case Type.Mod: 				children ~= new ExpressionOperatorNode(ExpressionOperator.Mod); scanToken(); break;
				default:
					if (children.length == 1)
						return children[0];
					return new BinaryExpressionNode(children);
			}
		}
	}
	Node parseUnaryExpression(int attributes = 0)
	{
		Node[] prefixExprs;
		while(token.type != Type.EndOfFile)
		{
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.match)
					{
						case "delete": prefixExprs ~= new PrefixExpressionNode(Prefix.Delete);
							scanToken();
							break;
						case "void": prefixExprs ~= new PrefixExpressionNode(Prefix.Void);
							scanToken();
							break;
						case "typeof": prefixExprs ~= new PrefixExpressionNode(Prefix.Typeof);
							scanToken();
							break;
						default:
							goto doLHS;
					}
					break;
				case Type.Increment:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Increment);
					scanToken();
					break;
				case Type.Decrement:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Decrement);
					scanToken();
					break;
				case Type.Add:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Positive);
					scanToken();
					break;
				case Type.Minus:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Negative);
					scanToken();
					break;
				case Type.Tilde:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Tilde);
					scanToken();
					break;
				case Type.Negation:
					prefixExprs ~= new PrefixExpressionNode(Prefix.Negation);
					scanToken();
					break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					scanToken();
					break;
				default:
					goto doLHS;
			}
		}
		return error("Found end of file before parsing UnaryExpression");
		doLHS:
		auto lhsexpr = parseLeftHandSideExpression(attributes);
		if (token.type == Type.Increment)
		{
			auto node = new UnaryExpressionNode(prefixExprs,lhsexpr);
			node.postfix = Postfix.Increment;
			scanToken();
			return node;
		} else if (token.type == Type.Decrement)
		{
			auto node = new UnaryExpressionNode(prefixExprs,lhsexpr);
			node.postfix = Postfix.Decrement;
			scanToken();
			return node;
		} else if (prefixExprs.length > 0)
			return new UnaryExpressionNode(prefixExprs,lhsexpr);
		return lhsexpr;
	}
	Node parseAccessor(int attributes = 0)
	{
		assert(token.type == Type.Dot);
		scanToken();

		while (1)
		{
			switch (token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					scanToken();
					break;
				case Type.Identifier:
					auto node = new AccessorNode(token.match);
					scanAndSkipCommentsAndTerminators();
					return node;
				default:
					auto node = error(format("Expected identifer got %s",token.type));
					scanToken();
					return node;
			}
		}
	}
	Node parseLeftHandSideExpression(int attributes = 0)
	{
		size_t news = 0, args = 0;
		Node content;
		Node[] calls = [content];
		while (1)
		{
			switch (token.type)
			{
				case Type.Identifier:
					switch (token.match)
					{
						case "super":
							content = parseSuperProperty(attributes);
							break;
						case "new":
							news++;
							scanToken();
							break;
						default:
							if (content !is null)
								return content;
							content = parsePrimaryExpression(attributes);
							break;
					}
					break;
				case Type.Dot:
					if (content !is null)
					{
						args++;
						calls ~= parseAccessor();
						break;
					}
					if (news == 0)
						return error("Invalid dot in LeftHandSideExpression");
					scanToken();
					if (token.type != Type.Identifier || token.match != "target")
						return error("The only valid meta property for new is new.target");
					content = new NewTargetNode();
					scanToken();
					break;
				case Type.Template:
					if (content is null)
						goto default;
					args++;
					calls ~= new TemplateLiteralNode(new TemplateNode(token.match));
					scanToken();
					break;
				case Type.TemplateHead:
					if (content is null)
						goto default;
					args++;
					calls ~= parseTemplateTail(attributes);
					break;
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					scanToken();
					break;
				case Type.OpenParenthesis:
					if (content is null)
						goto default;
					args++;
					calls ~= parseArguments(attributes);
					break;
				case Type.OpenSquareBrackets:
					if (content is null)
						goto default;
					args++;
					calls ~= parseArrayIndexing(Attribute.In | attributes);
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
		end: calls[0] = content;
		if (news > 0 && news > args)
		{
			return new NewExpressionNode(news,calls);
		} else if (args > 0)
		{
			return new CallExpressionNode(news,calls);
		}
		return content;
	}
	bool isIdentifierReservedKeyword(IdentifierNode i)
	{
		return (i.identifier == "break" || i.identifier == "do" || i.identifier == "in" || i.identifier == "typeof" || i.identifier == "case" || i.identifier == "else" ||
			i.identifier == "instanceof" || i.identifier == "var" || i.identifier == "catch" || i.identifier == "export" || i.identifier == "new" ||
			i.identifier == "void" || i.identifier == "class" || i.identifier == "extends" || i.identifier == "return" || i.identifier == "while" || i.identifier == "const" ||
			i.identifier == "finally" || i.identifier == "super" || i.identifier == "with" || i.identifier == "continue" || i.identifier == "for" || i.identifier == "switch" ||
			i.identifier == "yield" || i.identifier == "debugger" || i.identifier == "function" || i.identifier == "this" || i.identifier == "default" || i.identifier == "if" ||
			i.identifier == "throw" || i.identifier == "delete" || i.identifier == "import" || i.identifier == "try" || i.identifier == "enum" || i.identifier == "await" ||
			i.identifier == "null" || i.identifier == "true" || i.identifier == "false");
	}
	Node parseIdentifier(int attributes = 0)
	{
		assert(token.type == Type.Identifier);

		IdentifierNode n = new IdentifierNode(token.match);
		if (token.match == "yield")
		{
			if (!attributes.has!(Attribute.Yield))
				return error("keyword yield cannot be used in this context");

		} else if (isIdentifierReservedKeyword(n))
			return error(format("Invalid IdentifierReference %s",token.match));

		scanAndSkipCommentsAndTerminators();
		return n;
	}
	Node parseIdentifierName()
	{
		assert(token.type == Type.Identifier);
		auto n = new IdentifierNode(token.match);
		scanAndSkipCommentsAndTerminators();
		return n;
	}
	Node parseSuperProperty(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "super");
		scanToken();

		if (token.type == Type.Dot)
		{
			scanToken();
			if (token.type != Type.Identifier)
				return error("Expected Identifier after .");
			return new SuperPropertyNode(parseIdentifierName());
		} else if (token.type == Type.OpenSquareBrackets)
		{
			return new SuperPropertyNode(parseArrayIndexing(Attribute.In | attributes));
		}
		return error("Expected . or array index after super keyword");
	}
	Node parseArguments(int attributes = 0)
	{
		assert(token.type == Type.OpenParenthesis);
		scanToken();

		Node[] args;
		if (token.type == Type.Comma)
			return error("Expected argument before comma");
		while (1)
		{
			if (token.type == Type.MultiLineComment ||
				token.type == Type.SingleLineComment ||
				token.type == Type.LineTerminator)
			{
				scanToken();
				continue;
			}
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				args ~= new SpreadElementNode(parseAssignmentExpression(Attribute.In | attributes));
				// TODO what if parse fails ?
				if (token.type != Type.CloseParenthesis)
					return error("Expected closing parenthesis");
				scanAndSkipCommentsAndTerminators();
				return new ArgumentsNode(args);
			}
			if (token.type == Type.CloseParenthesis)
			{
				auto node = new ArgumentsNode(args);
				scanToken();
				return node;
			}
			if (token.type == Type.Comma)
			{
				scanToken();
				args ~= parseAssignmentExpression(Attribute.In | attributes);
				// TODO what if parse fails ?
			} else {
				if (args.length != 0)
				{
					return error("Expected Comma or CloseParenthesis");
				}
				args ~= parseAssignmentExpression(Attribute.In | attributes);
				// TODO what if parse fails ?
			}
		}
	}
	Node parseArrayIndexing(int attributes = 0)
	{
		assert(token.type == Type.OpenSquareBrackets);
		scanAndSkipCommentsAndTerminators();

		auto expr = parseExpression(attributes);
		if (token.type != Type.CloseSquareBrackets)
			return error("Expected closing square bracket");

		scanAndSkipCommentsAndTerminators();
		return new ArrayIndexNode(expr);
	}
	Node parseStatement(int attributes = 0)
	{
		Node node;
		switch (token.type)
		{
			case Type.OpenCurlyBrace:
				return parseBlockStatement(attributes);
			case Type.Identifier:
				switch (token.match)
				{
					case "var": node = parseVariableStatement(attributes.mask!(Attribute.Yield)); break;
					case "if": node = parseIfStatement(attributes); break;
					case "switch": node = parseSwitchStatement(attributes); break;
					case "do": node = parseDoWhileStatement(attributes); break;
					case "while": node = parseWhileStatement(attributes); break;
					case "for": node = parseForStatement(attributes); break;
					case "continue": scanToken(); node = new ContinueStatementNode(); break;
					case "break": scanToken(); node = new BreakStatementNode(); break;
					case "with": node = parseWithStatement(attributes); break;
					case "throw": node = parseThrowStatement(attributes); break;
					case "try": node = parseTryStatement(attributes); break;
					case "debugger": node = parseDebuggerStatement(); break;
					case "return":
						if (!attributes.has!(Attribute.Return))
						{
							scanAndSkipCommentsAndTerminators();
							return error("return keyword not allowed in this context");
						}
						node = parseReturnStatement(attributes.mask!(Attribute.Yield));
						break;
					default: node = parseExpression(Attribute.In | attributes.mask!(Attribute.Yield,Attribute.Return)); break;
				}
				break;
			case Type.Semicolon:
				scanAndSkipCommentsAndTerminators();
				return new EmptyStatementNode();
			default: node = parseExpression(Attribute.In | attributes.mask!(Attribute.Yield,Attribute.Return)); break;
		}
		if (token.type == Type.Semicolon)
		{
			scanToken();
			return node;
		}
		while (!isEndOfExpression || (node.type == NodeType.IdentifierNode && token.type == Type.Colon))
		{
			switch (token.type)
			{
				case Type.MultiLineComment:
				case Type.SingleLineComment:
				case Type.LineTerminator:
					scanToken();
					break;
				case Type.Colon:
					if (node.type == NodeType.IdentifierNode)
					{
						auto idNode = cast(IdentifierNode)node;
						assert(idNode !is null);
						scanToken();
						return new LabelledStatementNode(idNode.identifier);
					}
					return error("Unexpected colon in Statement");
				case Type.Semicolon:
					scanToken();
					return node;
				default: return node;
			}
		}
		return node;
	}
	Node parseReturnStatement(int attributes = 0)
	{
		assert(token.match == "return");
		scanAndSkipCommentsAndTerminators();
		if (isEndOfExpression)
			return new ReturnStatementNode();
		auto expr = parseExpression(Attribute.In | attributes);
		return new ReturnStatementNode(expr);
	}
	Node parseBlockStatement(int attributes = 0)
	{
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		Node[] children = parseStatementList(attributes);
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");
		scanAndSkipCommentsAndTerminators();
		return new BlockStatementNode(children);
	}
	Node[] parseStatementList(int attributes = 0)
	{
		Node[] children;
		while(!isEndOfExpression || token.type == Type.Semicolon)
		{
			if (token.type == Type.Identifier && (token.match == "case" || token.match == "default"))
				return children;
			switch(token.type)
			{
				case Type.LineTerminator:
				case Type.SingleLineComment:
				case Type.MultiLineComment:
					scanAndSkipCommentsAndTerminators();
					continue;
				default:
					children ~= parseStatementListItem(attributes);
					break;
			}
		}
		return children;
	}
	Node parseStatementListItem(int attributes = 0)
	{
		Node node;
		if (token.type == Type.Identifier)
		{
			switch(token.match)
			{
				case "class":
					node = parseClassDeclaration(attributes.mask!(Attribute.Yield));
					break;
				case "function":
					node = parseFunctionDeclaration(attributes.mask!(Attribute.Yield));
					break;
				case "let":
				case "const":
					node = parseLexicalDeclaration(Attribute.In | attributes.mask!(Attribute.Yield));
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
	void skipCommentsAndLineTerminators()
	{
		while(token.type == Type.MultiLineComment || token.type == Type.SingleLineComment || token.type == Type.LineTerminator)
			scanToken();
	}
	void scanAndSkipCommentsAndTerminators(in string file = __FILE__, in size_t line = __LINE__)
	{
		scanToken(file,line);
		skipCommentsAndLineTerminators();
	}
	Node parseVariableStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "var");
		scanAndSkipCommentsAndTerminators();
		return parseVariableDeclarationList(Attribute.In | attributes);
	}
	Node parseVariableDeclarationList(int attributes = 0)
	{
		Node[] children;
		while (!isEndOfExpression)
		{
			Node init = null;
			Node lhs = null;
			switch (token.type)
			{
				case Type.OpenSquareBrackets: lhs = parseArrayBindingPattern(attributes.mask!(Attribute.Yield)); break;
				case Type.OpenCurlyBrace: lhs = parseObjectBindingPattern(attributes.mask!(Attribute.Yield)); break;
				case Type.Identifier: lhs = parseIdentifier(attributes.mask!(Attribute.Yield)); break;
				default:
					return error(format("Expected ArrayBindingPattern, ObjectBindingPattern or Identifier, instead got %s",token.type));
			}
			skipCommentsAndLineTerminators();
			if (token.type == Type.Assignment)
			{
				scanAndSkipCommentsAndTerminators();
				init = parseAssignmentExpression(attributes.mask!(Attribute.In,Attribute.Yield));
			}
			children ~= new VariableDeclarationNode(lhs,init);
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		if (children.length == 0)
			return error("Expected variable declarations");
		return new VariableStatementNode(children);
	}
	Node parseArrayBindingPattern(int attributes = 0)
	{
		assert(token.type == Type.OpenSquareBrackets);
		scanAndSkipCommentsAndTerminators();
		Node[] children;
		if (token.type == Type.Comma)
		{
			children ~= parseElision();
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children ~= new RestElementNode(parseIdentifier(attributes));
				if (token.type != Type.CloseSquareBrackets)
					return error("Expected closing brace");
				scanAndSkipCommentsAndTerminators();
				return new ArrayBindingPatternNode(children);
			}
		}
		while (!isEndOfExpression)
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
				children ~= new RestElementNode(parseIdentifier(attributes));
				break;
			}
		}
		if (token.type != Type.CloseSquareBrackets)
			return error("Expected closing brace");
		scanAndSkipCommentsAndTerminators();
		return new ArrayBindingPatternNode(children);
	}
	Node parseObjectBindingPattern(int attributes = 0)
	{
		assert(token.type == Type.OpenCurlyBrace);
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.CloseCurlyBrace)
		{
			scanAndSkipCommentsAndTerminators();
			return new ObjectBindingPatternNode([]);
		}
		Node[] children;
		while(!isEndOfExpression)
		{
			auto name = parsePropertyName(attributes);
			if (token.type == Type.Colon)
			{
				scanAndSkipCommentsAndTerminators();
				auto elem = parseBindingElement(attributes);
				children ~= new BindingPropertyNode(name,elem);
			} else
			{
				auto iden = cast(IdentifierNode)name;
				if (iden is null)
					return error(format("Expected identifier, got %s",name));
				if (isIdentifierReservedKeyword(iden))
					return error(format("Invalid IdentifierReference %s",iden.identifier));
				if (token.type != Type.Assignment)
					children ~= iden;
				else
				{
					scanAndSkipCommentsAndTerminators();
					auto init = parseAssignmentExpression(Attribute.In | attributes);
					children ~= new SingleNameBindingNode(iden,init);
				}
			}
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}

		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing curly brace");

		scanAndSkipCommentsAndTerminators();
		return new ObjectBindingPatternNode(children);
	}
	Node parseIfStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "if");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of IfStatement");
		scanAndSkipCommentsAndTerminators();
		Node cond = parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of IfStatement");
		scanAndSkipCommentsAndTerminators();
		Node truth = parseStatement(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.Identifier || token.match != "else")
			return new IfStatementNode(cond,truth);
		scanAndSkipCommentsAndTerminators();
		Node elsePath = parseStatement(attributes);
		return new IfStatementNode(cond,truth,elsePath);
	}
	Node parseSwitchStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "switch");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of SwitchStatement");
		scanToken();
		Node[] children = [parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of SwitchStatement");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening curly brace as part of SwitchStatement");
		scanToken();
		while(1)
		{
			switch(token.type)
			{
				case Type.Identifier:
					switch(token.match)
					{
						case "case":
							scanToken();
							Node[] caseChildren = [parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
							if (token.type != Type.Colon)
								return error("Expected colon");
							scanAndSkipCommentsAndTerminators();
							// parseStatemenList should hold on "case" identifier
							caseChildren ~= parseStatementList(attributes);
							children ~= new CaseNode(caseChildren);
							break;
						case "default":
							Node[] caseChildren = [];
							scanAndSkipCommentsAndTerminators();
							if (token.type != Type.Colon)
								return error("Expected colon");
							scanAndSkipCommentsAndTerminators();
							caseChildren ~= parseStatementList(attributes);
							children ~= new DefaultNode(caseChildren);
							break;
						default:
							scanToken();
							return error("Expected case keyword");
					}
					break;
				case Type.SingleLineComment:
				case Type.MultiLineComment:
				case Type.LineTerminator:
					scanToken();
					break;
				case Type.CloseCurlyBrace:
					scanToken();
					return new SwitchStatementNode(children);
				default:
					return error(format("Unexpected token %s as part of SwitchStatement",token.type));
			}
		}
	}
	Node parseDoWhileStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "do");
		scanAndSkipCommentsAndTerminators();
		Node[] children = [parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return))];
		if (token.type != Type.Identifier || token.match != "while")
			return error("Expected while keyword");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of DoWhileStatement");
		scanAndSkipCommentsAndTerminators();
		children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of DoWhileStatement");
		scanAndSkipCommentsAndTerminators();
		if (token.type == Type.Semicolon)
			scanAndSkipCommentsAndTerminators();
		return new DoWhileStatementNode(children);
	}
	Node parseWhileStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "while");
		// todo eat lineterminators/comments
		scanToken();
		if (token.type != Type.OpenParenthesis)
			return error("expected parenthesis as part of DoWhileStatement");
		scanToken();
		Node[] children = [parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of DoWhileStatement");

		scanToken();
		children ~= parseStatement(attributes);

		if (token.type == Type.Semicolon)
			scanToken();
		return new WhileStatementNode(children);
	}
	Node parseForStatement(int attributes = 0)
	{
		Node parseOldSchoolForStatement(Node firstExpr, int attributes = 0)
		{
			Node[] children;
			if (firstExpr !is null)
				children ~= firstExpr;
			if (token.type != Type.Semicolon)
				return error(format("Expected semicolon, got %s",token.type));
			scanAndSkipCommentsAndTerminators();
			children ~= new SemicolonNode();
			if (token.type != Type.Semicolon)
			{
				children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
			}
			if (token.type != Type.Semicolon)
				return error(format("Expected semicolon, got %s",token.type));
			scanAndSkipCommentsAndTerminators();
			children ~= new SemicolonNode();
			if (token.type != Type.CloseParenthesis)
			{
				children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
				if (token.type != Type.CloseParenthesis)
					return error("Expected closing parenthesis");
			}
			scanAndSkipCommentsAndTerminators();
			children ~= parseStatement(attributes.mask!(Attribute.Return,Attribute.Yield));
			return new ForStatementNode(ForLoop.ExprCStyle,children);
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
					switch (token.match)
					{
						case "var":
							scanAndSkipCommentsAndTerminators();
							auto varStmt = parseVariableDeclarationList(attributes.mask!(Attribute.Yield));
							// todo what abour error
							if (varStmt.children.length == 1 && varStmt.children[0].children.length == 1 && token.type == Type.Identifier)
							{
								if (token.match == "in")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [varStmt.children[0].children[0],parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
									return new ForStatementNode(ForLoop.VarIn,children);
								} else if (token.match == "of")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [varStmt.children[0].children[0],parseAssignmentExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
									return new ForStatementNode(ForLoop.VarOf,children);
								}
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							Node[] children = [varStmt,new SemicolonNode()];
							if (token.type != Type.Semicolon)
							{
								children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							children ~= new SemicolonNode();
							if (token.type != Type.CloseParenthesis)
							{
								children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
								if (token.type != Type.CloseParenthesis)
									return error("Expected closing parenthesis");
							}
							scanAndSkipCommentsAndTerminators();
							children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
							return new ForStatementNode(ForLoop.VarCStyle,children);
						case "let":
						case "const":
							bool constDecl = token.match == "const";
							auto lexDecl = parseLexicalDeclaration(attributes.mask!(Attribute.Yield));
							if (lexDecl.children.length == 1 && lexDecl.children[0].children.length == 1 && token.type == Type.Identifier)
							{
								if (token.match == "in")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [lexDecl.children[0].children[0],parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
									auto loopType = constDecl ? ForLoop.ConstIn : ForLoop.LetIn;
									return new ForStatementNode(loopType,children);
								} else if (token.match == "of")
								{
									scanAndSkipCommentsAndTerminators();
									Node[] children = [lexDecl.children[0].children[0],parseAssignmentExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
									if (token.type != Type.CloseParenthesis)
										return error("Expect closing parenthesis");
									scanAndSkipCommentsAndTerminators();
									children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
									auto loopType = constDecl ? ForLoop.ConstOf : ForLoop.LetOf;
									return new ForStatementNode(loopType,children);
								}
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							Node[] children = [lexDecl,new SemicolonNode()];
							if (token.type != Type.Semicolon)
							{
								children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
							}
							if (token.type != Type.Semicolon)
								return error(format("Expected semicolon, got %s",token.type));
							scanAndSkipCommentsAndTerminators();
							children ~= new SemicolonNode();
							if (token.type != Type.CloseParenthesis)
							{
								children ~= parseExpression(Attribute.In | attributes.mask!(Attribute.Yield));
								if (token.type != Type.CloseParenthesis)
									return error("Expected closing parenthesis");
							}
							scanAndSkipCommentsAndTerminators();
							children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
							auto loopType = constDecl ? ForLoop.ConstCStyle : ForLoop.LetCStyle;
							return new ForStatementNode(loopType,children);
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
					auto expr = parseExpression(attributes.mask!(Attribute.Yield));
					skipCommentsAndLineTerminators();
					if (token.type == Type.Identifier)
					{
						if (expr.isEitherA!(NodeType.UnaryExpressionNode,NodeType.BinaryExpressionNode,NodeType.ConditionalExpressionNode,NodeType.ExpressionNode,NodeType.AssignmentExpressionNode))
							return error(format("Expected LeftHandSideExpression, instead got %s",expr.type));
						if (token.match == "in")
						{
							scanAndSkipCommentsAndTerminators();
							Node[] children = [expr,parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
							//todo eat comments and lineterminators
							if (token.type != Type.CloseParenthesis)
								return error("Expected closing parenthesis");
							scanAndSkipCommentsAndTerminators();
							//todo eat comments and lineterminators
							children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
							return new ForStatementNode(ForLoop.ExprIn,children);
						} else if (token.match == "of")
						{
							scanAndSkipCommentsAndTerminators();
							Node[] children = [expr,parseAssignmentExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
							//todo eat comments and lineterminators
							if (token.type != Type.CloseParenthesis)
								return error("Expected closing parenthesis");
							scanAndSkipCommentsAndTerminators();
							//todo eat comments and lineterminators
							children ~= parseStatement(attributes.mask!(Attribute.Yield,Attribute.Return));
							return new ForStatementNode(ForLoop.ExprOf,children);
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
		assert(token.type == Type.Identifier);
		LexicalDeclaration decl;
		if (token.match == "let")
			decl = LexicalDeclaration.Let;
		else if (token.match == "const")
			decl = LexicalDeclaration.Const;
		else return error("Expected let or const as part of LexicalDeclaration");

		scanAndSkipCommentsAndTerminators();
		Node[] children;
		while (!isEndOfExpression)
		{
			Node init = null;
			Node lhs = null;
			bool requiresAssignment = false;
			switch (token.type)
			{
				case Type.OpenSquareBrackets: requiresAssignment = true; lhs = parseArrayBindingPattern(Attribute.Yield); break;
				case Type.OpenCurlyBrace: requiresAssignment = true; lhs = parseObjectBindingPattern(Attribute.Yield); break;
				case Type.Identifier: lhs = parseIdentifier(attributes.mask!(Attribute.Yield)); break;
				default:
					return error(format("Expected ArrayBindingPattern, ObjectBindingPattern or Identifier, instead got %s",token.type));
			}
			if (token.type == Type.Assignment)
			{
				scanAndSkipCommentsAndTerminators();
				init = parseAssignmentExpression(attributes.mask!(Attribute.In,Attribute.Yield));
			} /*else if (requiresAssignment)
			{
				scanAndSkipCommentsAndTerminators();
				return error("BindingPattern requires initializer");
			}*/
			children ~= new LexicalDeclarationItemNode(lhs,init);
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanToken();
		}
		if (children.length == 0)
			return error("Expected at least on lexical declaration");

		return new LexicalDeclarationNode(decl,children);
	}
	Node parseWithStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "with");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected parenthesis");
		scanAndSkipCommentsAndTerminators();
		Node[] children = [parseExpression(Attribute.In | attributes.mask!(Attribute.Yield))];
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis");
		scanAndSkipCommentsAndTerminators();
		children ~= parseStatement(attributes);
		return new WithStatementNode(children);
	}
	Node parseThrowStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "throw");
		scanToken();
		return new ThrowStatementNode(parseExpression(Attribute.In | attributes.mask!(Attribute.Yield)));
	}
	Node parseTryStatement(int attributes = 0)
	{
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
					catchChildren ~= parseObjectBindingPattern(attributes.mask!(Attribute.Yield));
					break;
				case Type.OpenSquareBrackets:
					catchChildren ~= parseArrayBindingPattern(attributes.mask!(Attribute.Yield));
					break;
				case Type.Identifier:
					catchChildren ~= parseIdentifier(attributes.mask!(Attribute.Yield));
					break;
				default:
					return error("Expected ObjectBindingPattern, ArrayBindingPattern or Identifier");
			}
			if (token.type != Type.CloseParenthesis)
				return error("Expected closing parenthesis");
			scanAndSkipCommentsAndTerminators();
			catchChildren ~= parseBlockStatement(attributes);
			children ~= new CatchStatementNode(catchChildren);
		}
		if (token.type == Type.Identifier && token.match == "finally")
		{
			scanAndSkipCommentsAndTerminators();
			children ~= new FinallyStatementNode(parseBlockStatement(attributes));
		}
		return new TryStatementNode(children);
	}
	Node parseDebuggerStatement(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "debugger");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.Semicolon)
			return error("Expected semicolon after debugger keyword");
		return new DebuggerStatementNode();
	}
	Node parseClassDeclaration(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "class");
		scanAndSkipCommentsAndTerminators();
		Node name;
		if (token.type == Type.Identifier)
			name = parseIdentifier();
		else if (!attributes.has!(Attribute.Default))
			return error("Expected Identifier as part of ClassDeclaration");
		Node base;
		if (token.type == Type.Identifier && token.match == "extends")
		{
			scanAndSkipCommentsAndTerminators();
			base = parseLeftHandSideExpression(attributes.mask!(Attribute.Yield));
		}
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace as part of class declaration");
		scanAndSkipCommentsAndTerminators();
		Node[] methods;
		bool staticAttr = false;
		while(!isEndOfExpression || token.type == Type.Semicolon)
		{
			switch(token.type)
			{
				case Type.Identifier:
					switch (token.match)
					{
						case "static":
							if (staticAttr)
								return error("Expected class method after static");
							staticAttr = true;
							scanAndSkipCommentsAndTerminators();
							continue;
						case "set":
							methods ~= parseClassSetter(staticAttr,attributes.mask!(Attribute.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
						case "get":
							methods ~= parseClassGetter(staticAttr,attributes.mask!(Attribute.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
						default:
							methods ~= parseClassMethod(staticAttr,attributes.mask!(Attribute.Yield));
							if (methods[$-1].type == NodeType.ErrorNode)
								return methods[$-1];
							break;
					}
					staticAttr = false;
					break;
				case Type.Multiply:
					methods ~= parseClassGeneratorMethod(staticAttr,attributes.mask!(Attribute.Yield));
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
				default:
					return error("Expected keyword static, class method, class generator, setter or getter");
			}
		}
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing brace as part of class declaration");
		scanAndSkipCommentsAndTerminators();
		return new ClassDeclarationNode(name,base,methods);

	}
	Node parseClassGetter(bool isStatic, int attributes = 0)
	{
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
			return error("Expected closing brace");
		scanToken();
		return new ClassGetterNode(isStatic,name,funcBody);
	}
	Node parseClassSetter(bool isStatic, int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "set");
		scanAndSkipCommentsAndTerminators();
		auto name = parsePropertyName(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of class setter");
		scanToken();
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
			return error("Expected closing brace");
		scanToken();
		return new ClassSetterNode(isStatic,name,param,funcBody);
	}
	Node parseBindingElement(int attributes = 0)
	{
		skipCommentsAndLineTerminators();
		switch(token.type)
		{
			case Type.OpenCurlyBrace: 
				auto pattern = parseObjectBindingPattern(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return pattern;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Attribute.In | attributes);
				return new BindingElementNode(pattern,expr);
			case Type.OpenSquareBrackets:
				auto pattern = parseArrayBindingPattern(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return pattern;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Attribute.In | attributes);
				return new BindingElementNode(pattern,expr);
			case Type.Identifier:
				auto name = parseIdentifier(attributes);
				skipCommentsAndLineTerminators();
				if (token.type != Type.Assignment)
					return name;
				scanAndSkipCommentsAndTerminators();
				auto expr = parseAssignmentExpression(Attribute.In | attributes);
				return new SingleNameBindingNode(name,expr);
			default:
				return error(format("Expected BindingElement, instead got %s",token));
		}
	}
	Node parseClassMethod(bool isStatic, int attributes = 0, Node name = null)
	{
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
			return error("Expected closing brace");
		scanToken();
		return new ClassMethodNode(isStatic,name,params,funcBody);
	}
	Node parseFormalParameterList(int attributes = 0)
	{
		Node[] children;
		skipCommentsAndLineTerminators();
		while(!isEndOfExpression)
		{
			if (token.type == Type.SpreadOperator)
			{
				scanAndSkipCommentsAndTerminators();
				children ~= new RestElementNode(parseIdentifier(attributes));
				break;
			}
			children ~= parseBindingElement(attributes);
			skipCommentsAndLineTerminators();
			if (token.type != Type.Comma)
				break;
			scanAndSkipCommentsAndTerminators();
		}
		return new FormalParameterListNode(children);
	}
	Node parseClassGeneratorMethod(bool isStatic, int attributes = 0)
	{
		assert(token.type == Type.Multiply);
		scanAndSkipCommentsAndTerminators();
		auto name = parsePropertyName(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.OpenParenthesis)
			return error("Expected opening parenthesis as part of class method");
		scanToken();
		auto params = parseFormalParameterList(attributes);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseParenthesis)
			return error("Expected closing parenthesis as part of class method");
		scanAndSkipCommentsAndTerminators();
		if (token.type != Type.OpenCurlyBrace)
			return error("Expected opening brace");
		scanAndSkipCommentsAndTerminators();
		auto funcBody = parseFunctionBody(Attribute.Yield);
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing brace");
		scanToken();
		return new ClassGeneratorMethodNode(isStatic,name,params,funcBody);
	}
	Node parsePropertyName(int attributes = 0)
	{
		if (token.type == Type.OpenSquareBrackets)
		{
			scanToken();
			skipCommentsAndLineTerminators();
			auto expr = parseAssignmentExpression(Attribute.In | attributes.mask!(Attribute.Yield));
			if (token.type != Type.CloseSquareBrackets)
				return error("Expected closing brace after ComputedPropertyName");
			scanAndSkipCommentsAndTerminators();
			return new ComputedPropertyNameNode(expr);
		}
		switch(token.type)
		{
			case Type.Identifier: return parseIdentifierName();
			case Type.StringLiteral: auto node = new StringLiteralNode(token.match); scanToken(); return node;
			case Type.BinaryLiteral: auto node = new BinaryLiteralNode(token.match); scanToken(); return node;
			case Type.OctalLiteral: auto node = new OctalLiteralNode(token.match); scanToken(); return node;
			case Type.DecimalLiteral: auto node = new DecimalLiteralNode(token.match); scanToken(); return node;
			case Type.HexLiteral: auto node = new HexLiteralNode(token.match); scanToken(); return node;
			default:
				return error(format("Unexpected token %s in PropertyName",token.type));
		}
	}
	Node parseFunctionDeclaration(int attributes = 0)
	{
		assert(token.type == Type.Identifier && token.match == "function");
		scanAndSkipCommentsAndTerminators();
		bool generator = false;
		if (token.type == Type.Multiply)
		{
			generator =  true;
			scanAndSkipCommentsAndTerminators();
		}
		Node name = null;
		if (attributes.has!(Attribute.Default))
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
		auto funcBody = generator ? parseFunctionBody(Attribute.Yield) : parseFunctionBody();
		skipCommentsAndLineTerminators();
		if (token.type != Type.CloseCurlyBrace)
			return error("Expected closing brace");
		scanAndSkipCommentsAndTerminators();
		if (generator)
			return new GeneratorDeclarationNode(name,params,funcBody);
		return new FunctionDeclarationNode(name,params,funcBody);
	}
	Node parseFunctionBody(int attributes = 0)
	{
		return new FunctionBodyNode(parseStatementList(Attribute.Return | attributes));
	}
}

auto parser(Input)(Input i)
{
	import std.range : chain;
	auto c = chain(i,"\u0000");
	return new Parser!(typeof(c))(c);
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
		parser.empty.shouldBeTrue();
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
	assertNodeType!IdentifierNode("identifier");
	assertNodeType!ParenthesisNode("()");
	assertNodeType!KeywordNode("this").keyword.shouldEqual(Keyword.This);
	assertNodeType!KeywordNode("null").keyword.shouldEqual(Keyword.Null);
	assertNodeType!BooleanNode("true").value.shouldBeTrue();
	assertNodeType!BooleanNode("false").value.shouldBeFalse();
}
@("parseUnaryExpression")
unittest
{
	auto parseUnaryExpression(Type = UnaryExpressionNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseUnaryExpression();
		return shouldBeOfType!(Type)(n,file,line);
	}
	void assertUnaryExpressionPrefix(string r, Prefix[] prefixs, Postfix postfix = Postfix.None, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto unExpr = parseUnaryExpression(r,file,line);
		unExpr.prefixs.length.shouldEqual(prefixs.length,file,line);
		foreach(idx, prefix; prefixs)
			unExpr.prefixs[idx].shouldBeOfType!(PrefixExpressionNode).prefix.shouldEqual(prefix);
		unExpr.postfix.shouldEqual(postfix);
	}
	assertUnaryExpressionPrefix("delete abc",[Prefix.Delete]);
	assertUnaryExpressionPrefix("void abc",[Prefix.Void]);
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
	assertUnaryExpressionPrefix("typeof //comment \n!\nabc /* multi \n line */\n--",[Prefix.Typeof,Prefix.Negation],Postfix.Decrement);
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
		if (!parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
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
	assertNodeType!IdentifierNode("identifier");
	assertNodeType!ParenthesisNode("()");
	assertNodeType!KeywordNode("this").keyword.shouldEqual(Keyword.This);
	assertNodeType!KeywordNode("null").keyword.shouldEqual(Keyword.Null);
	assertNodeType!BooleanNode("true").value.shouldBeTrue();
	assertNodeType!BooleanNode("false").value.shouldBeFalse();
}
@("parseRightHandSideExpression")
unittest
{
	Type assertBinaryExpression(Type = BinaryExpressionNode)(in string r, ExpressionOperator[] ops, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.range : lockstep, stride, drop;
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseRightHandSideExpression(Attribute.In);
		if (!parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		auto t = shouldBeOfType!(Type)(n,file,line);
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

	parseConditionalExpression!(IdentifierNode)("abc");
	parseConditionalExpression("abc ? 6 : 7");
	parseConditionalExpression("abc ?").shouldThrow();
	parseConditionalExpression("abc ? 6").shouldThrow();
	parseConditionalExpression("abc ? 6 :").shouldThrow();
}
@("parseAssignmentExpression")
unittest
{
	auto parseAssignmentExpression(Type = AssignmentExpressionNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseAssignmentExpression();
		//if (!parser.empty)
			//throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}
	parseAssignmentExpression!(IdentifierNode)("abc");
	auto assign = parseAssignmentExpression("abc12 = def");
	assign.children.length.shouldEqual(3);
	assign.children[0].shouldBeOfType!IdentifierNode;
	assign.children[1].shouldBeOfType!AssignmentOperatorNode;
	assign.children[2].shouldBeOfType!IdentifierNode;

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
}
@("parseExpression")
unittest
{
	auto parseExpression(Type = ExpressionNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseExpression();
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}
	parseExpression!(IdentifierNode)("abc");
	parseExpression!(ConditionalExpressionNode)("abc ? 7 : 6");
	parseExpression!(UnaryExpressionNode)("!bla");
	parseExpression!(BinaryExpressionNode)("bla & 7");
	parseExpression("abc() // \n, /* multi \n line \r\n comment */ \n \r\n def = ghi");
	parseExpression!(ErrorNode)(",");
	parseExpression!(ErrorNode)("bla,,");
	parseExpression!(ErrorNode)("");

	auto expr = parseExpression("abc, def");
	expr.children.length.shouldEqual(2);
	expr.children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("abc");
	expr.children[1].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("def");

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
	auto parseStatement(Type)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseStatement();
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}

	parseStatement!(IdentifierNode)("abc").identifier.shouldEqual("abc");
	parseStatement!(IdentifierNode)("abc;");
	parseStatement!(IdentifierNode)("abc  \n\r\n;");
	parseStatement!(IdentifierNode)("abc  \n\r\n // \n;");
	parseStatement!(IdentifierNode)("abc  \n\r\n /* multi \n line \r\n comment */ \n;");
	parseStatement!(LabelledStatementNode)("abc  \n\r\n /* multi \n line \r\n comment */ \n:");
}
@("parseVariableStatement")
unittest
{
	alias parseVariableStatement(Type = VariableStatementNode) = parseNode!("parseVariableStatement",Type);

	parseVariableStatement("var ").shouldThrow();
	parseVariableStatement("var a").children[0].shouldBeOfType!(VariableDeclarationNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("a");
	parseVariableStatement("var a,b").children[1].shouldBeOfType!(VariableDeclarationNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("b");
	parseVariableStatement("var a = 77").children[0].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("77");
	parseVariableStatement("var a = b = 77").children[0].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(AssignmentExpressionNode);
	parseVariableStatement("var a = b = 77, c = 44").children[1].shouldBeOfType!(VariableDeclarationNode).children[1].shouldBeOfType!(DecimalLiteralNode).value.shouldEqual("44");
	parseVariableStatement("var /* multi \r\n line \n comment */ \r\n // \n \n a");
}
@("parseForStatement")
unittest
{
	auto parseForStatement(Type = ForStatementNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseForStatement();
		if (n.type == NodeType.ErrorNode)
		{
			import std.range : repeat;
			auto err = n.shouldBeOfType!(ErrorNode);
			throw new UnitTestException([format("%s\n%s\n%s^",err,r,' '.repeat(err.column-1))],file,line);
		}
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}

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
}
@("parseObjectLiteral")
unittest
{
	auto parseObjectLiteral(Type = ObjectLiteralNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseObjectLiteral();
		if (n.type == NodeType.ErrorNode)
		{
			import std.range : repeat;
			auto err = n.shouldBeOfType!(ErrorNode);
			throw new UnitTestException([format("%s\n%s\n%s^",err,r,' '.repeat(err.column-1))],file,line);
		}
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}

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
	parseObjectLiteral("{c=6}");
	parseObjectLiteral("{c=6").shouldThrowSaying("Error: Expected closing curly brace before EndOfFile");
	parseObjectLiteral(`{"abc"}`).shouldThrowSaying("Error: Expected colon as part of PropertyDefinition");
	parseObjectLiteral(`{function}`).shouldThrowSaying("Error: Unexpected keyword function");
	parseObjectLiteral(`{,}`).shouldThrowSaying("Error: Expected a PropertyDefinition");

}
@("parseFunctionExpression")
unittest
{
	auto parseFunctionExpression(Type = FunctionExpressionNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseFunctionExpression();
		if (n.type == NodeType.ErrorNode)
		{
			import std.range : repeat;
			auto err = n.shouldBeOfType!(ErrorNode);
			throw new UnitTestException([format("%s\n%s\n%s^",err,r,' '.repeat(err.column-1))],file,line);
		}
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}

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
	alias parseObjectBindingPattern(Type = ObjectBindingPatternNode) = parseNode!("parseObjectBindingPattern",Type);

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
	alias parseArrayBindingPattern(Type = ArrayBindingPatternNode) = parseNode!("parseArrayBindingPattern",Type);

	parseArrayBindingPattern(`[,,[a,b],l]`);
	parseArrayBindingPattern(`[l,,m,k=5]`);
	parseArrayBindingPattern(`[,,[a,b],l,,,m,k=5,{h:p},...rest]`);
}
@("parseDoWhileStatement")
unittest
{
	auto parseDoWhileStatement(Type = DoWhileStatementNode)(string r, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(r);
		parser.scanToken();
		auto n = parser.parseDoWhileStatement();
		if (n.type == NodeType.ErrorNode)
		{
			import std.range : repeat;
			auto err = n.shouldBeOfType!(ErrorNode);
			throw new UnitTestException([format("%s\n%s\n%s^",err,r,' '.repeat(err.column-1))],file,line);
		}
		if (n.type != NodeType.ErrorNode && !parser.empty)
			throw new UnitTestException([format("Expected input to be empty, got %s",parser.s)],file,line);
		return shouldBeOfType!(Type)(n,file,line);
	}

	parseDoWhileStatement("do a;while(true)");
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

	parseStatementList(";")[0].shouldBeOfType!(EmptyStatementNode);
	parseStatementList("").length.shouldEqual(0);
}
@("parseIdentifier")
unittest
{
	alias parseIdentifier(Type = IdentifierNode) = parseNode!("parseIdentifier",Type);

	parseIdentifier("name");
	parseIdentifier("name_");
	parseIdentifier("_name");
}
@("parseClassDeclaration")
unittest
{
	alias parseClassDeclaration(Type = ClassDeclarationNode) = parseNode!("parseClassDeclaration",Type);

	parseClassDeclaration("class abc{}").name.shouldBeOfType!(IdentifierNode).identifier.shouldEqual("abc");
	parseClassDeclaration("class abc extends def{}").base.shouldBeOfType!(IdentifierNode).identifier.shouldEqual("def");
	parseClassDeclaration("class abc{m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{m(){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{*m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{*m(){}}").methods[0].shouldBeOfType!(ClassGeneratorMethodNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{set m(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{set m(abc){}}").methods[0].shouldBeOfType!(ClassSetterNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{get m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{get m(){}}").methods[0].shouldBeOfType!(ClassGetterNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static m(){}}").methods[0].shouldBeOfType!(ClassMethodNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static *m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static *m(){}}").methods[0].shouldBeOfType!(ClassGeneratorMethodNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static set m(abc){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static set m(abc){}}").methods[0].shouldBeOfType!(ClassSetterNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class abc{static get m(){}}").methods.length.shouldEqual(1);
	parseClassDeclaration("class abc{static get m(){}}").methods[0].shouldBeOfType!(ClassGetterNode).children[0].shouldBeOfType!(IdentifierNode).identifier.shouldEqual("m");
	parseClassDeclaration("class name { a(){}/*comment*/b(){}\nc(){} // comment \nd(){}; ;; }");
	parseClassDeclaration("class {}").shouldThrowSaying("Expected Identifier as part of ClassDeclaration");
	parseClassDeclaration("class name").shouldThrowSaying("Expected opening brace as part of class declaration");
	parseClassDeclaration("class name { static static }").shouldThrowSaying("Expected class method after static");
	parseClassDeclaration("class name { static ; }").shouldThrowSaying("Expected class method after static");
	parseClassDeclaration("class name { + }").shouldThrowSaying("Expected keyword static, class method, class generator, setter or getter");
	parseClassDeclaration("class abc{").shouldThrowSaying("Expected closing brace as part of class declaration");
	parseClassDeclaration("class abc{get m").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m(").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m(a)").shouldThrowSaying("Expected empty parenthesis as part of class getter");
	parseClassDeclaration("class abc{get m()}").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{get m(){").shouldThrowSaying("Expected closing brace");
	parseClassDeclaration("class abc{set m").shouldThrowSaying("Expected opening parenthesis as part of class setter");
	parseClassDeclaration("class abc{set m(){}").shouldThrowSaying("Expected BindingElement");
	parseClassDeclaration("class abc{set m(a,b){}").shouldThrowSaying("Expected closing parenthesis as part of class setter");
	parseClassDeclaration("class abc{set m(a)").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{set m(a){").shouldThrowSaying("Expected closing brace");
	parseClassDeclaration("class abc{m").shouldThrowSaying("Expected opening parenthesis as part of class method");
	parseClassDeclaration("class abc{m(").shouldThrowSaying("Expected closing parenthesis as part of class method");
	parseClassDeclaration("class abc{m()").shouldThrowSaying("Expected opening brace");
	parseClassDeclaration("class abc{m(){").shouldThrowSaying("Expected closing brace");
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
	assertImportDeclarationError(`
		import
		a from;
		var b
		= 6;`,
		`
		import
		a from;
		      ^ Expected StringLiteral as part of ImportDeclaration
		var b
		= 6;`
	);
	assertImportDeclarationError(`/*

		*/


		import
		a from;
		var b
		= 6;`,
		`
		import
		a from;
		      ^ Expected StringLiteral as part of ImportDeclaration
		var b
		= 6;`
	);
	assertImportDeclarationError(`
		import

		/* multi
		line
		comment

		*/
		-- from "file.js";
		var b
		= 6;`,
		`
		*/
		-- from "file.js";
		^ Unexpected Decrement as part of ImportDeclaration
		var b
		= 6;`
	);
	assertImportDeclarationError(`
		import
		// comment
		// comment 2
		b "file.js";
		var b
		= 6;`,
		`		// comment
		// comment 2
		b "file.js";
		  ^ Expected from as part of ImportDeclaration
		var b
		= 6;`
	);
}




