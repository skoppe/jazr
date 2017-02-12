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
module es6.emitter;

import es6.nodes;
import std.range : repeat;
import std.algorithm : copy;

version (unittest)
{
	import unit_threaded;
	import es6.parser;
	import std.stdio;
	void assertEmitted(string input, in string file = __FILE__, in size_t line = __LINE__)
	{
		parse(input).shouldEqual(input,file,line);
	}
	auto parse(string input)
	{
		struct Result
		{
			string got;
			void shouldEqual(string expected, in string file = __FILE__, in size_t line = __LINE__)
			{
				got.shouldEqual(expected,file,line);
			}
		}
		auto parser = parser(input);
		parser.scanToken();
		auto root = parser.parseModule();
		writeln(root);
		return Result(root.emit());
	}
}
enum Guide
{
	None = 1 << 0,
	RequiresDelimiter = 1 << 1,
	RequiresSemicolon = 1 << 2,
	EndOfStatement = 1 << 3,
	Void = 1 << 4,
	RequiresWhitespaceBeforeIdentifier = 1 << 5,
	EndOfList = 1 << 6,
	PlusRequiresWhitespace = 1 << 7,
	MinusRequiresWhitespace = 1 << 8
}
Guide emitDelimited(Sink)(Node[] nodes, Sink sink, string delimiter, int guide = Guide.None)
{
	if (nodes.length == 0)
		return Guide.Void;
	auto g = guide;
	foreach(c; nodes[0..$-1])
	{
		auto r = c.emit(sink,g);
		if (r & Guide.RequiresSemicolon)
			sink.put(";");
		else if (!(r & Guide.EndOfStatement) || delimiter != ";")
			sink.put(delimiter);
		//if (r & Guide.RequiresWhitespaceBeforeIdentifier)
			//guide |= Guide.RequiresWhitespaceBeforeIdentifier;
		//else
			guide &= ~Guide.RequiresWhitespaceBeforeIdentifier;
		g = (guide | r) & ~Guide.RequiresWhitespaceBeforeIdentifier;
	}
	auto r = nodes[$-1].emit(sink,g | Guide.EndOfList);
	if (r & Guide.RequiresSemicolon)
		return Guide.RequiresSemicolon;
	if (r & Guide.RequiresDelimiter)
		return Guide.RequiresDelimiter;
	return Guide.None; // Todo
}
Guide emit(Sink)(Node[] nodes, Sink sink, int guide = Guide.None)
{
	Guide r;
	auto g = guide;
	foreach(c; nodes)
	{
		r = c.emit(sink,g);
		if (r & Guide.RequiresWhitespaceBeforeIdentifier)
			guide |= Guide.RequiresWhitespaceBeforeIdentifier;
		else
			guide &= ~Guide.RequiresWhitespaceBeforeIdentifier;
		g = guide | r;
	}
	return r;
}
string emit(Node node)
{
	import std.array : appender;
	auto app = appender!string;
	node.emit(app);
	return app.data;
}
Guide emit(Sink)(Node node, Sink sink, int guide = Guide.None)
{
	switch(node.type)
	{
		case NodeType.ErrorNode:
			auto n = node.as!ErrorNode;
			return Guide.None;
		case NodeType.BooleanNode:
			auto n = node.as!BooleanNode;
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put(n.value ? "true": "false");
			return Guide.RequiresDelimiter;
		case NodeType.StringLiteralNode:
			auto n = node.as!StringLiteralNode;
			sink.put("\'");
			sink.put(n.value);
			sink.put("\'");
			return Guide.None;
		case NodeType.BinaryLiteralNode:
			auto n = node.as!BinaryLiteralNode;
			if (guide == Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("0b");
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.OctalLiteralNode:
			auto n = node.as!OctalLiteralNode;
			if (guide == Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("0o");
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.DecimalLiteralNode:
			auto n = node.as!DecimalLiteralNode;
			if (guide == Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.HexLiteralNode:
			auto n = node.as!HexLiteralNode;
			if (guide == Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("0x");
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.TemplateNode:
			auto n = node.as!TemplateNode;
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.TemplateLiteralNode:
			sink.put("`");
			node.children[0].emit(sink);
			import std.range : chunks;
			foreach(chunk; node.children[1..$].chunks(2))
			{
				sink.put("${");
				chunk[0].emit(sink);
				sink.put("}");
				chunk[1].emit(sink);
			}
			sink.put("`");
			return Guide.EndOfStatement;
		case NodeType.RegexLiteralNode:
			auto n = node.as!RegexLiteralNode;
			sink.put(n.value);
			return Guide.RequiresDelimiter;
		case NodeType.KeywordNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			auto n = node.as!KeywordNode;
			switch(n.keyword)
			{
				case Keyword.This: sink.put("this"); break;
				case Keyword.Null: sink.put("null"); break; default: assert(0);
			}
			return Guide.RequiresDelimiter;
		case NodeType.IdentifierNode:
			auto n = node.as!IdentifierNode;
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put(n.identifier);
			return Guide.RequiresDelimiter | Guide.RequiresWhitespaceBeforeIdentifier;
		case NodeType.ExpressionNode:
			node.children.emitDelimited(sink,",",guide);
			return Guide.None;
		case NodeType.ParenthesisNode:
			auto n = node.as!ParenthesisNode;
			sink.put('(');
			n.children.emitDelimited(sink,",");
			sink.put(')');
			return Guide.RequiresDelimiter;
		case NodeType.PrefixExpressionNode:
			auto n = node.as!PrefixExpressionNode;
			final switch(n.prefix)
			{
				case Prefix.Delete: if (guide & Guide.RequiresWhitespaceBeforeIdentifier) sink.put(" "); sink.put("delete"); return Guide.RequiresWhitespaceBeforeIdentifier;
				case Prefix.Void: if (guide & Guide.RequiresWhitespaceBeforeIdentifier) sink.put(" "); sink.put("void"); return Guide.RequiresWhitespaceBeforeIdentifier;
				case Prefix.Typeof: if (guide & Guide.RequiresWhitespaceBeforeIdentifier) sink.put(" "); sink.put("typeof"); return Guide.RequiresWhitespaceBeforeIdentifier;
				case Prefix.Increment:
					if (guide & Guide.PlusRequiresWhitespace)
						sink.put(" ");
					sink.put("++"); 
					break;
				case Prefix.Decrement:
					if (guide & Guide.MinusRequiresWhitespace)
						sink.put(" ");
					sink.put("--");
					break;
				case Prefix.Positive:
					if (guide & Guide.PlusRequiresWhitespace)
						sink.put(" ");
					sink.put("+");
					break;
				case Prefix.Negative:
					if (guide & Guide.MinusRequiresWhitespace)
						sink.put(" ");
					sink.put("-");
					break;
				case Prefix.Tilde: sink.put("~"); break;
				case Prefix.Negation: sink.put("!"); break;
			}
			return Guide.None;
		case NodeType.SuperPropertyNode:
			auto n = node.as!SuperPropertyNode;
			sink.put("super");
			if (n.children[0].type == NodeType.IdentifierNode)
				sink.put(".");
			n.children[0].emit(sink);
			return Guide.RequiresDelimiter;
		case NodeType.AccessorNode:
			auto n = node.as!AccessorNode;
			sink.put(".");
			sink.put(n.identifier);
			return Guide.RequiresDelimiter;
		case NodeType.NewTargetNode:
			sink.put("new.target");
			return Guide.RequiresDelimiter;
		case NodeType.SpreadOperatorNode:
			sink.put("...");
			//node.children[0].emit(sink);
			return Guide.None;
		case NodeType.ArgumentsNode:
			sink.put("(");
			node.children.emitDelimited(sink,",");
			sink.put(")");
			return Guide.None;
		case NodeType.ArrayIndexNode:
			sink.put("[");
			node.children.emitDelimited(sink,",");
			sink.put("]");
			return Guide.None;
		case NodeType.NewExpressionNode:
			auto n = node.as!NewExpressionNode;
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			"new ".repeat(n.news).copy(sink);
			n.children.emit(sink);
			return Guide.RequiresDelimiter;
		case NodeType.CallExpressionNode:
			auto n = node.as!CallExpressionNode;
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			"new ".repeat(n.news).copy(sink);
			n.children.emit(sink);
			return Guide.RequiresDelimiter;
		case NodeType.UnaryExpressionNode:
			auto n = node.as!UnaryExpressionNode;
			auto g = n.prefixs.emit(sink,guide);
			n.children[0].emit(sink,g);
			final switch(n.postfix)
			{
				case Postfix.Increment: 
					if (guide & Guide.PlusRequiresWhitespace)
						sink.put(" ");
					sink.put("++");
					return Guide.RequiresDelimiter;
				case Postfix.Decrement: 
					if (guide & Guide.MinusRequiresWhitespace)
						sink.put(" ");
					sink.put("--");
					return Guide.RequiresDelimiter;
				case Postfix.None: break;
			}
			return Guide.RequiresDelimiter;
		case NodeType.ExpressionOperatorNode:
			auto n = node.as!ExpressionOperatorNode;
			final switch(n.operator)
			{
				case ExpressionOperator.InstanceOf: sink.put(" instanceof"); return Guide.RequiresWhitespaceBeforeIdentifier;
				case ExpressionOperator.In: if (guide & Guide.RequiresWhitespaceBeforeIdentifier) sink.put(" "); sink.put("in"); return Guide.RequiresWhitespaceBeforeIdentifier;
				case ExpressionOperator.LogicalAnd: sink.put("&&"); break;
				case ExpressionOperator.LogicalOr: sink.put("||"); break;
				case ExpressionOperator.BitwiseAnd: sink.put("&"); break;
				case ExpressionOperator.BitwiseOr: sink.put("|"); break;
				case ExpressionOperator.BitwiseXor: sink.put("^"); break;
				case ExpressionOperator.StrictEqual: sink.put("==="); break;
				case ExpressionOperator.Equal: sink.put("=="); break;
				case ExpressionOperator.StrictNotEqual: sink.put("!=="); break;
				case ExpressionOperator.NotEqual: sink.put("!="); break;
				case ExpressionOperator.LessOrEqual: sink.put("<="); break;
				case ExpressionOperator.LessThan: sink.put("<"); break;
				case ExpressionOperator.GreaterOrEqual: sink.put(">="); break;
				case ExpressionOperator.GreaterThan: sink.put(">"); break;
				case ExpressionOperator.LeftShift: sink.put("<<"); break;
				case ExpressionOperator.TripleRightSift: sink.put(">>>"); break;
				case ExpressionOperator.RightShift: sink.put(">>"); break;
				case ExpressionOperator.Add: 
					if (guide & Guide.PlusRequiresWhitespace)
						sink.put(" ");
					sink.put("+");
					return Guide.PlusRequiresWhitespace;
				case ExpressionOperator.Minus:
					if (guide & Guide.MinusRequiresWhitespace)
						sink.put(" ");
					sink.put("-");
					return Guide.MinusRequiresWhitespace;
				case ExpressionOperator.Multiply: sink.put("*"); break;
				case ExpressionOperator.Division: sink.put("/"); break;
				case ExpressionOperator.Mod: sink.put("%"); break;
			}
			return Guide.None;
		case NodeType.ConditionalExpressionNode:
			auto n = node.as!ConditionalExpressionNode;
			n.children[0].emit(sink,guide);
			sink.put("?");
			n.children[1].emit(sink);
			sink.put(":");
			n.children[2].emit(sink);
			return Guide.RequiresDelimiter;
		case NodeType.AssignmentExpressionNode:
			node.children.emit(sink,guide);
			return Guide.RequiresDelimiter;
		case NodeType.ArrowFunctionNode:
			auto n = node.as!ArrowFunctionNode;
			n.children[0].emit(sink);
			sink.put("=>");
			return n.children[1].emit(sink);
		case NodeType.AssignmentOperatorNode:
			auto n = node.as!AssignmentOperatorNode;
			final switch(n.assignment)
			{
				case Assignment.LeftShiftAssignment: sink.put("<<="); break;
				case Assignment.TripleRightShiftAssignment: sink.put(">>>="); break;
				case Assignment.RightShiftAssignment: sink.put(">>="); break;
				case Assignment.Assignment: sink.put("="); break;
				case Assignment.AdditiveAssignment: sink.put("+="); break;
				case Assignment.DecrementalAssignment: sink.put("-="); break;
				case Assignment.MultiplicativeAssignment: sink.put("*="); break;
				case Assignment.DivisionAssignment: sink.put("/="); break;
				case Assignment.ModAssignment: sink.put("%="); break;
				case Assignment.BitwiseAndAssignment: sink.put("&="); break;
				case Assignment.BitwiseOrAssignment: sink.put("|="); break;
				case Assignment.BitwiseXorAssignment: sink.put("^="); break;
			}
			return Guide.None;
		case NodeType.ContinueStatementNode:
			sink.put("continue");
			return Guide.RequiresSemicolon;
		case NodeType.BreakStatementNode:
			sink.put("break");
			return Guide.RequiresSemicolon;
		case NodeType.EmptyStatementNode:
			sink.put(";");
			return Guide.EndOfStatement;
		case NodeType.LabelledStatementNode:
			auto n = node.as!LabelledStatementNode;
			sink.put(n.label);
			sink.put(":");
			return Guide.RequiresSemicolon;
		case NodeType.VariableStatementNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("var");
			node.children.emitDelimited(sink,",",Guide.RequiresWhitespaceBeforeIdentifier);
			return Guide.RequiresSemicolon;
		case NodeType.VariableDeclarationNode:
			node.children[0].emit(sink,guide);
			if (node.children.length ==2)
			{
				sink.put("=");
				node.children[1].emit(sink);
			}
			return Guide.None;
		case NodeType.ReturnStatementNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("return");
			if (node.children.length == 1)
				return node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			return Guide.RequiresSemicolon;
		case NodeType.BlockStatementNode:
			sink.put("{");
			node.children.emitDelimited(sink,";");
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.IfStatementNode:
			if (guide == Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("if(");
			node.children[0].emit(sink);
			sink.put(")");
			auto r = node.children[1].emit(sink);
			if (node.children.length > 2)
			{
				if (r != Guide.EndOfStatement)
					sink.put(";");
				sink.put("else");
				return node.children[2].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			}
			return r;
		case NodeType.SwitchStatementNode:
			sink.put("switch(");
			node.children[0].emit(sink);
			sink.put("){");
			node.children[1..$].emitDelimited(sink,";");
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.DoWhileStatementNode:
			sink.put("do");
			auto r = node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			if (r & (Guide.RequiresSemicolon | Guide.RequiresDelimiter))
				sink.put(";");
			sink.put("while(");
			node.children[1].emit(sink);
			sink.put(")");
			return Guide.RequiresSemicolon;
		case NodeType.WhileStatementNode:
			sink.put("while(");
			node.children[0].emit(sink);
			sink.put(")");
			return node.children[1].emit(sink);
		case NodeType.CaseNode:
			sink.put("case");
			node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			sink.put(":");
			if (node.children.length == 2)
				return node.children[1].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.CaseBodyNode:
			node.children.emitDelimited(sink,";");
			return node.children.length > 0 ? Guide.RequiresSemicolon : Guide.EndOfStatement;
		case NodeType.DefaultNode:
			sink.put("default:");
			node.children.emitDelimited(sink,";");
			return Guide.None;
		case NodeType.ForStatementNode:
			auto n = node.as!ForStatementNode;
			sink.put("for(");
			guide = Guide.None;
			switch(n.loopType)
			{
				case ForLoop.ExprCStyle:
				case ForLoop.VarCStyle:
				case ForLoop.ConstCStyle:
				case ForLoop.LetCStyle:
					n.children[0..$-1].emit(sink);
					break;
				case ForLoop.VarIn:
				case ForLoop.VarOf:
					sink.put("var");
					guide = Guide.RequiresWhitespaceBeforeIdentifier;
					goto default;
				case ForLoop.ConstIn:
				case ForLoop.ConstOf:
					sink.put("const");
					guide = Guide.RequiresWhitespaceBeforeIdentifier;
					goto default;
				case ForLoop.LetIn:
				case ForLoop.LetOf:
					sink.put("let");
					guide = Guide.RequiresWhitespaceBeforeIdentifier;
					goto default;
				default:
					auto g = n.children[0].emit(sink,guide);
					switch (n.loopType)
					{
						case ForLoop.ExprIn:
						case ForLoop.VarIn:
						case ForLoop.ConstIn:
						case ForLoop.LetIn:
							if (g & Guide.RequiresWhitespaceBeforeIdentifier)
								sink.put(" ");
							sink.put("in");
							break;
						case ForLoop.ExprOf:
						case ForLoop.VarOf:
						case ForLoop.ConstOf:
						case ForLoop.LetOf:
							if (g & Guide.RequiresWhitespaceBeforeIdentifier)
								sink.put(" ");
							sink.put("of");
							break;
						default:
							assert(0);
					}
					n.children[1].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
					break;
			}
			sink.put(")");
			return n.children[$-1].emit(sink);
		case NodeType.WithStatementNode:
			sink.put("with(");
			node.children[0].emit(sink);
			sink.put(")");
			return node.children[1].emit(sink);
		case NodeType.CatchStatementNode:
			sink.put("catch(");
			node.children[0].emit(sink);
			sink.put(")");
			return node.children[1].emit(sink);
		case NodeType.FinallyStatementNode:
			sink.put("finally");
			return node.children[0].emit(sink);
		case NodeType.TryStatementNode:
			sink.put("try");
			node.children.emit(sink);
			return Guide.EndOfStatement;
		case NodeType.ThrowStatementNode:
			sink.put("throw");
			node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			return Guide.RequiresSemicolon;
		case NodeType.DebuggerStatementNode:
			sink.put("debugger");
			return Guide.RequiresSemicolon;
		case NodeType.ClassDeclarationNode:
			auto n = node.as!ClassDeclarationNode;
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("class");
			if (n.name !is null)
				n.name.emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			if (n.base !is null)
			{
				sink.put(" extends");
				n.base.emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			}
			sink.put("{");
			n.methods.emit(sink);
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.ClassGetterNode:
			auto n = node.as!ClassGetterNode;
			if (n.isStatic)
				sink.put("static ");
			sink.put("get ");
			n.children[0].emit(sink);
			sink.put("()");
			n.children[1].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.ClassMethodNode:
			auto n = node.as!ClassMethodNode;
			if (n.isStatic)
				sink.put("static ");
			n.children[0].emit(sink);
			n.children[1].emit(sink);
			n.children[2].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.ClassGeneratorMethodNode:
			auto n = node.as!ClassGeneratorMethodNode;
			if (n.isStatic)
				sink.put("static ");
			sink.put("*");
			n.children[0].emit(sink);
			n.children[1].emit(sink);
			n.children[2].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.ClassSetterNode:
			auto n = node.as!ClassSetterNode;
			if (n.isStatic)
				sink.put("static ");
			sink.put("set ");
			n.children[0].emit(sink);
			sink.put("(");
			n.children[1].emit(sink);
			sink.put(")");
			n.children[2].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.ComputedPropertyNameNode:
			sink.put('[');
			node.children[0].emit(sink);
			sink.put(']');
			return Guide.None;
		case NodeType.FormalParameterListNode:
			sink.put("(");
			node.children.emitDelimited(sink,",");
			sink.put(")");
			return Guide.None;
		case NodeType.FunctionExpressionNode:
		case NodeType.FunctionDeclarationNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("function");
			if (node.children.length == 3)
				node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			node.children[$-2..$].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.GeneratorExpressionNode:
		case NodeType.GeneratorDeclarationNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			sink.put("function*");
			if (node.children.length == 3)
				node.children[0].emit(sink);
			node.children[$-2..$].emit(sink);
			return Guide.EndOfStatement;
		case NodeType.RestElementNode:
		case NodeType.SpreadElementNode:
			sink.put("...");
			node.children[0].emit(sink);
			return Guide.RequiresDelimiter;
		case NodeType.SingleNameBindingNode:
			node.children.emitDelimited(sink,"=");
			return Guide.None;
		case NodeType.ArrayLiteralNode:
			sink.put("[");
			node.children.emitDelimited(sink,",");
			sink.put("]");
			return Guide.None;
		case NodeType.ObjectLiteralNode:
			sink.put("{");
			node.children.emitDelimited(sink,",");
			sink.put("}");
			return Guide.None;
		case NodeType.PropertyDefinitionNode:
			node.children[0].emit(sink);
			sink.put(":");
			return node.children[1].emit(sink);
		case NodeType.CoverInitializedName:
			node.children[0].emit(sink);
			sink.put("=");
			return node.children[1].emit(sink);
		case NodeType.ElisionNode:
			auto n = node.as!ElisionNode;
			if (n.cnt > 0 && !(guide & Guide.EndOfList))
				",".repeat(n.cnt-1).copy(sink);
			else
				",".repeat(n.cnt).copy(sink);
			return Guide.None;
		case NodeType.FunctionBodyNode:
			sink.put("{");
			node.children.emitDelimited(sink,";");
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.LexicalDeclarationItemNode:
			node.children[0].emit(sink,guide);
			if (node.children.length == 2)
			{
				sink.put("=");
				node.children[1].emit(sink);
			}
			return Guide.RequiresDelimiter;
		case NodeType.LexicalDeclarationNode:
			if (guide & Guide.RequiresWhitespaceBeforeIdentifier)
				sink.put(" ");
			auto n = node.as!LexicalDeclarationNode;
			if (n.declaration == LexicalDeclaration.Let)
				sink.put("let");
			else
				sink.put("const");
			node.children.emitDelimited(sink,",",Guide.RequiresWhitespaceBeforeIdentifier);
			return Guide.RequiresSemicolon;
		case NodeType.ArrayBindingPatternNode:
			sink.put("[");
			node.children.emitDelimited(sink,",");
			sink.put("]");
			return Guide.None;
		case NodeType.ObjectBindingPatternNode:
			sink.put("{");
			node.children.emitDelimited(sink,",");
			sink.put("}");
			return Guide.None;
		case NodeType.BindingPropertyNode:
			node.children[0].emit(sink);
			sink.put(":");
			node.children[1].emit(sink);
			return Guide.None;
		case NodeType.ExportClauseNode:
			sink.put("{");
			node.children.emitDelimited(sink,",");
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.ExportDeclarationNode:
			sink.put("export");
			auto r = node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			if (node.children.length == 2)
			{
				if (r & Guide.RequiresWhitespaceBeforeIdentifier)
					sink.put(" ");
				sink.put("from");
				node.children[1].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			}
			return Guide.RequiresSemicolon;
		case NodeType.ExportDefaultDeclarationNode:
			sink.put("export default");
			node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			return Guide.RequiresSemicolon;
		case NodeType.ExportSpecifierNode:
			node.children[0].emit(sink);
			sink.put(" as ");
			node.children[1].emit(sink);
			return Guide.None;
		case NodeType.ImportClauseNode:
			node.children.emitDelimited(sink,",",guide);
			return Guide.None;
		case NodeType.ImportDeclarationNode:
			sink.put("import");
			auto r = node.children[0].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			if (node.children.length == 2)
			{
				if (r != Guide.EndOfStatement)
					sink.put(" ");
				sink.put("from");
				node.children[1].emit(sink,Guide.RequiresWhitespaceBeforeIdentifier);
			}
			return Guide.RequiresSemicolon;
		case NodeType.ImportSpecifierNode:
			node.children[0].emit(sink);
			sink.put(" as ");
			node.children[1].emit(sink);
			return Guide.None;
		case NodeType.NamedImportsNode:
			sink.put("{");
			node.children.emitDelimited(sink,",");
			sink.put("}");
			return Guide.EndOfStatement;
		case NodeType.NameSpaceImportNode:
			sink.put("* as ");
			node.children[0].emit(sink);
			return Guide.None;
		case NodeType.ModuleNode:
			auto r = node.children.emitDelimited(sink,";");
			if (r & Guide.RequiresSemicolon || r & Guide.RequiresDelimiter)
				sink.put(";");
			return Guide.None;
		case NodeType.SemicolonNode:
			sink.put(";");
			return Guide.Void;
		case NodeType.BindingElementNode:
			node.children.emitDelimited(sink,"=");
			return Guide.Void;
		default: break;
	}
	return node.children.emit(sink,guide);
}
@("Import Declaration")
unittest
{
	assertEmitted(`import* as i from'file.js';`);
	assertEmitted(`import{m as i,k}from'file.js';`);
	assertEmitted(`import i,* as r from'file.js';`);
	assertEmitted(`import i,{m as p,k} from'file.js';`);
	assertEmitted(`import i from'file.js';`);
	assertEmitted(`import'file.js';`);
}
@("Export Declaration")
unittest
{
	assertEmitted(`export*from'file.js';`);
	assertEmitted(`export{m,k}from'file.js';`);
	assertEmitted(`export{m as i,k}from'file.js';`);
	assertEmitted(`export{m,k};`);
	assertEmitted(`export{m as i,k};`);
	assertEmitted(`export default function bla(){};`);
	assertEmitted(`export default function(){};`);
	assertEmitted(`export default function*bla(){};`);
	assertEmitted(`export default function*(){};`);
	assertEmitted(`export default class b{};`);
	assertEmitted(`export default class{};`);
	assertEmitted(`export default{a,b,c};`);
	assertEmitted(`export var a=6,{b,c}=d,e;`);
	assertEmitted(`export var a=6,[b,c]=d,e;`);
	assertEmitted(`export function bla(){};`);
	assertEmitted(`export function(){};`);
	assertEmitted(`export function*bla(){};`);
	assertEmitted(`export function*(){};`);
	assertEmitted(`export class b{};`);
	assertEmitted(`export class{};`);
	assertEmitted(`export let x=6,{b,c}=d;`);
	assertEmitted(`export const x=6,{b,c}=d;`);
}
@("Return Statement")
unittest
{
	assertEmitted(`function a(){return void 0}`);
	assertEmitted(`function a(){return delete a}`);
	assertEmitted(`function a(){return typeof a}`);
	assertEmitted(`function a(){return+a}`);
	assertEmitted(`function cd(){if(a)if(b)return 4;else return 5;else return 6}`);
}
@("Function Expression")
unittest
{
	assertEmitted("a=function(){};");
	assertEmitted("a=function(a,b,c){};");
	assertEmitted("a=function(){;};");
	assertEmitted("a=function fun(...rest){};");
	assertEmitted("a=function fun(a,b,c){};");
	assertEmitted("a=function fun(a,b,c,...rest){};");
}
@("Array Literal")
unittest
{
	assertEmitted("[]");
	assertEmitted("[,]");
	assertEmitted("[,,]");
	assertEmitted("[a,b,,c,,,d,...e]");
	assertEmitted("[,,,...e]");
	assertEmitted("[...e]");
}
@("Object Literal")
unittest
{
	// todo a ObjectLiteral and a BlockStatement look very much alike.
	// currently a BlockStatement wins, but this is incorrect.
	// when corrected we can remove the 'a' assignment
	assertEmitted(`a={};`);
	assertEmitted(`a={a};`);
	assertEmitted(`a={b:a};`);
	assertEmitted(`a={[b]:a};`);
	assertEmitted(`a={'123':a};`);
	assertEmitted(`a={'abc':a};`);
	assertEmitted(`a={class:a};`);
	assertEmitted(`a={a=44};`); // this is not valid syntax if not followed by an destructering assignment. still we need to be able to parse it.
	assertEmitted(`a={*bla(){}};`);
	assertEmitted(`a={get b(){}};`);
	assertEmitted(`a={set c(d){}};`);
	assertEmitted(`a={a,[b]:a,'123':a,'abc':a,class:a,a=44,*bla(){},get b(){},set c(d){}};`);
}
@("Parenthesis Expression")
unittest
{
	assertEmitted(`();`);
	assertEmitted(`(a=5,b);`);
	assertEmitted(`(a=5,...b);`);
	assertEmitted(`(...b);`);
}
@("Primary Expression")
unittest
{
	assertEmitted(`this;`);
	assertEmitted(`null;`);
	assertEmitted(`true;`);
	assertEmitted(`false;`);
	assertEmitted(`class bla{}`);
	assertEmitted(`identifier;`);
	assertEmitted(`'string'`);
	assertEmitted(`0o345;`);
	assertEmitted(`0b010;`);
	assertEmitted(`0xa91;`);
	assertEmitted(`91;`);
	assertEmitted("`template literal`");
	assertEmitted("`template literal ${b=5} ending`");
	assertEmitted("/regex/g;");
}
@("Lexical Declaration")
unittest
{
	assertEmitted(`let a;`);
	assertEmitted(`let a=5;`);
	assertEmitted(`let a,b;`);
	assertEmitted(`let{a,b}=c;`);
	assertEmitted(`let[first,{a,b},...rest]=c;`);
	assertEmitted(`const a;`);
	assertEmitted(`const a=5;`);
	assertEmitted(`const a,b;`);
	assertEmitted(`const{a,b}=c;`);
	assertEmitted(`const[first,{a,b},...rest]=c;`);
}
@("LeftHandSide Expression")
unittest
{
	assertEmitted(`a;`);
	assertEmitted(`super.iden;`);
	assertEmitted(`super[iden];`);
	assertEmitted(`new a;`);
	assertEmitted(`new a();`);
	assertEmitted(`new new a();`);
	assertEmitted(`a().b[c]().d;`);
	assertEmitted("a`template`;");
	assertEmitted("a`template ${b==5} with expression`;");
	assertEmitted(`new a(b,c).b[c](...rest).d;`);
}
@("Assignment Expression")
unittest
{
	assertEmitted(`a=()=>b;`);
	assertEmitted(`a=b?c:d;`);
	assertEmitted(`a=b=c?d:e;`);
	assertEmitted(`a=b=()=>c;`);
}
@("ArrowFunction Expression")
unittest
{
	assertEmitted(`a=>a;`);
	assertEmitted(`()=>a;`);
	assertEmitted(`()=>{a}`);
	assertEmitted(`()=>{if(a)bla()}`);
}
@("Unary Expression")
unittest
{
	assertEmitted(`void 0;`);
	assertEmitted(`var t=arguments.length>1&&void 0!==arguments[1]?arguments[1]:.15;`);
}
@("Binary Expression")
unittest
{
	//assertEmitted(`a instanceof b;`);
	//assertEmitted(`a in b;`);
	//assertEmitted(`'using'in options;`);
	//assertEmitted(`'using'in{b,c,d}`);
	//assertEmitted(`a&b|c&&d||e^f===g==h!==i!=j<=k<l>=m>n<<o>>>p>>q+r-s*t/u%v;`);
	assertEmitted(`'O'+ ++h;`);
	assertEmitted(`'O'-++h;`);
	assertEmitted(`'O'+--h;`);
	assertEmitted(`'O'- --h;`);
	assertEmitted(`'O'+++h;`);
	assertEmitted(`'O'++-h;`);
	assertEmitted(`'O'+++--h;`);
	assertEmitted(`'O'++- --h;`);
	assertEmitted(`'O'+++ ++h;`);
	assertEmitted(`'O'++-++h;`);
}
@("Variable Statement")
unittest
{
	assertEmitted(`var a;`);
	assertEmitted(`var a=b;`);
	assertEmitted(`var[a,b]=c;`);
	assertEmitted(`var[a,{b,c}]=d;`);
	assertEmitted(`var[a,[b,c]]=d;`);
	assertEmitted(`var{a,b}=c;`);
	assertEmitted(`var a=b=c;`);
	assertEmitted(`var a=[b,c]=d;`);
	assertEmitted(`var a={b,c}=d;`);
	assertEmitted(`var a=b,c=d;`);
	assertEmitted(`var a,b=c,[d]=e,{f}=g,h;`);
	assertEmitted(`var{a,b,c=5}=d;`);
	assertEmitted(`var[a,[b]=e]=d;`);
	assertEmitted(`var[a,{b}=e]=d;`);
	assertEmitted(`var[a,b=e]=d;`);
	assertEmitted(`var[a,{b,f}={b:[,,b]={g:t},f},c,k={o:[,,l]}]=d;`);
}
@("If Statement")
unittest
{
	assertEmitted(`if(c){}`);
	assertEmitted(`if(c);`);
	assertEmitted(`if(c,d==4){a(),d();var b}`);
	assertEmitted(`if(a);else;`);
	assertEmitted(`if(a){}else if(b)c;else d;`);
	assertEmitted(`if(a)b=1;else b=2;`);
}
@("Switch Statement")
unittest
{
	assertEmitted(`switch(a){}`);
	assertEmitted(`switch(a){case 4:}`);
	assertEmitted(`switch(a){case 4:case'abd':}`);
	assertEmitted(`switch(a){case 4:break;case 0x5a:default:}`);
}
@("DoWhile Statement")
unittest
{
	assertEmitted(`do a;while(true);`);
	assertEmitted(`do{a}while(true);`);
}
@("While Statement")
unittest
{
	assertEmitted(`while(true)a;`);
	assertEmitted(`while(true){a}`);
}
@("For Statement")
unittest
{
	assertEmitted(`for(var a,b=3;c in d;c++){}`);
	assertEmitted(`for(var a,b=3;;c++)a;`);
	assertEmitted(`for(var[a,b]=c;;)a;`);
	assertEmitted(`for(var a in c){}`);
	assertEmitted(`for(var a of c){}`);
	assertEmitted(`for(var{a,b}in c?d:e){}`);
	assertEmitted(`for(var[a,b]of c||d){}`);
	assertEmitted(`for(let[a,b]=c;a<b;a++){}`);
	assertEmitted(`for(let{a,b}=c;a<b;a++){}`);
	assertEmitted(`for(let a;;a++){}`);
	assertEmitted(`for(let a;;){}`);
	assertEmitted(`for(let{a,b}in c?d:e){}`);
	assertEmitted(`for(let[a,b]of c||d){}`);
	assertEmitted(`for(const[a,b]=c;a<b;a++){}`);
	assertEmitted(`for(const{a,b}=c;a<b;a++){}`);
	assertEmitted(`for(const a;;a++){}`);
	assertEmitted(`for(const a;;){}`);
	assertEmitted(`for(const{a,b}in c?d:e){}`);
	assertEmitted(`for(const[a,b]of c||d){}`);
	assertEmitted(`for(a;b;c){}`);
	assertEmitted(`for(a;;c){}`);
	assertEmitted(`for(a;;){}`);
	assertEmitted(`for(;;){}`);
	assertEmitted(`for(;;){}`);
	assertEmitted(`for(a in b){}`);
	assertEmitted(`for(a of b){}`);
	assertEmitted(`for(a in b,d in f){}`);
	assertEmitted(`for(a of b?d:e){}`);
	assertEmitted(`for([a,c]in'b'){}`);
	assertEmitted(`for([a,c]of'b'){}`);
	assertEmitted(`for({a,c}in'b'){}`);
	assertEmitted(`for({a,c}of'b'){}`);
}
@("With Statement")
unittest
{
	assertEmitted(`with(a)d;`);
	assertEmitted(`with(a){d}`);
}
@("Throw Statement")
unittest
{
	assertEmitted(`throw new error;`);
	assertEmitted(`throw'string';`);
}
@("Try Statement")
unittest
{
	assertEmitted(`try{}`);
	assertEmitted(`try{}catch(exception){}`);
	assertEmitted(`try{}finally{}`);
	assertEmitted(`try{}catch(exception){}finally{}`);
}
@("Debugger Statement")
unittest
{
	assertEmitted(`debugger;`);
}
@("Class Declaration")
unittest
{
	assertEmitted(`class a{}`);
	assertEmitted(`class a extends b{}`);
	assertEmitted(`class a extends b{}`);
	assertEmitted(`class a extends b.bla[b](){}`);
	assertEmitted(`class a{get fun(){return b}}`);
	assertEmitted(`class a{static fun(){return b}}`);
	assertEmitted(`class a{set bla(b){}}`);
	assertEmitted(`class a{static set bla(b){}}`);
	assertEmitted(`class a{bla(b,c=5){}}`);
	assertEmitted(`class a{static bla(b,c=5){}}`);
	assertEmitted(`class a{static bla(b,c=5){}}`);
	assertEmitted(`class a{*boo(b,c=5){}}`);
	assertEmitted(`class a{static *coo(b,c=5){}}`);
	assertEmitted(`class a extends b.bla[b](){get fun(){return b}static fun(){return b}set bla(b){}static set bla(b){}bla(b,c=5){}static bla(b,c=5){}static bla(b,c=5){}*boo(b,c=5){}static *coo(b,c=5){}}`);
}
@("Function Declaration")
unittest
{
	assertEmitted(`function a(){}`);
	assertEmitted(`function a(...rest){}`);
	assertEmitted(`function a(b,[c,d],...rest){}`);
	assertEmitted(`function a(a,b=5){}`);
	assertEmitted(`function a(a,{b,c}){}`);
	assertEmitted(`function*a(a,{b,c}){}`);
	assertEmitted(`function a(a,{b,c}){return}`);
	assertEmitted(`function a(a,{b,c}){return a,b,c}`);
	assertEmitted(`function a(a,{b,c}){return'string'}`);
	assertEmitted(`function a(a,{b,c}){return{a,b,c}}`);
	assertEmitted(`function a(a,{b,c}){return this}`);
	assertEmitted(`function a(a,{b,c}){return null}`);
	assertEmitted(`function a(){}var b;`);
}
@("Prefix Expression")
unittest
{
	assertEmitted(`if('using'in options)bla();`);
	assertEmitted(`!bla;`);
	assertEmitted(`bla<=ops;`);
	assertEmitted(`bla>-1;`);
}
@("Module")
unittest
{
	parse(
		`function
		a
		(
		)
		{
			var
			b
			=
			5;
		 }
		if
		(
		b
		)
		{
			bla
			(
			)
		}
		`).shouldEqual(`function a(){var b=5}if(b){bla()}`);
	// test for single and double quotes strings with singe and double quotes in them, they should be encoded
}






