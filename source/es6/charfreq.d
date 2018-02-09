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

module es6.charfreq;

import es6.nodes;
import es6.tokens;
import std.range : repeat;
import std.algorithm : copy;

import es6.emitter;

version (unittest)
{
	import es6.parser;
	import unit_threaded;
	void assertSortMostUsedLetters(string input, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(input);
		parser.scanToken();
		auto root = parser.parseModule();
		root.charfreq.shouldEqual(expected,file,line);
	}
}
struct CharBin
{
	int[54] bins;
	void put(const(ubyte)[] cs)
	{
		foreach(c; cs)
			put(cast(char)c);
	}
	void put(const(char)[] cs)
	{
		foreach(c; cs)
			put(c);
	}
	void put(string s)
	{
		foreach(char c; s)
			put(c);
	}
	void put(char c)
	{
		if (c >= 'a' && c <= 'z')
			bins[cast(size_t)(c-'a')]++;
		else if (c >= 'A' && c <= 'A')
			bins[26 + cast(size_t)(c-'A')]++;
		else if (c == '$')
			bins[52]++;
		else if (c == '_')
			bins[53]++;
	}
}

string charfreq(Node node)
{
	import std.algorithm : sort, map;
	import std.range : enumerate;
	import std.conv : text;
	import std.range;
	CharBin bins;
	countChars(node, bins);
	return bins.bins[].enumerate.array.sort!((a,b) => a.value > b.value).map!("a.index").map!((i){
		if (i < 26)
			return cast(char)('a'+i);
		if (i < 52)
			return cast(char)('A'+(i-26));
		if (i == 52)
			return '$';
		return '_';
	}).text;
}

void countChars(Node[] nodes, ref CharBin bins)
{
	foreach(c; nodes)
		countChars(c,bins);
}

void countChars(Node node, ref CharBin bins) @trusted
{
	switch(node.type)
	{
		case NodeType.SheBangNode:
			auto n = node.as!SheBangNode;
			bins.put(n.value);
			return;
		case NodeType.BooleanNode:
			auto n = node.as!BooleanNode;
			bins.put(n.value ? "true": "false");
			return;
		case NodeType.StringLiteralNode:
			auto n = node.as!StringLiteralNode;
			bins.put(n.value);
			return;
		case NodeType.BinaryLiteralNode:
			bins.put("b");
			return;
		case NodeType.OctalLiteralNode:
			bins.put("o");
			return;
		case NodeType.DecimalLiteralNode:
			return;
		case NodeType.HexLiteralNode:
			bins.put("x");
			return;
		case NodeType.TemplateNode:
			auto n = node.as!TemplateNode;
			bins.put(n.value);
			return;
		case NodeType.TemplateLiteralNode:
			node.children[0].countChars(bins);
			import std.range : chunks;
			foreach(chunk; node.children[1..$].chunks(2))
			{
				chunk[0].countChars(bins);
				chunk[1].countChars(bins);
			}
			return;
		case NodeType.RegexLiteralNode:
			auto n = node.as!RegexLiteralNode;
			bins.put(n.value);
			return;
		case NodeType.KeywordNode:
			auto n = node.as!KeywordNode;
			switch(n.keyword)
			{
				case Keyword.This: bins.put("this"); break;
				case Keyword.Null: bins.put("null"); break; default: assert(0);
			}
			return;
		case NodeType.IdentifierReferenceNode:
			auto n = node.as!IdentifierReferenceNode;
			bins.put(n.identifier);
			return;
		case NodeType.IdentifierNameNode:
			auto n = node.as!IdentifierNameNode;
			bins.put(n.identifier);
			return;
		case NodeType.ExpressionNode:
			node.children.countChars(bins);
			return;
		case NodeType.ParenthesisNode:
			auto n = node.as!ParenthesisNode;
			n.children.countChars(bins);
			return;
		case NodeType.PrefixExpressionNode:
			auto n = node.as!PrefixExpressionNode;
			switch(n.prefix)
			{
				case Prefix.Delete: bins.put("delete"); return;
				case Prefix.Void: bins.put("void"); return;
				case Prefix.Typeof: bins.put("typeof"); return;
				default: return;
			}
		case NodeType.SuperPropertyNode:
			auto n = node.as!SuperPropertyNode;
			bins.put("super");
			n.children[0].countChars(bins);
			return;
		case NodeType.AccessorNode:
			auto n = node.as!AccessorNode;
			bins.put(n.identifier);
			return;
		case NodeType.NewTargetNode:
			bins.put("new.target");
			return;
		case NodeType.SpreadOperatorNode:
			return;
		case NodeType.ArgumentsNode:
			node.children.countChars(bins);
			return;
		case NodeType.ArrayIndexNode:
			node.children.countChars(bins);
			return;
		case NodeType.NewExpressionNode:
			auto n = node.as!NewExpressionNode;
			"new".repeat(n.news).copy(bins);
			n.children.countChars(bins);
			return;
		case NodeType.CallExpressionNode:
			auto n = node.as!CallExpressionNode;
			"new".repeat(n.news).copy(bins);
			return n.children.countChars(bins);
		case NodeType.UnaryExpressionNode:
			auto n = node.as!UnaryExpressionNode;
			if (n.prefixs.length > 0)
				n.prefixs.countChars(bins);
			n.children[0].countChars(bins);
			return;
		case NodeType.ExpressionOperatorNode:
			auto n = node.as!ExpressionOperatorNode;
			final switch(n.operator)
			{
				case ExpressionOperator.InstanceOf: bins.put(" instanceof"); return;
				case ExpressionOperator.In: bins.put("in"); return;
				case ExpressionOperator.LogicalAnd:
				case ExpressionOperator.LogicalOr:
				case ExpressionOperator.BitwiseAnd:
				case ExpressionOperator.BitwiseOr:
				case ExpressionOperator.BitwiseXor:
				case ExpressionOperator.StrictEqual:
				case ExpressionOperator.Equal:
				case ExpressionOperator.StrictNotEqual:
				case ExpressionOperator.NotEqual:
				case ExpressionOperator.LessOrEqual:
				case ExpressionOperator.LessThan:
				case ExpressionOperator.GreaterOrEqual:
				case ExpressionOperator.GreaterThan:
				case ExpressionOperator.LeftShift:
				case ExpressionOperator.TripleRightSift:
				case ExpressionOperator.RightShift:
				case ExpressionOperator.Add: 
				case ExpressionOperator.Minus:
				case ExpressionOperator.Multiply:
				case ExpressionOperator.Division:
				case ExpressionOperator.Mod:
					break;
			}
			return;
		case NodeType.ConditionalExpressionNode:
			auto n = node.as!ConditionalExpressionNode;
			n.children[0].countChars(bins);
			n.children[1].countChars(bins);
			n.children[2].countChars(bins);
			return;
		case NodeType.AssignmentExpressionNode:
			node.children.countChars(bins);
			return;
		case NodeType.ArrowFunctionNode:
			auto n = node.as!ArrowFunctionNode;
			n.children[0].countChars(bins);
			return n.children[1].countChars(bins);
		case NodeType.AssignmentOperatorNode:
			auto n = node.as!AssignmentOperatorNode;
			return;
		case NodeType.ContinueStatementNode:
			bins.put("continue");
			auto cntStmt = node.as!ContinueStatementNode;
			if (cntStmt.label)
			{
				bins.put(cast(const(char)[])cntStmt.label);
			}
			return;
		case NodeType.BreakStatementNode:
			bins.put("break");
			auto brkStmt = node.as!BreakStatementNode;
			if (brkStmt.label)
			{
				bins.put(cast(const(char)[])brkStmt.label);
			}
			return;
		case NodeType.EmptyStatementNode:
			return;
		case NodeType.LabelledStatementNode:
			auto n = node.as!LabelledStatementNode;
			bins.put(n.label);
			return n.children[0].countChars(bins);
		case NodeType.VariableStatementNode:
			bins.put("var");
			node.children.countChars(bins);
			return;
		case NodeType.VariableDeclarationNode:
			node.children[0].countChars(bins);
			if (node.children.length ==2)
			{
				node.children[1].countChars(bins);
			}
			return;
		case NodeType.ReturnStatementNode:
			bins.put("return");
			if (node.children.length == 1)
				node.children[0].countChars(bins);
			return;
		case NodeType.BlockStatementNode:
			node.children.countChars(bins);
			return;
		case NodeType.IfStatementNode:
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			if (node.children.length > 2)
			{
				bins.put("else");
				node.children[2].countChars(bins);
			}
			return;
		case NodeType.SwitchStatementNode:
			bins.put("switch");
			node.children[0].countChars(bins);
			node.children[1..$].countChars(bins);
			return;
		case NodeType.DoWhileStatementNode:
			bins.put("do");
			node.children[0].countChars(bins);
			bins.put("while");
			node.children[1].countChars(bins);
			return;
		case NodeType.WhileStatementNode:
			bins.put("while");
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.CaseNode:
			bins.put("case");
			node.children[0].countChars(bins);
			if (node.children.length == 2)
				node.children[1].countChars(bins);
			return;
		case NodeType.CaseBodyNode:
			node.children.countChars(bins);
			return;
		case NodeType.DefaultNode:
			bins.put("default");
			node.children.countChars(bins);
			return;
		case NodeType.ForStatementNode:
			auto n = node.as!ForStatementNode;
			bins.put("for");
			switch(n.loopType)
			{
				case ForLoop.ExprCStyle:
				case ForLoop.VarCStyle:
				case ForLoop.ConstCStyle:
				case ForLoop.LetCStyle:
					n.children[0..$-1].countChars(bins);
					break;
				case ForLoop.VarIn:
				case ForLoop.VarOf:
					bins.put("var");
					goto default;
				case ForLoop.ConstIn:
				case ForLoop.ConstOf:
					bins.put("const");
					goto default;
				case ForLoop.LetIn:
				case ForLoop.LetOf:
					bins.put("let");
					goto default;
				default:
					n.children[0].countChars(bins);
					switch (n.loopType)
					{
						case ForLoop.ExprIn:
						case ForLoop.VarIn:
						case ForLoop.ConstIn:
						case ForLoop.LetIn:
							bins.put("in");
							break;
						case ForLoop.ExprOf:
						case ForLoop.VarOf:
						case ForLoop.ConstOf:
						case ForLoop.LetOf:
							bins.put("of");
							break;
						default:
							assert(0);
					}
					n.children[1].countChars(bins);
					break;
			}
			n.children[$-1].countChars(bins);
			return;
		case NodeType.WithStatementNode:
			bins.put("with");
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.CatchStatementNode:
			bins.put("catch");
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.FinallyStatementNode:
			bins.put("finally");
			node.children[0].countChars(bins);
			return;
		case NodeType.TryStatementNode:
			bins.put("try");
			node.children.countChars(bins);
			return;
		case NodeType.ThrowStatementNode:
			bins.put("throw");
			node.children[0].countChars(bins);
			return;
		case NodeType.DebuggerStatementNode:
			bins.put("debugger");
			return;
		case NodeType.ClassDeclarationNode:
			auto n = node.as!ClassDeclarationNode;
			bins.put("class");
			if (n.name !is null)
				n.name.countChars(bins);
			if (n.base !is null)
			{
				bins.put("extends");
				n.base.countChars(bins);
			}
			n.methods.countChars(bins);
			return;
		case NodeType.ClassGetterNode:
			auto n = node.as!ClassGetterNode;
			if (n.isStatic)
				bins.put("static");
			bins.put("get");
			n.children[0].countChars(bins);
			n.children[1].countChars(bins);
			return;
		case NodeType.ClassMethodNode:
			auto n = node.as!ClassMethodNode;
			if (n.isStatic)
				bins.put("static");
			n.children[0].countChars(bins);
			n.children[1].countChars(bins);
			n.children[2].countChars(bins);
			return;
		case NodeType.ClassGeneratorMethodNode:
			auto n = node.as!ClassGeneratorMethodNode;
			if (n.isStatic)
				bins.put("static");
			n.children[0].countChars(bins);
			n.children[1].countChars(bins);
			n.children[2].countChars(bins);
			return;
		case NodeType.ClassSetterNode:
			auto n = node.as!ClassSetterNode;
			if (n.isStatic)
				bins.put("static");
			bins.put("set");
			n.children[0].countChars(bins);
			n.children[1].countChars(bins);
			n.children[2].countChars(bins);
			return;
		case NodeType.ComputedPropertyNameNode:
			node.children[0].countChars(bins);
			return;
		case NodeType.FormalParameterListNode:
			node.children.countChars(bins);
			return;
		case NodeType.FunctionExpressionNode:
		case NodeType.FunctionDeclarationNode:
		case NodeType.GeneratorExpressionNode:
		case NodeType.GeneratorDeclarationNode:
			bins.put("function");
			if (node.children.length == 3)
				node.children[0].countChars(bins);
			node.children[$-2..$].countChars(bins);
			return;
		case NodeType.RestElementNode:
		case NodeType.SpreadElementNode:
			node.children[0].countChars(bins);
			return;
		case NodeType.SingleNameBindingNode:
			node.children.countChars(bins);
			return;
		case NodeType.ArrayLiteralNode:
			node.children.countChars(bins);
			return;
		case NodeType.ObjectLiteralNode:
			node.children.countChars(bins);
			return;
		case NodeType.PropertyDefinitionNode:
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.CoverInitializedName:
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.ElisionNode:
			return;
		case NodeType.FunctionBodyNode:
			if (node.entersStrictMode && (node.parent.parent is null || node.parent.parent.type != NodeType.ClassDeclarationNode))
				bins.put(`use strict`);
			node.children.countChars(bins);
			return;
		case NodeType.LexicalDeclarationItemNode:
			node.children[0].countChars(bins);
			if (node.children.length == 2)
			{
				node.children[1].countChars(bins);
			}
			return;
		case NodeType.LexicalDeclarationNode:
			auto n = node.as!LexicalDeclarationNode;
			if (n.declaration == LexicalDeclaration.Let)
				bins.put("let");
			else
				bins.put("const");
			node.children.countChars(bins);
			return;
		case NodeType.ArrayBindingPatternNode:
			node.children.countChars(bins);
			return;
		case NodeType.ObjectBindingPatternNode:
			node.children.countChars(bins);
			return;
		case NodeType.BindingPropertyNode:
			node.children[0].countChars(bins);
			node.children[1].countChars(bins);
			return;
		case NodeType.ExportClauseNode:
			node.children.countChars(bins);
			return;
		case NodeType.ExportDeclarationNode:
			bins.put("export");
			node.children[0].countChars(bins);
			if (node.children.length == 2)
			{
				bins.put("from");
				node.children[1].countChars(bins);
			}
			return;
		case NodeType.ExportDefaultDeclarationNode:
			bins.put("exportdefault");
			node.children[0].countChars(bins);
			return;
		case NodeType.ExportSpecifierNode:
			node.children[0].countChars(bins);
			bins.put("as");
			node.children[1].countChars(bins);
			return;
		case NodeType.ImportClauseNode:
			node.children.countChars(bins);
			return;
		case NodeType.ImportDeclarationNode:
			bins.put("import");
			node.children[0].countChars(bins);
			if (node.children.length == 2)
			{
				bins.put("from");
				node.children[1].countChars(bins);
			}
			return;
		case NodeType.ImportSpecifierNode:
			node.children[0].countChars(bins);
			bins.put("as");
			node.children[1].countChars(bins);
			return;
		case NodeType.NamedImportsNode:
			node.children.countChars(bins);
			return;
		case NodeType.NameSpaceImportNode:
			bins.put("as");
			node.children[0].countChars(bins);
			return;
		case NodeType.ModuleNode:
			if (node.entersStrictMode)
				bins.put(`use strict`);
			node.children.countChars(bins);
			return;
		case NodeType.SemicolonNode:
			return;
		case NodeType.BindingElementNode:
			node.children.countChars(bins);
			return;
		default: break;
	}
	node.children.countChars(bins);
}

@("charfreq")
unittest
{
	assertSortMostUsedLetters(
		`function b(){ var B = "_$$hup"; switch(b){case 5: return function(){}}};`,
		"nctuirabefhos$pvw_dgjklmqxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	);
	assertSortMostUsedLetters(
		"function b(){ var b = function() {return 6;}; switch(6) { case 4: break; default: return 5 }};",
		"nrteuacbfiosdhklvwgjmpqxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$_"
	);
}
