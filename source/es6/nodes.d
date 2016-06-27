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
module es6.nodes;

import es6.tokens;
import es6.scopes;

import std.range : lockstep;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import es6.analyse;
	import unit_threaded;
	import std.stdio;
}

enum Keyword
{
	This,
	Null
}
enum Prefix {
	Delete,
	Void,
	Typeof,
	Increment,
	Decrement,
	Positive,
	Negative,
	Tilde,
	Negation
}
enum Postfix {
	None,
	Increment,
	Decrement
}
enum ExpressionOperator {
	InstanceOf,
	In,
	LogicalAnd,
	LogicalOr,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	StrictEqual,
	Equal,
	StrictNotEqual,
	NotEqual,
	LessOrEqual,
	LessThan,
	GreaterOrEqual,
	GreaterThan,
	LeftShift,
	TripleRightSift,
	RightShift,
	Add,
	Minus,
	Multiply,
	Division,
	Mod
}
enum NodeType {
	ErrorNode,
	BooleanNode,
	StringLiteralNode,
	BinaryLiteralNode,
	OctalLiteralNode,
	DecimalLiteralNode,
	HexLiteralNode,
	TemplateNode,
	TemplateLiteralNode,
	RegexLiteralNode,
	KeywordNode,
	IdentifierNode,
	ExpressionNode,
	ParenthesisNode,
	PrefixExpressionNode,
	SuperPropertyNode,
	AccessorNode,
	NewTargetNode,
	SpreadOperatorNode,
	ArgumentsNode,
	ArrayIndexNode,
	NewExpressionNode,
	CallExpressionNode,
	UnaryExpressionNode,
	ExpressionOperatorNode,
	BinaryExpressionNode,
	ConditionalExpressionNode,
	AssignmentExpressionNode,
	ArrowFunctionNode,
	AssignmentOperatorNode,
	ContinueStatementNode,
	BreakStatementNode,
	EmptyStatementNode,
	LabelledStatementNode,
	VariableDeclarationNode,
	VariableStatementNode,
	ReturnStatementNode,
	BlockStatementNode,
	IfStatementNode,
	SwitchStatementNode,
	DoWhileStatementNode,
	WhileStatementNode,
	CaseNode,
	DefaultNode,
	ForStatementNode,
	WithStatementNode,
	CatchStatementNode,
	FinallyStatementNode,
	TryStatementNode,
	ThrowStatementNode,
	DebuggerStatementNode,
	ClassDeclarationNode,
	ClassGetterNode,
	ClassMethodNode,
	ClassGeneratorMethodNode,
	ClassSetterNode,
	ComputedPropertyNameNode,
	FormalParameterListNode,
	FunctionDeclarationNode,
	FunctionExpressionNode,
	GeneratorDeclarationNode,
	GeneratorExpressionNode,
	RestElementNode,
	SingleNameBindingNode,
	SpreadElementNode,
	ArrayLiteralNode,
	ObjectLiteralNode,
	PropertyDefinitionNode,
	CoverInitializedName,
	ElisionNode,
	FunctionBodyNode,
	LexicalDeclarationItemNode,
	LexicalDeclarationNode,
	ArrayBindingPatternNode,
	ObjectBindingPatternNode,
	BindingPropertyNode,
	ExportClauseNode,
	ExportDeclarationNode,
	ExportDefaultDeclarationNode,
	ExportSpecifierNode,
	ImportClauseNode,
	ImportDeclarationNode,
	ImportSpecifierNode,
	NamedImportsNode,
	NameSpaceImportNode,
	ModuleNode,
	SemicolonNode,
	BindingElementNode
}
enum Assignment {
	LeftShiftAssignment,
	TripleRightShiftAssignment,
	RightShiftAssignment,
	Assignment,
	AdditiveAssignment,
	DecrementalAssignment,
	MultiplicativeAssignment,
	DivisionAssignment,
	ModAssignment,
	BitwiseAndAssignment,
	BitwiseOrAssignment,
	BitwiseXorAssignment
}
enum ForLoop
{
	ExprIn,
	ExprOf,
	VarIn,
	VarOf,
	ConstIn,
	ConstOf,
	LetIn,
	LetOf,
	ExprCStyle,
	VarCStyle,
	ConstCStyle,
	LetCStyle
}
enum LexicalDeclaration
{
	Let,
	Const
}
enum Hint {
	None = 0,
	Return = 1 << 0,
	ReturnValue = 1 << 1,
	NonExpression = 1 << 2
}
struct Hints
{
	private int hs = Hint.None;
	alias hs this;
	this(int h)
	{
		hs = h;
	}
	void toString(scope void delegate(const(char)[]) sink) const
	{
		import std.traits : EnumMembers;
		import std.conv : to;
		bool emitDelimiter = false;
		foreach(m; EnumMembers!Hint)
		{
			if (hs & m)
			{
				if (emitDelimiter)
					sink(", ");
				sink(m.to!string);
				emitDelimiter = true;
			}
		}
	}
	int get() { return hs; }
	bool has(int v)
	{
		return (hs & v) != 0;
	}
}
struct PrettyPrintSink
{
	private void delegate(const(char)[]) sink;
	private char[1] chr;
	this(void delegate(const(char)[]) sink)
	{
		this.sink = sink;
	}
	void put(string s)
	{
		sink(s);
	}
	void put(char c)
	{
		chr[0] = c;
		sink(chr);
	}
	void indent(int indent)
	{
		import std.range : repeat;
		import std.algorithm : copy;
		" ".repeat(indent*2).copy(sink);
	}
	void print(const Node[] children, int level)
	{
		foreach(c; children)
		{
			assert(c !is null);
			c.prettyPrint(this,level);
		}
	}
}
import std.format;
class Node
{
	Branch branch;
	Node[] children;
	Node parent;
	private NodeType _type;
	@property NodeType type() const { return _type; }
	private Hints _hints;
	@property Hints hints() { return _hints; }
	@property void hints(int hs) { _hints = Hints(hs); }
	this(NodeType t)
	{
		_type = t;
	}
	this(NodeType t, Node n)
	{
		_type = t;
		if (n !is null)
		{
			n.parent = this;
			children = [n];
		}
	}
	this(NodeType t, Node[] cs)
	{
		_type = t;
		import std.algorithm :each;
		foreach(c;cs)
			c.parent = this;
		children = cs;
	}
	void toString(scope void delegate(const(char)[]) sink) const
	{
		prettyPrint(PrettyPrintSink(sink));
	}
	void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s",_type);
		if (_hints != Hint.None)
			sink.formattedWrite(" %s",_hints);
		sink.formattedWrite("\n");
		sink.print(children,level+1);
	}
	auto as(Type)(in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format;
		auto r = cast(Type)this;
		version (unittest)
		{
			if (r is null)
				throw new UnitTestException([format("Tried to interpret as %s, but got %s",Type.stringof,this)],file,line);
		}
		assert(r !is null,format("Assertion this != %s. In %s @ %s",Type.stringof,file,line));
		return r;
	}
	// this won't diff the children
	Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		return Diff.No;
	}
	Node getRoot()
	{
		if (branch)
			return branch.scp.getRoot();
		return this; // todo: could also return parent's getRoot but this is probably pointless...
	}
	void replace(Node other)
	{
		assert(parent !is null);
		assert(other !is null);
		this.parent.replaceChild(this,other);
	}
	void replaceChild(Node child, Node other)
	{
		import std.algorithm : countUntil;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		children[idx] = other;
		other.parent = this;
	}
}

class ErrorNode : Node
{
	string value;
	size_t line;
	size_t column;
	string debugMessage;
	this(string v, size_t l, size_t c, string debugMsg = "")
	{
		value = v;
		column = c;
		line = l;
		debugMessage = debugMsg;
		super(NodeType.ErrorNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("Error: %s at line %s at column %s\n",value,line,column);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!ErrorNode();
		return o.value == value && o.line == line && o.column == column ? Diff.No : Diff.Content;
	}
}
class BooleanNode : Node
{
	bool value;
	this(bool v)
	{
		value = v;
		super(NodeType.BooleanNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class StringLiteralNode : Node
{
	string value; // all strings are normalized to single quoted string (meaning original double quoted strings are properly unescaped)
	this(string v)
	{
		value = v;
		super(NodeType.StringLiteralNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s \"%s\"\n",_type,value);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class BinaryLiteralNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.BinaryLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class OctalLiteralNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.OctalLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class DecimalLiteralNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.DecimalLiteralNode);
	}
	override string toString()
	{
		return "DecimalLiteralNode ("~value~")";
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class HexLiteralNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.HexLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class TemplateLiteralNode : Node
{
	this(TemplateNode n)
	{
		super(NodeType.TemplateLiteralNode,[n]);
	}
	this(Node[] children)
	{
		super(NodeType.TemplateLiteralNode,children);
	}
}
class TemplateNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.TemplateNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,value);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class RegexLiteralNode : Node
{
	string value;
	this(string v)
	{
		value = v;
		super(NodeType.RegexLiteralNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,value);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
class KeywordNode : Node
{
	Keyword keyword;
	this(Keyword k)
	{
		keyword = k;
		super(NodeType.KeywordNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.keyword == keyword ? Diff.No : Diff.Content;
	}
}

class IdentifierNode : Node
{
	string identifier;
	this(string i)
	{
		identifier = i;
		super(NodeType.IdentifierNode);
	}
	override string toString()
	{
		return "IdentifierNode (\""~identifier~"\")";
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,identifier);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.identifier == identifier ? Diff.No : Diff.Content;
	}
}
class ExpressionNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ExpressionNode,children);
	}
	bool isSingleExpression()
	{
		return children.length < 2;
	}
}
class ParenthesisNode : Node
{
	this()
	{
		super(NodeType.ParenthesisNode);
	}
	this(Node child)
	{
		super(NodeType.ParenthesisNode,child);
	}
	this(Node[] children)
	{
		super(NodeType.ParenthesisNode,children);
	}
}

class PrefixExpressionNode : Node
{
	Prefix prefix;
	this(Prefix p)
	{
		super(NodeType.PrefixExpressionNode);
		prefix = p;
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,prefix);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.prefix == prefix ? Diff.No : Diff.Content;
	}
}
class SuperPropertyNode : Node
{
	this(Node n)
	{
		super(NodeType.SuperPropertyNode,[n]);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s super\n",type);
		sink.print(children,level+1);
	}
}
class AccessorNode : Node
{
	string identifier;
	this(string i)
	{
		identifier = i;
		super(NodeType.AccessorNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,identifier);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.identifier == identifier ? Diff.No : Diff.Content;
	}
}
class NewTargetNode : Node
{
	this()
	{
		super(NodeType.NewTargetNode);
	}
}
class SpreadOperatorNode : Node
{
	this()
	{
		super(NodeType.SpreadOperatorNode);
	}
}
class ArgumentsNode : Node
{
	this()
	{
		super(NodeType.ArgumentsNode);
	}
	this(Node[] args)
	{
		super(NodeType.ArgumentsNode,args);
	}
}
class ArrayIndexNode : Node
{
	this(Node expr)
	{
		super(NodeType.ArrayIndexNode,[expr]);
	}
}
class NewExpressionNode : Node
{
	size_t news;
	this(size_t news, Node[] calls)
	{
		this.news = news;
		super(NodeType.NewExpressionNode,calls);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.news == news ? Diff.No : Diff.Content;
	}
}
class CallExpressionNode : Node
{
	size_t news;
	this(size_t news, Node[] calls)
	{
		this.news = news;
		super(NodeType.CallExpressionNode,calls);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.news == news ? Diff.No : Diff.Content;
	}
}
class UnaryExpressionNode : Node
{
	Node[] prefixs;
	Postfix postfix = Postfix.None;
	this(Node[] ps, Node n)
	{
		prefixs = ps;
		super(NodeType.UnaryExpressionNode,n);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		if (prefixs.length != o.prefixs.length)
			return Diff.Content;
		foreach(a,b; lockstep(prefixs,o.prefixs))
		{
			auto d = a.diff(b);
			if (d != Diff.No)
				return d;
		}
		return o.postfix == postfix ? Diff.No : Diff.Content;
	}
}
class ExpressionOperatorNode : Node
{
	ExpressionOperator operator;
	this(ExpressionOperator op)
	{
		operator = op;
		super(NodeType.ExpressionOperatorNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.operator == operator ? Diff.No : Diff.Content;
	}
}
class BinaryExpressionNode : Node
{
	this(Node[] children)
	{
		super(NodeType.BinaryExpressionNode,children);
	}
}
class ConditionalExpressionNode : Node
{
	this(Node cond, Node truthPath, Node elsePath)
	{
		super(NodeType.ConditionalExpressionNode,[cond,truthPath,elsePath]);
	}
}
class AssignmentExpressionNode : Node
{
	this(Node[] children)
	{
		super(NodeType.AssignmentExpressionNode,children);
	}
}
void removeFirstAssignment(AssignmentExpressionNode node)
{
	if (node.children.length == 3)
		node.replace(node.children[2]);
	else
		node.children = node.children[2..$];
}
class ArrowFunctionNode : Node
{
	this(Node parameter, Node functionBody)
	{
		super(NodeType.ArrowFunctionNode,[parameter,functionBody]);
	}
}
class AssignmentOperatorNode : Node
{
	Assignment assignment;
	this(Assignment a)
	{
		assignment = a;
		super(NodeType.AssignmentOperatorNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.assignment == assignment ? Diff.No : Diff.Content;
	}
}
class ContinueStatementNode : Node
{
	this()
	{
		super(NodeType.ContinueStatementNode);
	}
}
class BreakStatementNode : Node
{
	this()
	{
		super(NodeType.BreakStatementNode);
	}
}
class EmptyStatementNode : Node
{
	this()
	{
		super(NodeType.EmptyStatementNode);
	}
}
class LabelledStatementNode : Node
{
	string label;
	this(string l)
	{
		label = l;
		super(NodeType.LabelledStatementNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.label == label ? Diff.No : Diff.Content;
	}
}
class VariableDeclarationNode : Node
{
	this(Node lhs, Node init = null)
	{
		if (init is null)
			super(NodeType.VariableDeclarationNode,[lhs]);
		else
			super(NodeType.VariableDeclarationNode,[lhs,init]);
	}
}
class VariableStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.VariableStatementNode,children);
	}
}
class ReturnStatementNode : Node
{
	this(Node expr = null)
	{
		super(NodeType.ReturnStatementNode,expr);
	}
}
class BlockStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.BlockStatementNode,children);
	}
}
struct IfPath
{
	Node node;
	alias node this;
	this(Node n)
	{
		node = n;
	}
	Node getLastStatement()
	{
		if (node.type != NodeType.BlockStatementNode)
			return node;
		return node.children[$-1];
	}
	Node convertToAssignmentExpression()
	{
		if (node.type == NodeType.BlockStatementNode)
			return node.as!(BlockStatementNode).convertToAssignmentExpression();
		return node.convertToAssignmentExpression();
	}
}
Node convertToAssignmentExpression(BlockStatementNode node)
{
	import std.array : array;
	import std.algorithm : each;
	import std.array : appender;
	version (unittest) if (node.hints.has(Hint.NonExpression)) throw new UnitTestException(["Cannot convert non-expressions"]);
	assert(!node.hints.has(Hint.NonExpression));
	if (node.children.length == 1)
		return convertToAssignmentExpression(node.children[0]);
	auto app = appender!(Node[]);
	node.children.each!((Node child){
		if (child.type == NodeType.ExpressionNode)
		{
			child.children.each!(c=>app.put(c));
		} else
			app.put(child);
	});
	return new ParenthesisNode(new ExpressionNode(app.data));
}
Node convertToAssignmentExpression(Node node)
{
	if (node.type == NodeType.ExpressionNode)
		return new ParenthesisNode(node);
	version (unittest) if (node.hints.has(Hint.NonExpression)) throw new UnitTestException(["Cannot convert non-expressions"]);
	assert(!node.hints.has(Hint.NonExpression));
	return node;
}
@("convertToAssignmentExpression")
unittest
{
	void assertConvertBlockStatementToAssignmentExpression(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto node = parseNode!("parseBlockStatement",BlockStatementNode)(input);
		auto assignExpr = node.convertToAssignmentExpression();
		assignExpr.emit().shouldEqual(output,file,line);
	}
	assertConvertBlockStatementToAssignmentExpression(
		`{ d = 5; b = 6 }`,
		`(d=5,b=6)`
	);
	assertConvertBlockStatementToAssignmentExpression(
		`{ d = 5 }`,
		`d=5`
	);
	assertConvertBlockStatementToAssignmentExpression(
		`{ for(;;) ; }`,
		`d=5`
	).shouldThrow();
	assertConvertBlockStatementToAssignmentExpression(
		`{ d = 5; b = 6, e = 5 }`,
		`(d=5,b=6,e=5)`
	);
}
class IfStatementNode : Node
{
	this(Node cond, Node truth, Node falsy = null)
	{
		if (falsy is null)
			super(NodeType.IfStatementNode,[cond,truth]);
		else
			super(NodeType.IfStatementNode,[cond,truth,falsy]);
	}
	bool hasElsePath() { return children.length == 3; }
	IfPath truthPath() { return IfPath(children[1]); }
	IfPath elsePath() { assert(hasElsePath); return IfPath(children[2]); }
	Node condition() { return children[0]; }
}
class SwitchStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.SwitchStatementNode,children);
	}
}
class DoWhileStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.DoWhileStatementNode,children);
	}
}
class WhileStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.WhileStatementNode,children);
	}
}
class CaseNode : Node
{
	this(Node[] children)
	{
		super(NodeType.CaseNode,children);
	}
}
class DefaultNode : Node
{
	this(Node[] children)
	{
		super(NodeType.DefaultNode,children);
	}
}
class ForStatementNode : Node
{
	ForLoop loopType;
	this(ForLoop l, Node[] children)
	{
		loopType = l;
		super(NodeType.ForStatementNode,children);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("ForStatement %s\n",loopType);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.loopType == loopType ? Diff.No : Diff.Content;
	}
}
bool isEitherA(Ts...)(Node node)
{
	static if (Ts.length == 1)
		return node.type == Ts[0];
	else
		return node.type == Ts[0] || node.isEitherA!(Ts[1..$]);
}
class WithStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.WithStatementNode,children);
	}
}
class CatchStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.CatchStatementNode,children);
	}
}
class FinallyStatementNode : Node
{
	this(Node block)
	{
		super(NodeType.FinallyStatementNode,[block]);
	}
}
class TryStatementNode : Node
{
	this(Node[] children)
	{
		super(NodeType.TryStatementNode,children);
	}
}
class ThrowStatementNode : Node
{
	this(Node expr)
	{
		super(NodeType.ThrowStatementNode,[expr]);
	}
}
class DebuggerStatementNode : Node
{
	this()
	{
		super(NodeType.DebuggerStatementNode);
	}
}
class ClassDeclarationNode : Node
{
	Node name;
	Node base;
	Node[] methods;
	this(Node name, Node base, Node[] methods)
	{
		this.name = name;
		this.base = base;
		this.methods = methods;
		super(NodeType.ClassDeclarationNode);
		children ~= name;
		if (base !is null)
			children ~= base;
		children ~= methods;
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		return Diff.No;
	}
}
class ClassGetterNode : Node
{
	bool isStatic;
	this(bool isStatic, Node name, Node funcBody)
	{
		this.isStatic = isStatic;
		super(NodeType.ClassGetterNode,[name,funcBody]);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
class ClassMethodNode : Node
{
	bool isStatic;
	this(bool isStatic, Node name, Node params, Node funcBody)
	{
		this.isStatic = isStatic;
		super(NodeType.ClassMethodNode,[name,params,funcBody]);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
class ClassGeneratorMethodNode : Node
{
	bool isStatic;
	this(bool isStatic, Node name, Node params, Node funcBody)
	{
		this.isStatic = isStatic;
		super(NodeType.ClassGeneratorMethodNode,[name,params,funcBody]);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
class ClassSetterNode : Node
{
	bool isStatic;
	this(bool isStatic, Node name, Node param, Node funcBody)
	{
		this.isStatic = isStatic;
		super(NodeType.ClassSetterNode,[name,param,funcBody]);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
class ComputedPropertyNameNode : Node
{
	this(Node expr)
	{
		super(NodeType.ComputedPropertyNameNode,[expr]);
	}
}
class FormalParameterListNode : Node
{
	this(Node[] children)
	{
		super(NodeType.FormalParameterListNode,children);
	}
}
class FunctionDeclarationNode : Node
{
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.FunctionDeclarationNode,[params,funcBody]);
		else
			super(NodeType.FunctionDeclarationNode,[name,params,funcBody]);
	}
}
class FunctionExpressionNode : Node
{
	this(Node name, Node params, Node funcBody)
	{
		if (name !is null)
			super(NodeType.FunctionExpressionNode,[name,params,funcBody]);
		else
			super(NodeType.FunctionExpressionNode,[params,funcBody]);
	}
}
class GeneratorDeclarationNode : Node
{
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.GeneratorDeclarationNode,[params,funcBody]);
		else
			super(NodeType.GeneratorDeclarationNode,[name,params,funcBody]);
	}
}
class GeneratorExpressionNode : Node
{
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.GeneratorExpressionNode,[params,funcBody]);
		else
			super(NodeType.GeneratorExpressionNode,[name,params,funcBody]);
	}
}
class RestElementNode : Node
{
	this(Node iden)
	{
		super(NodeType.RestElementNode,[iden]);
	}
}
class SingleNameBindingNode : Node
{
	this(Node name, Node expr)
	{
		super(NodeType.SingleNameBindingNode,[name,expr]);
	}
}
class SpreadElementNode : Node
{
	this(Node iden)
	{
		super(NodeType.SpreadElementNode,[iden]);
	}
}
class ArrayLiteralNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ArrayLiteralNode,children);
	}
}
class ObjectLiteralNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ObjectLiteralNode,children);
	}
}
class PropertyDefinitionNode : Node
{
	this(Node name, Node expr)
	{
		super(NodeType.PropertyDefinitionNode,[name,expr]);
	}
}
class CoverInitializedName : Node
{
	this(Node iden, Node init)
	{
		super(NodeType.CoverInitializedName,[iden,init]);
	}
}
class ElisionNode : Node
{
	int cnt;
	this(int cnt)
	{
		this.cnt = cnt;
		super(NodeType.ElisionNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("ElisionNode %s\n",cnt);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.cnt == cnt ? Diff.No : Diff.Content;
	}
}
class FunctionBodyNode : Node
{
	this(Node[] children)
	{
		super(NodeType.FunctionBodyNode,children);
	}
}
class LexicalDeclarationNode : Node
{
	LexicalDeclaration declaration;
	this(LexicalDeclaration decl, Node[] children)
	{
		declaration = decl;
		super(NodeType.LexicalDeclarationNode,children);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		auto o = other.as!(typeof(this));
		return o.declaration == declaration ? Diff.No : Diff.Content;
	}
}
class LexicalDeclarationItemNode : Node
{
	this(Node lhs, Node init = null)
	{
		if (init is null)
			super(NodeType.LexicalDeclarationItemNode,[lhs]);
		else
			super(NodeType.LexicalDeclarationItemNode,[lhs,init]);
	}
}
class ArrayBindingPatternNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ArrayBindingPatternNode,children);
	}
}
class ObjectBindingPatternNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ObjectBindingPatternNode,children);
	}
}
class BindingPropertyNode : Node
{
	this(Node name, Node elem)
	{
		super(NodeType.BindingPropertyNode,[name,elem]);
	}
}
class ExportClauseNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ExportClauseNode,children);
	}
}
class ExportDeclarationNode : Node
{
	this(Node clause, Node specifier = null)
	{
		if (specifier is null)
			super(NodeType.ExportDeclarationNode,clause);
		else
			super(NodeType.ExportDeclarationNode,[clause,specifier]);
	}
}
class ExportDefaultDeclarationNode : Node
{
	this(Node child)
	{
		super(NodeType.ExportDefaultDeclarationNode,child);
	}
}
class ExportSpecifierNode : Node
{
	this(Node name, Node as)
	{
		super(NodeType.ExportSpecifierNode,[name,as]);
	}
}
class ImportClauseNode : Node
{
	this(Node clause, Node imports)
	{
		super(NodeType.ImportClauseNode,[clause,imports]);
	}
}
class ImportDeclarationNode : Node
{
	this(Node string)
	{
		super(NodeType.ImportDeclarationNode,[string]);
	}
	this(Node clause, Node string)
	{
		super(NodeType.ImportDeclarationNode,[clause,string]);
	}
}
class ImportSpecifierNode : Node
{
	this(Node name, Node identifier)
	{
		super(NodeType.ImportSpecifierNode,[name,identifier]);
	}
}
class NamedImportsNode : Node
{
	this(Node[] children)
	{
		super(NodeType.NamedImportsNode,children);
	}
}
class NameSpaceImportNode : Node
{
	this(Node child)
	{
		super(NodeType.NameSpaceImportNode,child);
	}
}
class ModuleNode : Node
{
	this(Node[] children)
	{
		super(NodeType.ModuleNode,children);
	}
}
class SemicolonNode : Node
{
	this()
	{
		super(NodeType.SemicolonNode);
	}
}
class BindingElementNode : Node
{
	this(Node pattern, Node init)
	{
		super(NodeType.BindingElementNode,[pattern,init]);
	}
}
ErrorNode[] collectErrors(Node root)
{
	import std.array : appender, Appender;
	auto a = appender!(ErrorNode[]);
	void walk(Node root, Appender!(ErrorNode[]) a)
	{
		if (root.type == NodeType.ErrorNode)
			a.put(root.as!ErrorNode);
		else
			foreach(c; root.children)
				walk(c,a);
	}
	walk(root,a);
	return a.data;
}
enum Diff
{
	No,
	Type,
	Children,
	Content,
	Branch
}
struct DiffResult
{
	Node a;
	Node b;
	Diff type;
}
void assertTreeInternals(Node a, in string file = __FILE__, in size_t line = __LINE__)
{
	version(unittest)
		if (a.branch is null)
			throw new UnitTestException([format("Node has no branch.\n%s",a)],file,line);
	assert(a.branch !is null);
	foreach(c; a.children)
	{
		assert(c.parent is a);
		assertTreeInternals(c);
	}
}
DiffResult diffBranch(Branch a, Branch b)
{
	import std.range : lockstep;
	if (a.children.length != b.children.length)
		return DiffResult(a.entry,b.entry,Diff.Branch);
	foreach(ca,cb; lockstep(a.children,b.children))
	{
		auto r = diffBranch(ca,cb);
		if (r.type != Diff.No)
			return r;
	}
	return DiffResult(a.entry,b.entry,Diff.No);

}
DiffResult diffTree(Node a, Node b)
{
	import std.range : lockstep;
	auto r = diffBranch(a.branch,b.branch);
	if (r.type != Diff.No)
		return r;
	if (a.type != b.type)
		return DiffResult(a,b,Diff.Type);
	if (a.children.length != b.children.length)
		return DiffResult(a,b,Diff.Children);
	foreach(ca,cb; lockstep(a.children,b.children))
	{
		auto r = diffTree(ca,cb);
		if (r.type != Diff.No)
			return r;
		auto d = ca.diff(cb);
		if (d != Diff.No)
			return DiffResult(ca,cb,d);
	}
	return DiffResult(a,b,Diff.No);
}
@("diffTree")
unittest
{
	import std.format;
	Node n = parseModule("true;\"s\";0b01;0o01;10;0x01;`t`;/regex/;null;identifier;!expr;obj.a;new a;a();a+b;c=d;bla:;for(;;);class b{get x(){}set x(a){}method(){}*gen(){}}[,,a]=b;let b=d;");
	diffTree(n,n).type.shouldEqual(Diff.No);
	format("%s",n).shouldEqual("ModuleNode
  BooleanNode
  StringLiteralNode \"s\"
  BinaryLiteralNode
  OctalLiteralNode
  DecimalLiteralNode
  HexLiteralNode
  TemplateLiteralNode
    TemplateNode t
  RegexLiteralNode /regex/
  KeywordNode
  IdentifierNode identifier
  UnaryExpressionNode
    IdentifierNode expr
  CallExpressionNode
    IdentifierNode obj
    AccessorNode a
  NewExpressionNode
    IdentifierNode a
  CallExpressionNode
    IdentifierNode a
    ArgumentsNode
  BinaryExpressionNode
    IdentifierNode a
    ExpressionOperatorNode
    IdentifierNode b
  AssignmentExpressionNode
    IdentifierNode c
    AssignmentOperatorNode
    IdentifierNode d
  LabelledStatementNode
  ForStatement ExprCStyle
    SemicolonNode
    SemicolonNode
    EmptyStatementNode
  ClassDeclarationNode
    IdentifierNode b
    ClassGetterNode
      IdentifierNode x
      FunctionBodyNode
    ClassSetterNode
      IdentifierNode x
      IdentifierNode a
      FunctionBodyNode
    ClassMethodNode
      IdentifierNode method
      FormalParameterListNode
      FunctionBodyNode
    ClassGeneratorMethodNode
      IdentifierNode gen
      FormalParameterListNode
      FunctionBodyNode
  AssignmentExpressionNode
    ArrayLiteralNode
      ElisionNode 2
      IdentifierNode a
    AssignmentOperatorNode
    IdentifierNode b
  LexicalDeclarationNode
    LexicalDeclarationItemNode
      IdentifierNode b
      IdentifierNode d
");
}
bool startsNewScope(Node node)
{
	switch (node.type)
	{
		case NodeType.FunctionDeclarationNode:
		case NodeType.FunctionExpressionNode:
		case NodeType.GeneratorDeclarationNode:
		case NodeType.GeneratorExpressionNode:
		case NodeType.ArrowFunctionNode:
		case NodeType.ClassGetterNode:
		case NodeType.ClassMethodNode:
		case NodeType.ClassGeneratorMethodNode:
		case NodeType.ClassSetterNode:
			return true;
		default:
			return false;
	}
}