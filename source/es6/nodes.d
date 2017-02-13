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
import std.format : formattedWrite, format;
import std.algorithm : each, countUntil, map;
import std.range : lockstep, stride, take;
import std.array : array, insertInPlace;
import option;
import es6.utils;
import es6.lexer : Keyword;
import es6.analyse;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import std.stdio;
}

version(chatty)
{
	import std.stdio;
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
	IdentifierReferenceNode,
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
	CaseBodyNode,
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
	NonExpression = 1 << 2,
	Or = 1 << 3,
	HasAssignment = 1 << 4
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
	int prefixIndent = 0;
	this(void delegate(const(char)[]) sink, int indent = 0)
	{
		this.sink = sink;
		prefixIndent = indent;
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
		" ".repeat((prefixIndent+indent)*2).copy(sink);
	}
	void print(Child)(const Child[] children, int level)
	{
		foreach(c; children)
		{
			assert(c !is null);
			c.prettyPrint(this,level);
		}
	}
}
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
		version(chatty) { writeln(t, this); }
	}
	this(NodeType t, Node n)
	{
		_type = t;
		if (n !is null)
		{
			n.parent = this;
			children = [n];
		}
		version(chatty) { writeln(t, this); }
	}
	this(NodeType t, Node[] cs)
	{
		_type = t;
		import std.algorithm :each;
		foreach(c;cs)
			c.parent = this;
		children = cs;
		version(chatty) { writeln(t, this); }
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
		auto r = cast(Type)this;
		version (unittest)
		{
			import std.format;
			if (r is null)
				throw new UnitTestException([format("Tried to interpret as %s, but got %s",Type.stringof,this)],file,line);
		}
		assert(r !is null,format("Assertion this != %s. In %s @ %s",Type.stringof,file,line));
		return r;
	}
	auto opt(Type)()
	{
		auto r = cast(Type)this;
		if (r is null)
			return None!Type;
		return Some(r);
	}
	// this won't diff the children
	Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if (other.hints != hints)
			return Diff.Hints;
		return Diff.No;
	}
	Node getRoot()
	{
		if (branch)
			return branch.scp.getRoot();
		return this; // todo: could also return parent's getRoot but this is probably pointless...
	}
	Node replaceWith(Node other)
	{
		if (other is this)
			return other;
		assert(other !is null);
		if (parent !is null)
			this.parent.replaceChild(this,other);
		else if (other.branch != this.branch)
			other.assignBranch(this.branch);

		if (this.branch !is null && this.branch.entry is this)
		{
			this.branch.entry = other;
			assert(other.branch is this.branch);
		}
		return other;
	}
	void replaceChild(Node child, Node other)
	{
		import std.algorithm : countUntil;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		children[idx] = other;
		other.parent = this;
		if (other.branch != child.branch)
			other.assignBranch(child.branch);
		if (child.parent is this)
			child.parent = null;
	}
	auto getIndexOfChild(Node child)
	{
		import std.algorithm : countUntil;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		return idx;
	}
	bool isLastSibling()
	{
		if (parent is null)
			return true;
		return parent.children.length == parent.getIndexOfChild(this)+1;
	}
	void addChildren(Node[] children)
	{
		this.children ~= children;
		children.each!((c){c.parent = this; c.assignBranch(branch);});
	}
	void insertInPlace(size_t idx, Node child)
	{
		this.children.insertInPlace(idx,child);
		child.parent = this;
		child.assignBranch(branch);
	}
	void prependChildren(Node[] children)
	{
		this.children.insertInPlace(0,children);
		children.each!((c){c.parent = this; c.assignBranch(branch);});
	}
	void addChildren(Option!(Node[]) children)
	{
		if (children.isDefined)
			addChildren(children.get);
	}
	void addChild(Node child)
	{
		child.parent = this;
		this.children ~= child;
		child.assignBranch(branch);
	}
	Node[] detachStatementsAfter(Node node)
	{
		auto removed = this.removeChildrenAfter(node);
		removed.each!(c => c.branch = null);
		return removed;
	}
	void insertAfter(Node node)
	{
		// TODO: Test this
		if (this.parent.type == NodeType.CaseBodyNode)
		{
			this.parent.as!(CaseBodyNode).insertAfter(this,[node]);
		} else if (this.parent.type == NodeType.FunctionBodyNode)
		{
			if (node.type == NodeType.BlockStatementNode)
				this.parent.as!(FunctionBodyNode).insertAfter(this, node.children);
			else
				this.parent.as!(FunctionBodyNode).insertAfter(this, [node]);
		} else
		{
			if (this.parent.type != NodeType.BlockStatementNode)
			{
				auto block = new BlockStatementNode([]);
				this.replaceWith(block);
				block.addChild(this);
			}
			if (node.type == NodeType.BlockStatementNode)
				this.parent.as!(BlockStatementNode).insertAfter(this, node.children);
			else
				this.parent.as!(BlockStatementNode).insertAfter(this, [node]);
		}
		this.parent.reanalyseHints();
	}
	void insertBefore(Node node)
	{
		if (node.type == NodeType.BlockStatementNode)
			this.insertBefore(node.children);
		else
			this.insertBefore([node]);
	}
	void insertBefore(Node[] nodes)
	{
		if (this.parent.type == NodeType.ExpressionNode)
			this.parent.as!(ExpressionNode).insertBefore(this, nodes);
		else if (this.parent.type == NodeType.FunctionBodyNode)
			this.parent.as!(FunctionBodyNode).insertBefore(this, nodes);
		else if (this.parent.type == NodeType.ModuleNode)
			this.parent.as!(ModuleNode).insertBefore(this, nodes);
		else {
			if (this.parent.type != NodeType.BlockStatementNode)
			{
				auto block = new BlockStatementNode([]);
				this.replaceWith(block);
				block.addChild(this);
			}
			this.parent.as!(BlockStatementNode).insertBefore(this, nodes);
		}
		this.parent.reanalyseHints();
	}
	Node findFirst(NodeType t)
	{
		if (type == t)
			return this;
		foreach(c; children)
			if (auto f = c.findFirst(t))
				return f;
		return null;
	}
	void removeChild(Node child)
	{
		import std.algorithm : remove;
		this.children = this.children.remove!(c => c is child);
		if (this.type == NodeType.ExpressionNode)
		{
			if (this.children.length == 1)
			{
				this.children[0].parent = null;
				this.replaceWith(this.children[0]);
			}
			return;
		}
	}
	void detach()
	{
		if (this.parent is null)
			return;
		auto parent = this.parent;
		this.parent = null;
		parent.removeChild(this);
	}
}

class ErrorNode : Node
{
	string value;
	size_t line;
	size_t column;
	string debugMessage;
	this(string v, size_t l, size_t c, string debugMsg = "", in string file = __FILE__, in size_t line2 = __LINE__)
	{
		version (chatty) { writefln("Error At %s@%s: %s@%s:%s",file,line2,v,l,c); }
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.keyword == keyword ? Diff.No : Diff.Content;
	}
}

class IdentifierReferenceNode : Node
{
	string identifier;
	this(string i)
	{
		identifier = i;
		super(NodeType.IdentifierReferenceNode);
	}
	override string toString()
	{
		return "IdentifierReferenceNode (\""~identifier~"\")";
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
		if (other.hints != hints)
			return Diff.Hints;
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
	void addExpression(Node node)
	{
		if (node.type == NodeType.ExpressionNode)
			this.addChildren(node.children);
		else
			this.addChild(node);
	}
	void insertBefore(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.news == news ? Diff.No : Diff.Content;
	}
}
class UnaryExpressionNode : Node
{
	Node[] prefixs;
	Postfix postfix = Postfix.None;
	this(Node[] ps, Node n = null)
	{
		prefixs = ps;
		super(NodeType.UnaryExpressionNode,n);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s\n",type);
		sink.print(prefixs,level+1);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
	// TODO: we can remove the array if we return a custom range...
	ExpressionOperatorNode[] getOperatorsSurrounding(Node node)
	{
		auto idx = this.getIndexOfChild(node);
		if (idx > 0)
			return this.children[idx-1..$].stride(2).take(1).map!(op=>op.as!(ExpressionOperatorNode)).array();
		if (this.children.length == 1)
			return [];
		return this.children[idx+1..idx+2].map!(op=>op.as!(ExpressionOperatorNode)).array();
	}
	// TODO: we can remove the array if we return a custom range...
	ExpressionOperatorNode[] getOperators()
	{
		if (this.children.length < 2)
			return [];
		return this.children[1..$].stride(2).map!(op=>op.as!(ExpressionOperatorNode)).array();
	}
	void replaceChildWith(Node child, BinaryExpressionNode bin)
	{
		auto idx = this.getIndexOfChild(child);
		this.children[idx] = bin.children[0];
		if (bin.children.length > 1)
			this.children.insertInPlace(idx+1, bin.children[1..$]);
		bin.children.each!((c){ c.parent = this; c.assignBranch(this.branch); });
	}
	auto nodes()
	{
		return this.children.stride(2);
	}
}
class ConditionalExpressionNode : Node
{
	this(Node cond, Node truthPath, Node elsePath)
	{
		super(NodeType.ConditionalExpressionNode,[cond,truthPath,elsePath]);
	}
	Node condition()
	{
		return this.children[0];
	}
	Node truthPath()
	{
		return this.children[1];
	}
	Node elsePath()
	{
		return this.children[2];
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
		node.replaceWith(node.children[2]).reanalyseHints();
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
	void dropAllFrom(Node node)
	{
		// TODO: need to test this
		auto idx = children.countUntil!(c => c is node);
		if (idx == -1)
			return;
		children = children[0..idx];
	}
	void dropAllFrom(NodeType type)
	{
		// TODO: need to test this
		auto idx = children.countUntil!(c => c.type == type);
		if (idx == -1)
			return;
		children = children[0..idx];
	}
	void insertAfter(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx+1, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
		// ALSO move branches in children 
	}
	void insertBefore(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
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
		if (node.children.length == 0)
			assert(0);
		return node.children[$-1];
	}
	bool hasStatements()
	{
		if (node.type == NodeType.BlockStatementNode)
			return node.children.length > 0;
		return node.type != NodeType.EmptyStatementNode;
	}
	Node convertToAssignmentExpression()
	{
		if (node.type == NodeType.BlockStatementNode)
			return node.as!(BlockStatementNode).convertToAssignmentExpression();
		return node.convertToAssignmentExpression();
	}
	bool isBlockStatement()
	{
		return node.type == NodeType.BlockStatementNode;
	}
	bool isSingleStatement()
	{
		return !isBlockStatement() || node.children.length == 1;
	}
	void clearStatements()
	{
		if (isBlockStatement)
			node.children = node.children[0..0];
		else
		{
			node = node.replaceWith(new BlockStatementNode([]));
		}
	}
	void addStatements(Node[] children)
	{
		import es6.analyse;
		if (isBlockStatement)
		{
			node.addChildren(children);
		} else {
			Node block = new BlockStatementNode([]);
			node.replaceWith(block);
			block.addChild(node);
			block.addChildren(children);
			node = block;
		}
		this.reanalyseHints();
	}
	void addStatements(Option!(Node[]) children)
	{
		if (children.isDefined)
		{
			addStatements(children.get);
		}
	}
}
Node convertToAssignmentExpression(BlockStatementNode node)
{
	import std.array : array;
	import std.algorithm : each;
	import std.array : appender;
	version (unittest) if (node.hints.has(Hint.NonExpression)) throw new UnitTestException(["Cannot convert non-expressions"]);
	if (node.hints.has(Hint.NonExpression)) {
		import std.stdio;
		writeln(node);
	}
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
	auto expr = new ExpressionNode(app.data);
	expr.reanalyseHints();
	return new ParenthesisNode(expr);
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
	void removeElsePath() { children[2].branch.remove(); children[2].parent = null; children = children[0..2]; }
	void swapPaths()
	{
		// TODO: need to test
		assert(hasElsePath());
		auto firstBranch = truthPath.branch;
		auto elsePath = children[2];
		children[2] = children[1];
		children[1] = elsePath;
		firstBranch.swapWithNext();
	}
	bool bothPathsReturn()
	{
		// TODO: need to test
		if (!hasElsePath)
			return false;
		return truthPath.node.hints.has(Hint.Return | Hint.ReturnValue) &&
			elsePath.node.hints.has(Hint.Return | Hint.ReturnValue);
	}
	void forceElsePath()
	{
		if (hasElsePath)
			return;
		auto block = new BlockStatementNode([]);
		auto branch = truthPath.node.branch.newSiblingAfter(block);
		addChild(block);
		block.branch = branch;
	}
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
	this(Node condition, Node caseBody)
	{
		super(NodeType.CaseNode,[condition,caseBody]);
	}
}
class CaseBodyNode : Node
{
	this(Node[] children)
	{
		super(NodeType.CaseBodyNode,children);
	}
	void insertAfter(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx+1, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
	}
}
class DefaultNode : Node
{
	this(Node child)
	{
		super(NodeType.DefaultNode,child);
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
		sink.formattedWrite("ForStatement %s",loopType);
		if (_hints != Hint.None)
			sink.formattedWrite(" %s\n",_hints);
		else
			sink.formattedWrite("\n");
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if (other.hints != hints)
			return Diff.Hints;
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
		if (name !is null)
			children ~= name;
		if (base !is null)
			children ~= base;
		children ~= methods;
		foreach(c; children)
		{
			assert(c !is null);
			c.parent = this;
		}
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
		if (other.hints != hints)
			return Diff.Hints;
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
	Node name()
	{
		return this.children[0];
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
		if (other.hints != hints)
			return Diff.Hints;
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
	void insertAfter(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx+1, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
	}
	void insertBefore(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
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
		if (other.hints != hints)
			return Diff.Hints;
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
	void insertBefore(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
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
	BranchChildren,
	BranchEntryTypes,
	BranchHints,
	Hints
}
struct DiffResult
{
	Node a;
	Node b;
	Diff type;
}
string getDiffMessage(DiffResult d)
{
	final switch (d.type)
	{
		case Diff.No:
			return "No difference";
		case Diff.Type:
			return format("Node a is of type %s but Node b is of type %s\n%s%s",d.a.type,d.b.type,d.a,d.b);
		case Diff.Children:
			return format("Node a has the following children\n%sYet node b has\n%s",d.a,d.b);
		case Diff.Content:
			return format("Node a doesn't have the same content as node b");
		case Diff.BranchChildren:
			return format("Branch a has %s children, but branch b has %s\n%s%sFrom nodes\n%s%s",d.a.branch.children.length,d.b.branch.children.length,d.a.branch,d.b.branch,d.a,d.b);
		case Diff.BranchEntryTypes:
			return format("Branch a has entry\n%swhile branch b has\n%s--%s++%s",d.a,d.b,d.a.branch,d.b.branch);
		case Diff.BranchHints:
			return format("Branch a has hints\n%swhile branch b has\n%s",d.a.branch.hints,d.b.branch.hints);
		case Diff.Hints:
			return format("Node a has hints\n%swhile node b has\n%s",d.a,d.b);
	}
}
version (unittest)
{
	void assertNoDiff(DiffResult d, in string file = __FILE__, in size_t line = __LINE__)
	{
		if (d.type == Diff.No)
			return;
		throw new UnitTestException([d.getDiffMessage()],file,line);
	}	
}
void assertTreeInternals(Node a, in string file = __FILE__, in size_t line = __LINE__)
{
	void assertNodeInternals(Node a)
	{
		version(unittest)
			if (a.branch is null)
				throw new UnitTestException([format("Node has no branch.\n%s",a)],file,line);
		import std.stdio;
		if (a.branch is null)
			writeln(a.parent);
		assert(a.branch !is null);
		foreach(c; a.children)
		{
			version(unittest)
				if (c.parent !is a)
				{
					import std.stdio;
					writeln(c.parent,a);
					throw new UnitTestException([format("Node's child doesn't have right parent.")],file,line);
				}
			assert(c.parent is a);
			assertNodeInternals(c);
		}
	}
			import std.stdio;
	void assertBranchInternals(Branch a)
	{
		version(unittest)
		{
			if (a.entry is null)
				throw new UnitTestException([format("Branch has no entry.")],file,line);
			if (a.entry.branch !is a) {
				throw new UnitTestException([format("Branch's entry node doesn't refer to this branch\n%s%s",a.entry.branch,a)],file,line);
			}
		}
		if (a.parent is null)
			assert(a.scp.entry == a.entry);
		assert(a.entry !is null);
		assert(a.entry.branch is a);

		foreach(c; a.children)
		{
			assert(c.parent is a);
			assertBranchInternals(c);
		}
	}
	void assertScopeInternals(Scope a)
	{
		assert(a.branch !is null);
		assertBranchInternals(a.branch);
		foreach(c; a.children)
			assertScopeInternals(c);
	}
	assertNodeInternals(a);
	assert(a.branch !is null);
	assert(a.branch.scp !is null);
	assertScopeInternals(a.branch.scp);
}
private DiffResult diffBranch(Branch a, Branch b, Node c, Node d)
{
	import std.range : lockstep;
	assert(a !is null && b !is null);
	if (a.children.length != b.children.length)
		return DiffResult(c,d,Diff.BranchChildren);
	if (a.entry.type != b.entry.type)
		return DiffResult(c,d,Diff.BranchEntryTypes);
	if (a.hints != b.hints)
		return DiffResult(c,d,Diff.BranchHints);

	//foreach(ca,cb; lockstep(a.children,b.children))
	//{
	//	auto r = diffBranch(ca,cb);
	//	if (r.type != Diff.No)
	//		return r;
	//}
	return DiffResult(c,d,Diff.No);

}
DiffResult diffTree(Node a, Node b, in string file = __FILE__, in size_t line = __LINE__)
{
	import std.range : lockstep;
	version (unittest)
	{
		if (a.branch is null)
			throw new UnitTestException([format("Found a null on left branch while diffing\n%sand\n%s",a,b)],file,line);
		if (b.branch is null)
			throw new UnitTestException([format("Found a null on right branch while diffing\n%sand\n%s",a,b)],file,line);
	}
	if (a.type != b.type)
		return DiffResult(a,b,Diff.Type);
	if (a.children.length != b.children.length)
		return DiffResult(a,b,Diff.Children);

	assert(a.branch !is null && b.branch !is null);
	auto r = diffBranch(a.branch,b.branch,a,b);
	if (r.type != Diff.No)
		return r;
	foreach(ca,cb; lockstep(a.children,b.children))
	{
		auto r = diffTree(ca,cb,file,line);
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
	n.analyseNode();
	diffTree(n,n).type.shouldEqual(Diff.No);
	format("%s",n).shouldEqual("ModuleNode NonExpression, HasAssignment
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
  IdentifierReferenceNode identifier
  UnaryExpressionNode
    PrefixExpressionNode Negation
    IdentifierReferenceNode expr
  CallExpressionNode
    IdentifierReferenceNode obj
    AccessorNode a
  NewExpressionNode
    IdentifierReferenceNode a
  CallExpressionNode
    IdentifierReferenceNode a
    ArgumentsNode
  BinaryExpressionNode
    IdentifierReferenceNode a
    ExpressionOperatorNode
    IdentifierReferenceNode b
  AssignmentExpressionNode HasAssignment
    IdentifierReferenceNode c
    AssignmentOperatorNode HasAssignment
    IdentifierReferenceNode d
  LabelledStatementNode NonExpression
  ForStatement ExprCStyle NonExpression
    SemicolonNode
    SemicolonNode
    EmptyStatementNode
  ClassDeclarationNode NonExpression
    IdentifierReferenceNode b
    ClassGetterNode
      IdentifierReferenceNode x
      FunctionBodyNode
    ClassSetterNode
      IdentifierReferenceNode x
      IdentifierReferenceNode a
      FunctionBodyNode
    ClassMethodNode
      IdentifierReferenceNode method
      FormalParameterListNode
      FunctionBodyNode
    ClassGeneratorMethodNode
      IdentifierReferenceNode gen
      FormalParameterListNode
      FunctionBodyNode
  AssignmentExpressionNode HasAssignment
    ArrayLiteralNode
      ElisionNode 2
      IdentifierReferenceNode a
    AssignmentOperatorNode HasAssignment
    IdentifierReferenceNode b
  LexicalDeclarationNode NonExpression
    LexicalDeclarationItemNode
      IdentifierReferenceNode b
      IdentifierReferenceNode d
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

template getNodeType(AstNode : Node)
{
	mixin("alias getNodeType = NodeType."~AstNode.stringof~";");
}

template getAstNodeType(alias NodeType type)
{
	mixin("alias getAstNodeType = "~type.stringof~";");
}

@("getNodeType")
unittest
{
	assert(getNodeType!IfStatementNode == NodeType.IfStatementNode);
}
@("getAstNodeType")
unittest
{
	assert(is(getAstNodeType!(NodeType.IfStatementNode) : IfStatementNode));
}


