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

@safe:

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
enum NodeType: byte {
	ErrorNode,
	BooleanNode,
	SheBangNode,
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
	IdentifierNameNode,
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
	BindingElementNode,
	YieldExpressionNode
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
enum Hint:byte {
	None = 0,
	Return = 1 << 0,
	ReturnValue = 1 << 1,
	NonExpression = 1 << 2,
	Or = 1 << 3,
	HasAssignment = 1 << 4,
	Visited = 1 << 5 // Special hint used in testing to flag node having been visited
}
struct Hints
{
	private byte hs = Hint.None;
	alias hs this;
	this(byte h)
	{
		hs = h;
	}
	void toString(scope void delegate(const(char)[]) @safe sink) const
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
	byte get() { return hs; }
	bool has(byte v)
	{
		return (hs & v) != 0;
	}
	void add(byte h) {
		hs |= h;
	}
}
struct PrettyPrintSink
{
	private void delegate(const(char)[]) sink;
	private char[1] chr;
	int prefixIndent = 0;
	this(void delegate(const(char)[]) @safe sink, int indent = 0)
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

interface NodeVisitor
{
	void visit(Node node);
	void visit(ErrorNode node);
	void visit(BooleanNode node);
	void visit(SheBangNode node);
	void visit(StringLiteralNode node);
	void visit(BinaryLiteralNode node);
	void visit(OctalLiteralNode node);
	void visit(DecimalLiteralNode node);
	void visit(HexLiteralNode node);
	void visit(TemplateNode node);
	void visit(TemplateLiteralNode node);
	void visit(RegexLiteralNode node);
	void visit(KeywordNode node);
	void visit(IdentifierReferenceNode node);
	void visit(IdentifierNameNode node);
	void visit(ExpressionNode node);
	void visit(ParenthesisNode node);
	void visit(PrefixExpressionNode node);
	void visit(SuperPropertyNode node);
	void visit(AccessorNode node);
	void visit(NewTargetNode node);
	void visit(SpreadOperatorNode node);
	void visit(ArgumentsNode node);
	void visit(ArrayIndexNode node);
	void visit(NewExpressionNode node);
	void visit(CallExpressionNode node);
	void visit(UnaryExpressionNode node);
	void visit(ExpressionOperatorNode node);
	void visit(BinaryExpressionNode node);
	void visit(ConditionalExpressionNode node);
	void visit(AssignmentExpressionNode node);
	void visit(ArrowFunctionNode node);
	void visit(AssignmentOperatorNode node);
	void visit(ContinueStatementNode node);
	void visit(BreakStatementNode node);
	void visit(EmptyStatementNode node);
	void visit(LabelledStatementNode node);
	void visit(VariableDeclarationNode node);
	void visit(VariableStatementNode node);
	void visit(ReturnStatementNode node);
	void visit(BlockStatementNode node);
	void visit(IfStatementNode node);
	void visit(SwitchStatementNode node);
	void visit(DoWhileStatementNode node);
	void visit(WhileStatementNode node);
	void visit(CaseNode node);
	void visit(CaseBodyNode node);
	void visit(DefaultNode node);
	void visit(ForStatementNode node);
	void visit(WithStatementNode node);
	void visit(CatchStatementNode node);
	void visit(FinallyStatementNode node);
	void visit(TryStatementNode node);
	void visit(ThrowStatementNode node);
	void visit(DebuggerStatementNode node);
	void visit(ClassDeclarationNode node);
	void visit(ClassGetterNode node);
	void visit(ClassMethodNode node);
	void visit(ClassGeneratorMethodNode node);
	void visit(ClassSetterNode node);
	void visit(ComputedPropertyNameNode node);
	void visit(FormalParameterListNode node);
	void visit(FunctionDeclarationNode node);
	void visit(FunctionExpressionNode node);
	void visit(GeneratorDeclarationNode node);
	void visit(GeneratorExpressionNode node);
	void visit(RestElementNode node);
	void visit(SingleNameBindingNode node);
	void visit(SpreadElementNode node);
	void visit(ArrayLiteralNode node);
	void visit(ObjectLiteralNode node);
	void visit(PropertyDefinitionNode node);
	void visit(CoverInitializedName node);
	void visit(ElisionNode node);
	void visit(FunctionBodyNode node);
	void visit(LexicalDeclarationItemNode node);
	void visit(LexicalDeclarationNode node);
	void visit(ArrayBindingPatternNode node);
	void visit(ObjectBindingPatternNode node);
	void visit(BindingPropertyNode node);
	void visit(ExportClauseNode node);
	void visit(ExportDeclarationNode node);
	void visit(ExportDefaultDeclarationNode node);
	void visit(ExportSpecifierNode node);
	void visit(ImportClauseNode node);
	void visit(ImportDeclarationNode node);
	void visit(ImportSpecifierNode node);
	void visit(NamedImportsNode node);
	void visit(NameSpaceImportNode node);
	void visit(ModuleNode node);
	void visit(SemicolonNode node);
	void visit(BindingElementNode node);
	void visit(YieldExpressionNode node);
}

mixin template ImplVisitor(T)
{
	override void accept(NodeVisitor visitor) {
		visitor.visit(cast(T)this);
	}
}

class Node
{
	Node[] children;
	Node parent;
	Branch branch;
	private NodeType _type;
	@property NodeType type() pure const { return _type; }
	private Hints _hints;
	@property ref Hints hints() { return _hints; }
	@property void hints(byte hs) { _hints = Hints(hs); }
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
	void accept(NodeVisitor visitor) {
		visitor.visit(this);
	}
	void toString(scope void delegate(const(char)[]) @safe sink) const
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
	version (unittest)
	{
		auto as(Type)(in string file = __FILE__, in size_t line = __LINE__)
		{
			auto r = cast(Type)this;
			version (unittest)
			{
				import std.format;
				if (r is null)
					throw new UnitTestException([format("Tried to interpret as %s, but got %s",Type.stringof,this)],file,line);
			}
			assert(r !is null,format("Assertion this(%s) != %s. In %s @ %s",this.type,Type.stringof,file,line));
			return r;
		}
	} else
	{
		auto as(Type)()
		{
			auto r = cast(Type)this;
			assert(r !is null,format("Assertion this(%s) != %s.",this.type,Type.stringof));
			return r;
		}
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		return Diff.No;
	}
	Node getRoot()
	{
		if (branch)
			return branch.scp.getRoot();
		return this;
	}
	Node replaceWith(Node other) @trusted
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
	void replaceAt(size_t idx, Node other)
	{
		assert(idx < children.length);
		children[idx].parent = null;
		children[idx] = other;
		other.parent = this;
		if (other.branch !is branch)
			other.assignBranch(branch);
	}
	void replaceChild(Node child, Node other) @trusted
	{
		// TODO: rewrite to use replaceAt
		import std.algorithm : countUntil;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		children[idx] = other;
		other.parent = this;
		if (other.branch !is child.branch)
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
	auto getIndex()
	{
		version (unittest){ if (parent is null) throw new UnitTestException([format("Expected parent pointer to be !null.\n%s",this)]); }
		assert(parent);
		return parent.getIndexOfChild(this);
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
	void insertInPlace(size_t idx, Node[] children)
	{
		this.children.insertInPlace(idx,children);
		children.each!((c){c.parent = this; c.assignBranch(branch);});
	}
	void prependChildren(Node[] children)
	{
		insertInPlace(0,children);
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
			if (node.type == NodeType.BlockStatementNode)
				this.parent.as!(CaseBodyNode).insertAfter(this,node.children);
			else
				this.parent.as!(CaseBodyNode).insertAfter(this,[node]);
		} else if (this.parent.type == NodeType.FunctionBodyNode)
		{
			if (node.type == NodeType.BlockStatementNode)
				this.parent.as!(FunctionBodyNode).insertAfter(this, node.children);
			else
				this.parent.as!(FunctionBodyNode).insertAfter(this, [node]);
		} else if (this.parent.type == NodeType.ModuleNode)
			if (node.type == NodeType.BlockStatementNode)
				this.parent.as!(ModuleNode).insertAfter(this, node.children);
			else
				this.parent.as!(ModuleNode).insertAfter(this, [node]);
		else
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

final class ErrorNode : Node
{
	mixin ImplVisitor!ErrorNode;
	const(char)[] value;
	size_t line;
	size_t column;
	string debugMessage;
	this(const(char)[] v, size_t l, size_t c, string debugMsg = "", in string file = __FILE__, in size_t line2 = __LINE__)
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
		sink.formattedWrite("Error: %s at line %s at column %s %s\n",value,line,column,debugMessage);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!ErrorNode();
		return o.value == value && o.line == line && o.column == column ? Diff.No : Diff.Content;
	}
}
final class BooleanNode : Node
{
	mixin ImplVisitor!BooleanNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class SheBangNode : Node
{
	mixin ImplVisitor!SheBangNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.SheBangNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s \"%s\" %s\n",_type,cast(const(char)[])value,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class StringLiteralNode : Node
{
	mixin ImplVisitor!StringLiteralNode;
	const(ubyte)[] value; // all strings are normalized to single quoted string (meaning original double quoted strings are properly unescaped)
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.StringLiteralNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s \"%s\" %s\n",_type,cast(const(char)[])value,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class BinaryLiteralNode : Node
{
	mixin ImplVisitor!BinaryLiteralNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.BinaryLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class OctalLiteralNode : Node
{
	mixin ImplVisitor!OctalLiteralNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.OctalLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class DecimalLiteralNode : Node
{
	mixin ImplVisitor!DecimalLiteralNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.DecimalLiteralNode);
	}
	override string toString() @trusted
	{
		return "DecimalLiteralNode ("~cast(immutable(char)[])value~")";
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,cast(const(char)[])value,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class HexLiteralNode : Node
{
	mixin ImplVisitor!HexLiteralNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.HexLiteralNode);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class TemplateLiteralNode : Node
{
	mixin ImplVisitor!TemplateLiteralNode;
	this(TemplateNode n)
	{
		super(NodeType.TemplateLiteralNode,[n]);
	}
	this(Node[] children)
	{
		super(NodeType.TemplateLiteralNode,children);
	}
}
final class TemplateNode : Node
{
	mixin ImplVisitor!TemplateNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.TemplateNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,cast(const(char)[])value,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class RegexLiteralNode : Node
{
	mixin ImplVisitor!RegexLiteralNode;
	const(ubyte)[] value;
	this(const(ubyte)[] v)
	{
		value = v;
		super(NodeType.RegexLiteralNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,cast(const(char)[])value,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.value == value ? Diff.No : Diff.Content;
	}
}
final class KeywordNode : Node
{
	mixin ImplVisitor!KeywordNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.keyword == keyword ? Diff.No : Diff.Content;
	}
}
class IdentifierNode : Node
{
	const(ubyte)[] identifier;
	this(NodeType parentType, const(ubyte)[] i)
	{
		identifier = i;
		super(parentType);		
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,cast(const(char)[])identifier,_hints);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.identifier == identifier ? Diff.No : Diff.Content;
	}
}

final class IdentifierReferenceNode : IdentifierNode
{
	mixin ImplVisitor!IdentifierReferenceNode;
	this(const(ubyte)[] identifier)
	{
		super(NodeType.IdentifierReferenceNode,identifier);
	}
	override string toString() @trusted
	{
		return "IdentifierReferenceNode (\""~cast(immutable(char)[])identifier~"\")";
	}
}
final class IdentifierNameNode : IdentifierNode
{
	mixin ImplVisitor!IdentifierNameNode;
	this(const(ubyte)[] identifier)
	{
		super(NodeType.IdentifierNameNode,identifier);
	}
	override string toString() @trusted
	{
		return "IdentifierNameNode (\""~cast(immutable(char)[])identifier~"\")";
	}
}
final class ExpressionNode : Node
{
	mixin ImplVisitor!ExpressionNode;
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
final class ParenthesisNode : Node
{
	mixin ImplVisitor!ParenthesisNode;
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

final class PrefixExpressionNode : Node
{
	mixin ImplVisitor!PrefixExpressionNode;
	Prefix prefix;
	this(Prefix p)
	{
		super(NodeType.PrefixExpressionNode);
		prefix = p;
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,prefix,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.prefix == prefix ? Diff.No : Diff.Content;
	}
}
final class SuperPropertyNode : Node
{
	mixin ImplVisitor!SuperPropertyNode;
	this(Node n)
	{
		super(NodeType.SuperPropertyNode,[n]);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s super %s\n",type,_hints);
		sink.print(children,level+1);
	}
}
final class AccessorNode : Node
{
	mixin ImplVisitor!AccessorNode;
	const(ubyte)[] identifier;
	this(const(ubyte)[] i)
	{
		identifier = i;
		super(NodeType.AccessorNode);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s %s\n",type,cast(const(char)[])identifier,_hints);
		sink.print(children,level+1);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.identifier == identifier ? Diff.No : Diff.Content;
	}
}
final class NewTargetNode : Node
{
	mixin ImplVisitor!NewTargetNode;
	this()
	{
		super(NodeType.NewTargetNode);
	}
}
final class SpreadOperatorNode : Node
{
	mixin ImplVisitor!SpreadOperatorNode;
	this()
	{
		super(NodeType.SpreadOperatorNode);
	}
}
final class ArgumentsNode : Node
{
	mixin ImplVisitor!ArgumentsNode;
	this()
	{
		super(NodeType.ArgumentsNode);
	}
	this(Node[] args)
	{
		super(NodeType.ArgumentsNode,args);
	}
}
final class ArrayIndexNode : Node
{
	mixin ImplVisitor!ArrayIndexNode;
	this(Node expr)
	{
		super(NodeType.ArrayIndexNode,[expr]);
	}
}
final class NewExpressionNode : Node
{
	mixin ImplVisitor!NewExpressionNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.news == news ? Diff.No : Diff.Content;
	}
}
final class CallExpressionNode : Node
{
	mixin ImplVisitor!CallExpressionNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.news == news ? Diff.No : Diff.Content;
	}
}
final class UnaryExpressionNode : Node
{
	mixin ImplVisitor!UnaryExpressionNode;
	Node[] prefixs;
	Postfix postfix = Postfix.None;
	this(Node[] ps, Node n = null, Postfix postfix = Postfix.None)
	{
		prefixs = ps;
		this.postfix = postfix;
		super(NodeType.UnaryExpressionNode,n);
	}
	override void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		sink.formattedWrite("%s %s\n",type,_hints);
		sink.print(prefixs,level+1);
		sink.print(children,level+1);
	}
	override Diff diff(Node other) @trusted
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
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
final class ExpressionOperatorNode : Node
{
	mixin ImplVisitor!ExpressionOperatorNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.operator == operator ? Diff.No : Diff.Content;
	}
}
final class BinaryExpressionNode : Node
{
	mixin ImplVisitor!BinaryExpressionNode;
	this(Node[] children)
	{
		super(NodeType.BinaryExpressionNode,children);
	}
	// TODO: we can remove the array if we return a custom range...
	ExpressionOperatorNode[] getOperatorsSurrounding(Node node)
	{
		auto idx = this.getIndexOfChild(node);
		if (idx > 0)
			return this.children[idx-1..$].stride(2).take(2).map!(op=>op.as!(ExpressionOperatorNode)).array();
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
final class ConditionalExpressionNode : Node
{
	mixin ImplVisitor!ConditionalExpressionNode;
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
	void swapPaths()
	{
		auto tmp = this.children[1];
		this.children[1] = this.children[2];
		this.children[2] = tmp;
	}
}
final class AssignmentExpressionNode : Node
{
	mixin ImplVisitor!AssignmentExpressionNode;
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
final class ArrowFunctionNode : Node
{
	mixin ImplVisitor!ArrowFunctionNode;
	this(Node parameter, Node functionBody)
	{
		super(NodeType.ArrowFunctionNode,[parameter,functionBody]);
	}
}
final class AssignmentOperatorNode : Node
{
	mixin ImplVisitor!AssignmentOperatorNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.assignment == assignment ? Diff.No : Diff.Content;
	}
}
final class ContinueStatementNode : Node
{
	mixin ImplVisitor!ContinueStatementNode;
	const(ubyte)[] label;
	this(const(ubyte)[] label = null)
	{
		super(NodeType.ContinueStatementNode);
		this.label = label;
	}
}
final class BreakStatementNode : Node
{
	mixin ImplVisitor!BreakStatementNode;
	const(ubyte)[] label;
	this(const(ubyte)[] label = null)
	{
		super(NodeType.BreakStatementNode);
		this.label = label;
	}
}
final class EmptyStatementNode : Node
{
	mixin ImplVisitor!EmptyStatementNode;
	this()
	{
		super(NodeType.EmptyStatementNode);
	}
}
final class LabelledStatementNode : Node
{
	mixin ImplVisitor!LabelledStatementNode;
	const(ubyte)[] label;
	this(const(ubyte)[] l, Node stmt)
	{
		label = l;
		super(NodeType.LabelledStatementNode, stmt);
	}
	override Diff diff(Node other)
	{
		if (other.type != type)
			return Diff.Type;
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.label == label ? Diff.No : Diff.Content;
	}
}
final class VariableDeclarationNode : Node
{
	mixin ImplVisitor!VariableDeclarationNode;
	this(Node lhs, Node init = null)
	{
		if (init is null)
			super(NodeType.VariableDeclarationNode,[lhs]);
		else
			super(NodeType.VariableDeclarationNode,[lhs,init]);
	}
}
final class VariableStatementNode : Node
{
	mixin ImplVisitor!VariableStatementNode;
	this(Node[] children)
	{
		super(NodeType.VariableStatementNode,children);
	}
	void sortOnUnitializedUsage() @trusted
	{
		// Note: actually makes gzipped minified file a little larger on average...
		// Todo: maybe if put initialized vars with most usage at the front...
		/*import std.algorithm : schwartzSort;
		import std.typecons : tuple;
		children.schwartzSort!(c => tuple!("index","node")(c.getIndex(),c.as!VariableDeclarationNode),
			(a,b){
				if (a.node.children.length == 1 && b.node.children.length == 2)
					return true;
				if (b.node.children.length == 1 && a.node.children.length == 2)
					return false;
				return a.index < b.index;
			}
		);*/
	}
}
final class ReturnStatementNode : Node
{
	mixin ImplVisitor!ReturnStatementNode;
	this(Node expr = null)
	{
		super(NodeType.ReturnStatementNode,expr);
	}
}
final class BlockStatementNode : Node
{
	mixin ImplVisitor!BlockStatementNode;
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
		import std.range : retro;
		foreach(c; children[idx..$].retro)
		{
			c.detach();
			c.shread();
		}
		children = children[0..idx];
	}
	void dropAllFrom(NodeType type)
	{
		// TODO: need to test this
		auto idx = children.countUntil!(c => c.type == type);
		if (idx == -1)
			return;
		import std.range : retro;
		foreach(c; children[idx..$].retro)
		{
			c.detach();
			c.shread();
		}
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
		{
			node.children.shread();
			node.children = node.children[0..0];
		}
		else
		{
			auto old = node;
			node = node.replaceWith(new BlockStatementNode([]));
			shread(old);
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
		assignExpr.emitVisitor().shouldEqual(output,file,line);
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
final class IfStatementNode : Node
{
	mixin ImplVisitor!IfStatementNode;
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
	override void detach()
	{
		truthPath.branch.remove();
		if (hasElsePath)
			elsePath.branch.remove();
		super.detach();
	}
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
final class SwitchStatementNode : Node
{
	mixin ImplVisitor!SwitchStatementNode;
	this(Node[] children)
	{
		super(NodeType.SwitchStatementNode,children);
	}
}
final class DoWhileStatementNode : Node
{
	mixin ImplVisitor!DoWhileStatementNode;
	this(Node[] children)
	{
		super(NodeType.DoWhileStatementNode,children);
	}
}
final class WhileStatementNode : Node
{
	mixin ImplVisitor!WhileStatementNode;
	this(Node[] children)
	{
		super(NodeType.WhileStatementNode,children);
	}
}
final class CaseNode : Node
{
	mixin ImplVisitor!CaseNode;
	this(Node condition, Node caseBody)
	{
		super(NodeType.CaseNode,[condition,caseBody]);
	}
}
final class CaseBodyNode : Node
{
	mixin ImplVisitor!CaseBodyNode;
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
final class DefaultNode : Node
{
	mixin ImplVisitor!DefaultNode;
	this(Node child)
	{
		super(NodeType.DefaultNode,child);
	}
}
final class ForStatementNode : Node
{
	mixin ImplVisitor!ForStatementNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
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
final class WithStatementNode : Node
{
	mixin ImplVisitor!WithStatementNode;
	this(Node[] children)
	{
		super(NodeType.WithStatementNode,children);
	}
}
final class CatchStatementNode : Node
{
	mixin ImplVisitor!CatchStatementNode;
	this(Node[] children)
	{
		super(NodeType.CatchStatementNode,children);
	}
}
final class FinallyStatementNode : Node
{
	mixin ImplVisitor!FinallyStatementNode;
	this(Node block)
	{
		super(NodeType.FinallyStatementNode,[block]);
	}
}
final class TryStatementNode : Node
{
	mixin ImplVisitor!TryStatementNode;
	this(Node[] children)
	{
		super(NodeType.TryStatementNode,children);
	}
}
final class ThrowStatementNode : Node
{
	mixin ImplVisitor!ThrowStatementNode;
	this(Node expr)
	{
		super(NodeType.ThrowStatementNode,[expr]);
	}
}
final class DebuggerStatementNode : Node
{
	mixin ImplVisitor!DebuggerStatementNode;
	this()
	{
		super(NodeType.DebuggerStatementNode);
	}
}
final class ClassDeclarationNode : Node
{
	mixin ImplVisitor!ClassDeclarationNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		return Diff.No;
	}
}
final class ClassGetterNode : Node
{
	mixin ImplVisitor!ClassGetterNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
final class ClassMethodNode : Node
{
	mixin ImplVisitor!ClassMethodNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
final class ClassGeneratorMethodNode : Node
{
	mixin ImplVisitor!ClassGeneratorMethodNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
final class ClassSetterNode : Node
{
	mixin ImplVisitor!ClassSetterNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.isStatic == isStatic ? Diff.No : Diff.Content;
	}
}
final class ComputedPropertyNameNode : Node
{
	mixin ImplVisitor!ComputedPropertyNameNode;
	this(Node expr)
	{
		super(NodeType.ComputedPropertyNameNode,[expr]);
	}
}
final class FormalParameterListNode : Node
{
	mixin ImplVisitor!FormalParameterListNode;
	this(Node[] children)
	{
		super(NodeType.FormalParameterListNode,children);
	}
}
final class FunctionDeclarationNode : Node
{
	mixin ImplVisitor!FunctionDeclarationNode;
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.FunctionDeclarationNode,[params,funcBody]);
		else
			super(NodeType.FunctionDeclarationNode,[name,params,funcBody]);
	}
}
final class FunctionExpressionNode : Node
{
	mixin ImplVisitor!FunctionExpressionNode;
	this(Node name, Node params, Node funcBody)
	{
		if (name !is null)
			super(NodeType.FunctionExpressionNode,[name,params,funcBody]);
		else
			super(NodeType.FunctionExpressionNode,[params,funcBody]);
	}
}
final class GeneratorDeclarationNode : Node
{
	mixin ImplVisitor!GeneratorDeclarationNode;
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.GeneratorDeclarationNode,[params,funcBody]);
		else
			super(NodeType.GeneratorDeclarationNode,[name,params,funcBody]);
	}
}
final class GeneratorExpressionNode : Node
{
	mixin ImplVisitor!GeneratorExpressionNode;
	this(Node name, Node params, Node funcBody)
	{
		if (name is null)
			super(NodeType.GeneratorExpressionNode,[params,funcBody]);
		else
			super(NodeType.GeneratorExpressionNode,[name,params,funcBody]);
	}
}
final class RestElementNode : Node
{
	mixin ImplVisitor!RestElementNode;
	this(Node iden)
	{
		super(NodeType.RestElementNode,[iden]);
	}
}
final class SingleNameBindingNode : Node
{
	mixin ImplVisitor!SingleNameBindingNode;
	this(Node name, Node expr)
	{
		super(NodeType.SingleNameBindingNode,[name,expr]);
	}
}
final class SpreadElementNode : Node
{
	mixin ImplVisitor!SpreadElementNode;
	this(Node iden)
	{
		super(NodeType.SpreadElementNode,[iden]);
	}
}
final class ArrayLiteralNode : Node
{
	mixin ImplVisitor!ArrayLiteralNode;
	this(Node[] children)
	{
		super(NodeType.ArrayLiteralNode,children);
	}
}
final class ObjectLiteralNode : Node
{
	mixin ImplVisitor!ObjectLiteralNode;
	this(Node[] children)
	{
		super(NodeType.ObjectLiteralNode,children);
	}
}
final class PropertyDefinitionNode : Node
{
	mixin ImplVisitor!PropertyDefinitionNode;
	this(Node name, Node expr)
	{
		super(NodeType.PropertyDefinitionNode,[name,expr]);
	}
	Node name()
	{
		return this.children[0];
	}
}
final class CoverInitializedName : Node
{
	mixin ImplVisitor!CoverInitializedName;
	this(Node iden, Node init)
	{
		super(NodeType.CoverInitializedName,[iden,init]);
	}
}
final class ElisionNode : Node
{
	mixin ImplVisitor!ElisionNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.cnt == cnt ? Diff.No : Diff.Content;
	}
}
final class FunctionBodyNode : Node
{
	mixin ImplVisitor!FunctionBodyNode;
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
final class LexicalDeclarationNode : Node
{
	mixin ImplVisitor!LexicalDeclarationNode;
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
		if ((other.hints ^ hints) & ~Hint.Visited)
			return Diff.Hints;
		auto o = other.as!(typeof(this));
		return o.declaration == declaration ? Diff.No : Diff.Content;
	}
}
final class LexicalDeclarationItemNode : Node
{
	mixin ImplVisitor!LexicalDeclarationItemNode;
	this(Node lhs, Node init = null)
	{
		if (init is null)
			super(NodeType.LexicalDeclarationItemNode,[lhs]);
		else
			super(NodeType.LexicalDeclarationItemNode,[lhs,init]);
	}
}
final class ArrayBindingPatternNode : Node
{
	mixin ImplVisitor!ArrayBindingPatternNode;
	this(Node[] children)
	{
		super(NodeType.ArrayBindingPatternNode,children);
	}
}
final class ObjectBindingPatternNode : Node
{
	mixin ImplVisitor!ObjectBindingPatternNode;
	this(Node[] children)
	{
		super(NodeType.ObjectBindingPatternNode,children);
	}
}
final class BindingPropertyNode : Node
{
	mixin ImplVisitor!BindingPropertyNode;
	this(Node name, Node elem)
	{
		super(NodeType.BindingPropertyNode,[name,elem]);
	}
}
final class ExportClauseNode : Node
{
	mixin ImplVisitor!ExportClauseNode;
	this(Node[] children)
	{
		super(NodeType.ExportClauseNode,children);
	}
}
final class ExportDeclarationNode : Node
{
	mixin ImplVisitor!ExportDeclarationNode;
	this(Node clause, Node specifier = null)
	{
		if (specifier is null)
			super(NodeType.ExportDeclarationNode,clause);
		else
			super(NodeType.ExportDeclarationNode,[clause,specifier]);
	}
}
final class ExportDefaultDeclarationNode : Node
{
	mixin ImplVisitor!ExportDefaultDeclarationNode;
	this(Node child)
	{
		super(NodeType.ExportDefaultDeclarationNode,child);
	}
}
final class ExportSpecifierNode : Node
{
	mixin ImplVisitor!ExportSpecifierNode;
	this(Node name, Node as)
	{
		super(NodeType.ExportSpecifierNode,[name,as]);
	}
}
final class ImportClauseNode : Node
{
	mixin ImplVisitor!ImportClauseNode;
	this(Node clause, Node imports)
	{
		super(NodeType.ImportClauseNode,[clause,imports]);
	}
}
final class ImportDeclarationNode : Node
{
	mixin ImplVisitor!ImportDeclarationNode;
	this(Node string)
	{
		super(NodeType.ImportDeclarationNode,[string]);
	}
	this(Node clause, Node string)
	{
		super(NodeType.ImportDeclarationNode,[clause,string]);
	}
}
final class ImportSpecifierNode : Node
{
	mixin ImplVisitor!ImportSpecifierNode;
	this(Node name, Node identifier)
	{
		super(NodeType.ImportSpecifierNode,[name,identifier]);
	}
}
final class NamedImportsNode : Node
{
	mixin ImplVisitor!NamedImportsNode;
	this(Node[] children)
	{
		super(NodeType.NamedImportsNode,children);
	}
}
final class NameSpaceImportNode : Node
{
	mixin ImplVisitor!NameSpaceImportNode;
	this(Node child)
	{
		super(NodeType.NameSpaceImportNode,child);
	}
}
final class ModuleNode : Node
{
	mixin ImplVisitor!ModuleNode;
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
	void insertAfter(Node sibling, Node[] children)
	{
		import std.algorithm : countUntil, each;
		auto idx = this.children.countUntil!(c=>c is sibling);
		assert(idx != -1);
		this.children.insertInPlace(idx+1, children);
		children.each!((c){c.parent = this; c.assignBranch(this.branch);});
	}
}
final class SemicolonNode : Node
{
	mixin ImplVisitor!SemicolonNode;
	this()
	{
		super(NodeType.SemicolonNode);
	}
}
final class BindingElementNode : Node
{
	mixin ImplVisitor!BindingElementNode;
	this(Node pattern, Node init)
	{
		super(NodeType.BindingElementNode,[pattern,init]);
	}
}
final class YieldExpressionNode : Node
{
	mixin ImplVisitor!YieldExpressionNode;
	Node assignExpr;
	bool _delegate;
	this(Node assignExpr, bool _delegate)
	{
		super(NodeType.YieldExpressionNode, assignExpr);
		this.assignExpr = assignExpr;
		this._delegate = _delegate;
	}
}
ErrorNode[] collectErrors(Node root)
{
	import std.array : appender, Appender;
	auto a = appender!(ErrorNode[]);
	void walk(Node root, Appender!(ErrorNode[]) a)
	{
		foreach(c; root.children)
			walk(c,a);
		if (root.type == NodeType.ErrorNode)
			a.put(root.as!ErrorNode);
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
	Hints,
	StrictMode,
	VariablesCount,
	Variable,
	VariableReferencesCount,
	IdentifiersCount
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
			return format("Node a (%s) doesn't have the same content as node b (%s)",d.a,d.b);
		case Diff.BranchChildren:
			return format("Branch a has %s children, but branch b has %s\na: %s\bb: %sFrom nodes\n%s%s\nRoot %s\n%s",d.a.branch.children.length,d.b.branch.children.length,d.a.branch,d.b.branch,d.a,d.b,d.a.getRoot,d.b.getRoot);
		case Diff.BranchEntryTypes:
			return format("Branch a has entry\n%swhile branch b has\n%s--%s++%s",d.a,d.b,d.a.branch,d.b.branch);
		case Diff.BranchHints:
			return format("Branch a has hints\n%s\nwhile branch b has\n%s\n%s\n%s",d.a.branch.hints,d.b.branch.hints,d.a.branch,d.b.branch);
		case Diff.Hints:
			return format("Node a has hints\n%swhile node b has\n%s",d.a,d.b);
		case Diff.StrictMode:
			return format("Scope a has strictMode=%s, b has strictMode=%s",d.a.branch.scp.strictMode,d.b.branch.scp.strictMode);
		case Diff.VariablesCount:
			return format("something VariablesCount, something");
		case Diff.Variable:
			return format("something Variable, something");
		case Diff.VariableReferencesCount:
			return format("yadayada, references");
		case Diff.IdentifiersCount:
			return format("oops, IdentifiersCount");
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
void assertTreeInternals(Node a, in string file = __FILE__, in size_t line = __LINE__) @trusted
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
	if ((a.hints & ~Hint.Visited) != (b.hints & ~Hint.Visited))
		return DiffResult(c,d,Diff.BranchHints);

	//foreach(ca,cb; lockstep(a.children,b.children))
	//{
	//	auto r = diffBranch(ca,cb);
	//	if (r.type != Diff.No)
	//		return r;
	//}
	return DiffResult(c,d,Diff.No);

}
private DiffResult diffScope(Scope a, Scope b, Node c, Node d) @trusted
{
	import std.algorithm : sort, setDifference, cmp;
	import std.range : lockstep;
	if (a.strictMode != b.strictMode)
		return DiffResult(c,d,Diff.StrictMode);
	if (a.variables.length != b.variables.length)
		return DiffResult(c,d,Diff.VariablesCount);
	auto varsA = a.variables.dup.sort!((a,b) => cmp(a.node.identifier,b.node.identifier) < 0);
	auto varsB = b.variables.dup.sort!((a,b) => cmp(a.node.identifier,b.node.identifier) < 0);
	auto diffs = setDifference!("a.node.identifier < b.node.identifier")(varsA,varsB);
	if (!diffs.empty)
		return DiffResult(c,d,Diff.Variable);
	foreach(ref a, ref b; varsA.lockstep(varsB))
	{
		if (a.references.length != b.references.length)
		{
			return DiffResult(c,d,Diff.VariableReferencesCount);
		}
	}
	if (a.identifiers.length != b.identifiers.length){
		return DiffResult(c,d,Diff.IdentifiersCount);
	}
	return DiffResult(c,d,Diff.No);
}
DiffResult diffTree(Node a, Node b, in string file = __FILE__, in size_t line = __LINE__) @trusted
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
	r = diffScope(a.branch.scp,b.branch.scp,a,b);
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
  DecimalLiteralNode 10
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
    EmptyStatementNode
  ForStatement ExprCStyle NonExpression
    SemicolonNode
    SemicolonNode
    EmptyStatementNode
  ClassDeclarationNode NonExpression
    IdentifierReferenceNode b
    ClassGetterNode
      IdentifierNameNode x
      FunctionBodyNode
    ClassSetterNode
      IdentifierNameNode x
      IdentifierReferenceNode a
      FunctionBodyNode
    ClassMethodNode
      IdentifierNameNode method
      FormalParameterListNode
      FunctionBodyNode
    ClassGeneratorMethodNode
      IdentifierNameNode gen
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

bool isStatementLike(Node node)
{
	switch(node.type)
	{
		case NodeType.ArgumentsNode:
		case NodeType.ArrowFunctionNode:
		case NodeType.ContinueStatementNode:
		case NodeType.BreakStatementNode:
		case NodeType.EmptyStatementNode:
		case NodeType.LabelledStatementNode:
		case NodeType.VariableStatementNode:
		case NodeType.ReturnStatementNode:
		case NodeType.BlockStatementNode:
		case NodeType.IfStatementNode:
		case NodeType.SwitchStatementNode:
		case NodeType.DoWhileStatementNode:
		case NodeType.WhileStatementNode:
		case NodeType.CaseBodyNode:
		case NodeType.ForStatementNode:
		case NodeType.WithStatementNode:
		case NodeType.CatchStatementNode:
		case NodeType.FinallyStatementNode:
		case NodeType.TryStatementNode:
		case NodeType.ThrowStatementNode:
		case NodeType.DebuggerStatementNode:
		case NodeType.FunctionBodyNode:
		case NodeType.ModuleNode:
			return true;
		default:
			return false;
	}
}

Node getStatementItem(Node node)
{
	switch (node.parent.type)
	{
		case NodeType.BlockStatementNode:
		case NodeType.FunctionBodyNode:
		case NodeType.ModuleNode:
			return node;
		default:
			break;
	}
	return node.parent.getStatementItem();
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

auto assignmentToExpressionOperator(Assignment a)
{
	final switch (a)
	{
	case Assignment.LeftShiftAssignment: return ExpressionOperator.LeftShift;
	case Assignment.TripleRightShiftAssignment: return ExpressionOperator.TripleRightSift;
	case Assignment.RightShiftAssignment: return ExpressionOperator.RightShift;
	case Assignment.Assignment: assert(false);
	case Assignment.AdditiveAssignment: return ExpressionOperator.Add;
	case Assignment.DecrementalAssignment: return ExpressionOperator.Minus;
	case Assignment.MultiplicativeAssignment: return ExpressionOperator.Multiply;
	case Assignment.DivisionAssignment: return ExpressionOperator.Division;
	case Assignment.ModAssignment: return ExpressionOperator.Mod;
	case Assignment.BitwiseAndAssignment: return ExpressionOperator.BitwiseAnd;
	case Assignment.BitwiseOrAssignment: return ExpressionOperator.BitwiseOr;
	case Assignment.BitwiseXorAssignment: return ExpressionOperator.BitwiseXor;
	}
}

bool canHaveSideEffects(Node node)
{
	import std.algorithm : any;
	switch (node.type)
	{
		case NodeType.KeywordNode:
			auto k = node.as!(KeywordNode).keyword;
			return !(k == Keyword.Null || k == Keyword.This);
		case NodeType.IdentifierReferenceNode:
		case NodeType.DecimalLiteralNode:
		case NodeType.StringLiteralNode:
		case NodeType.ExpressionOperatorNode:
		case NodeType.RegexLiteralNode:
			return false;
		case NodeType.BinaryExpressionNode:
		case NodeType.UnaryExpressionNode:
		case NodeType.ExpressionNode:
		case NodeType.ParenthesisNode:
			return node.children.any!(canHaveSideEffects);
		default:
			return true;
	}
}

bool inLoop(Node node)
{
	switch(node.type)
	{
		case NodeType.ForStatementNode:
		case NodeType.DoWhileStatementNode:
		case NodeType.WhileStatementNode:
			return true;
		case NodeType.FunctionBodyNode:
			return false;
		default:
			return node.parent.inLoop();
	}
}

auto calcDepth(Node node, Node root = null)
{
	size_t i;
	for(; node !is root; i++)
		node = node.parent;
	return i;
}

auto getNthParent(Node node, size_t d)
{
	for(;d != 0;d--)
		node = node.parent;
	return node;
}

auto requiresLHS(Node node)
{
	import std.algorithm : any;
	switch(node.parent.type)
	{
		case NodeType.UnaryExpressionNode:
			if (node.parent.as!(UnaryExpressionNode).postfix != Postfix.None)
				return true;
			if (node.parent.as!(UnaryExpressionNode).prefixs.map!(p => p.as!(PrefixExpressionNode).prefix).any!(p => p == Prefix.Increment || p == Prefix.Decrement))
				return true;
			return false;
		default:
			return false; // TODO: There are more cases a lhs is required (e.g. left hand side of assignment expression)
	}
}
// This function determines if node `node` can be moved to `to` and would still evaluate the same, E.g. there is no expression between `node` and `to` that might change the `node` expression or any of its sub expressions.
// TODO: this can be improved upon by dataflow analysis (but is a rabbit hole :)
auto canBeMovedTo(Node node, Node to)
{
	import std.algorithm : any;
	switch (node.type)
	{
		case NodeType.KeywordNode:
			auto k = node.as!(KeywordNode).keyword;
			return k == Keyword.Null || k == Keyword.This;
		case NodeType.DecimalLiteralNode:
		case NodeType.StringLiteralNode:
		case NodeType.ExpressionOperatorNode:
		case NodeType.RegexLiteralNode:
			return true;
		case NodeType.BinaryExpressionNode:
		case NodeType.UnaryExpressionNode:
		case NodeType.ExpressionNode:
		case NodeType.ParenthesisNode:
			return !node.children.any!(c => !canBeMovedTo(c,to));
		default:
			return false;
	}
}

bool isVoid0(Node node)
{
	if (node.type != NodeType.UnaryExpressionNode)
		return false;
	auto un = node.as!UnaryExpressionNode;
	if (un.prefixs.length != 1)
		return false;
	if (un.prefixs[0].as!(PrefixExpressionNode).prefix != Prefix.Void)
		return false;
	if (un.children[0].type != NodeType.DecimalLiteralNode)
		return false;
	if (un.children[0].as!(DecimalLiteralNode).value != cast(const(ubyte)[])"0")
		return false;
	return true;
}

bool isUndefined(Node node)
{
	if (node.type != NodeType.IdentifierReferenceNode)
		return false;
	return node.as!(IdentifierReferenceNode).identifier == "undefined";
}

bool isLastInFunctionBody(Node node)
{
	if (node.branch.scp.entry.type != NodeType.FunctionBodyNode)
		return false;
	auto funBody = node.branch.scp.entry;
	do
	{
		if (node.parent.children[$-1] !is node)
			return false;
		node = node.parent;
	} while (node !is funBody);
	return true;
}
bool isComparator(ExpressionOperator op)
{
	final switch (op)
	{
		case ExpressionOperator.InstanceOf:
		case ExpressionOperator.In:
		case ExpressionOperator.LogicalAnd:
		case ExpressionOperator.LogicalOr:
		case ExpressionOperator.BitwiseAnd:
		case ExpressionOperator.BitwiseOr:
		case ExpressionOperator.BitwiseXor:
			return false;
		case ExpressionOperator.StrictEqual:
		case ExpressionOperator.Equal:
		case ExpressionOperator.StrictNotEqual:
		case ExpressionOperator.NotEqual:
		case ExpressionOperator.LessOrEqual:
		case ExpressionOperator.LessThan:
		case ExpressionOperator.GreaterOrEqual:
		case ExpressionOperator.GreaterThan:
			return true;
		case ExpressionOperator.LeftShift:
		case ExpressionOperator.TripleRightSift:
		case ExpressionOperator.RightShift:
		case ExpressionOperator.Add:
		case ExpressionOperator.Minus:
		case ExpressionOperator.Multiply:
		case ExpressionOperator.Division:
		case ExpressionOperator.Mod:
			return false;
	}
}

bool isLogical(ExpressionOperator op)
{
	final switch (op)
	{
		case ExpressionOperator.InstanceOf:
		case ExpressionOperator.In:
			return false;
		case ExpressionOperator.LogicalAnd:
		case ExpressionOperator.LogicalOr:
			return true;
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
			return false;
	}
}

bool isArithmetic(ExpressionOperator op)
{
	final switch (op)
	{
		case ExpressionOperator.InstanceOf:
		case ExpressionOperator.In:
		case ExpressionOperator.LogicalAnd:
		case ExpressionOperator.LogicalOr:
			return false;
		case ExpressionOperator.BitwiseAnd:
		case ExpressionOperator.BitwiseOr:
		case ExpressionOperator.BitwiseXor:
			return true;
		case ExpressionOperator.StrictEqual:
		case ExpressionOperator.Equal:
		case ExpressionOperator.StrictNotEqual:
		case ExpressionOperator.NotEqual:
		case ExpressionOperator.LessOrEqual:
		case ExpressionOperator.LessThan:
		case ExpressionOperator.GreaterOrEqual:
		case ExpressionOperator.GreaterThan:
			return false;
		case ExpressionOperator.LeftShift:
		case ExpressionOperator.TripleRightSift:
		case ExpressionOperator.RightShift:
		case ExpressionOperator.Add:
		case ExpressionOperator.Minus:
		case ExpressionOperator.Multiply:
		case ExpressionOperator.Division:
		case ExpressionOperator.Mod:
			return true;
	}
}
bool partOfCondition(Node node)
{
	switch (node.parent.type)
	{
		case NodeType.IfStatementNode:
			return node.parent.children[0] is node;
		case NodeType.ConditionalExpressionNode:
			return node.parent.children[0] is node;
		case NodeType.ParenthesisNode:
		case NodeType.ExpressionOperatorNode:
			return node.parent.partOfCondition();
		case NodeType.BinaryExpressionNode:
		case NodeType.ModuleNode:
		case NodeType.FunctionBodyNode:
		case NodeType.BlockStatementNode:
			return true;
		default:
			return false;
	}
}
// Whether a node requires a semicolon of line-break after it
bool requiresSeparator(Node node)
{
	switch (node.type) {
		case NodeType.FunctionDeclarationNode:
		case NodeType.FunctionExpressionNode:
		case NodeType.GeneratorDeclarationNode:
		case NodeType.GeneratorExpressionNode:
		case NodeType.ClassDeclarationNode:
		case NodeType.SwitchStatementNode:
		case NodeType.BlockStatementNode:
			return false;
		case NodeType.LabelledStatementNode:
			return node.children[$-1].requiresSeparator;
		case NodeType.IfStatementNode:
			auto ifStmt = node.as!(IfStatementNode);
			if (ifStmt.hasElsePath)
				return ifStmt.elsePath.node.requiresSeparator;
			return ifStmt.truthPath.node.requiresSeparator;
		case NodeType.ForStatementNode:
			return !node.children[$-1].type == NodeType.BlockStatementNode;
		case NodeType.WhileStatementNode:
			return !node.children[$-1].type == NodeType.BlockStatementNode;
		default: return true;
	}
}



