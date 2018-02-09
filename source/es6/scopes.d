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
module es6.scopes;

@safe:

import es6.nodes;
import es6.utils;
import std.format : formattedWrite;
import std.algorithm : each, countUntil, remove;
import std.array : insertInPlace;
import std.range : lockstep;
import option;

version (unittest)
{
	import unit_threaded;
	import std.stdio;
	import es6.parser;
	import es6.analyse;
	void assertIdentifierEqual(Node node, string identifier, in string file = __FILE__, in size_t line = __LINE__)
	{
		if (node.type == NodeType.IdentifierReferenceNode)
			node.as!(IdentifierReferenceNode).identifier.shouldEqual(identifier,file,line);
		else if (node.type == NodeType.IdentifierNameNode)
			node.as!(IdentifierNameNode).identifier.shouldEqual(identifier,file,line);		
	}
	void assertGlobals(Scope s, string[] expecteds, in string file = __FILE__, in size_t line = __LINE__) @trusted
	{
		s.globals.length.shouldEqual(expecteds.length,file,line);
		foreach(got, expected; lockstep(s.globals,expecteds))
			got.node.assertIdentifierEqual(expected,file,line);
	}
}

enum IdentifierType
{
	Parameter,
	Variable,
	Function,
	Identifier
}
struct Variable
{
	IdentifierType type;
	IdentifierNode node;
	Node[] references; //TODO: we always assume the references to be in the order they appear in the tree depth-first. Ergo, we need to make this private and ensure the assumption always holds.
	Node definition; // will only be set when variable is redeclared (e.g. var a; var a;)
	this(IdentifierNode n, IdentifierType t, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format;
		assert(t != IdentifierType.Identifier,format("At %s:%s, %s, got %s",file,line,n));
		type = t;
		node = n;
	}
	void insertReference(Node reference)
	{
		// The references array is sorted in the order the references appear in the AST tree (depth-first).
		// Some algorithms assume this is always the case. Therefor when inserting a new reference,
		// we need to determine its place in the sorted array.

		// To do that we determine the index in the references array where the reference to be inserted
		// comes before the item in the reference array at that index. Where before is defined depth-first.

		// To determine whether a reference comes before another we need to find their common parent
		// and compare indices in their common parent children array.

		size_t depth = reference.calcDepth(node.branch.scp.entry);
		import std.stdio;

		size_t idx = references.countUntil!((r){
			Node item = reference;
			size_t d = r.calcDepth(node.branch.scp.entry);
			if (d > depth)
				r = r.getNthParent(d-depth);
			else if (depth > d)
				item = item.getNthParent(depth-d);

			while(r.parent !is item.parent)
			{
				assert(r.parent);
				assert(item.parent);
				r = r.parent;
				item = item.parent;
			}

			return item.getIndex() < r.getIndex();
		});

		if (idx == -1)
			references ~= reference;
		else
			references.insertInPlace(idx, reference);
	}
}
struct Label
{
	LabelledStatementNode definition;
	Node[] references;
}
bool isLocal(ref Variable v)
{
	import std.algorithm : any;
	return !v.references.any!(r => r.branch.scp !is v.node.branch.scp);
}
struct Identifier
{
	IdentifierNode node;
	Node definition;
	this(IdentifierNode n)
	{
		node = n;
	}
	this(IdentifierNode n, Node def)
	{
		node = n;
		definition = def;
	}
}
void linkToDefinition(Scope s, ref Identifier i)
{
	auto idx = s.variables.countUntil!(v => v.node.identifier == i.node.identifier);
	if (idx == -1)
	{
		if (s.parent)
			s.parent.linkToDefinition(i);
	} else
	{
		s.variables[idx].references ~= i.node;
		i.definition = s.variables[idx].node;
	}
}
void linkToLabel(Scope s, Node node, const(ubyte)[] label)
{
	auto idx = s.labels.countUntil!(l => l.definition.label == label);
	if (idx == -1)
		return;
	s.labels[idx].references ~= node;
}
class Scope
{
	Scope parent;
	Scope[] children;
	Branch branch;
	Node entry;
	bool strictMode = false;
	bool isModule = false;
	/*private*/ Variable[] variables;
	/*private*/ Identifier[] identifiers;
	Label[] labels;
	private Identifier[] globals;
	private int nextFreeIdentifier = -1;
	this(Node e, Scope p = null)
	{
		parent = p;
		entry = e;
		branch = new Branch(this,null,e);
		if (parent)
			strictMode = parent.strictMode;
	}
	Node getRoot()
	{
		if (parent)
			return parent.getRoot();
		return entry;
	}
	Scope newScope(Node entry)
	{
		auto s = new Scope(entry,this);
		this.children ~= s;
		return s;
	}
	void remove()
	{
		if (parent is null)
			return;
		parent.removeChild(this);
		parent = null;
	}
	void removeChild(Scope child)
	{
		import std.algorithm : countUntil;
		import std.algorithm : remove;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		children.remove(idx);
		children = children[0..$-1];
	}
	void addVariable(Variable v, bool prepend = false)
	{
		auto idx = variables.countUntil!(a => a.node.identifier == v.node.identifier);
		if (idx != -1)
		{
			v.definition = variables[idx].node;
			variables[idx].references ~= v.node;
		}
		if (prepend)
			variables.insertInPlace(0,v);
		else
			variables ~= v;
	}
	void addIdentifier(Identifier i)
	{
		identifiers ~= i;
	}
	void addLabel(LabelledStatementNode n)
	{
		labels ~= Label(n);
	}
	Variable[] getVariables()
	{
		return variables;
	}
	const(Identifier[]) getGlobals()
	{
		return globals;
	}
	void removeVariable(Node node)
	{
		variables = variables.remove!(v => v.node is node);
		node.detach(); // TODO: this should already have happened
	}
	void removeIdentifier(Node node)
	{
		auto idx = identifiers.countUntil!(i => i.node is node);
		if (idx == -1)
			return;
		if (identifiers[idx].definition !is null)
			identifiers[idx].definition.branch.scp.removeReference(identifiers[idx]);
		identifiers = identifiers.remove(idx);
	}
	void removeReference(ref Identifier iden)
	{
		import std.algorithm : find, remove;
		auto definition = iden.definition;
		if (definition is null)
			return;
		foreach(ref var; variables.find!((ref v) => v.node is definition))
			var.references = var.references.remove!(n => n is iden.node);
	}
	// only works after analyses
	bool isGlobal(string identifier)
	{
		import std.algorithm : canFind, filter;
		if (variables
				.canFind!((a,b) => a.node.identifier == b)(identifier))
			return false;
		if (identifiers
				.filter!(i => i.definition is null || i.definition.branch.scp !is this)
				.canFind!((a,b) => a.node.identifier == b)(identifier))
			return true;
		foreach(c; children)
			if (c.isGlobal(identifier))
				return true;
		return false;
	}
}
import std.range : ElementType;
auto onlyParameters(Range)(Range vs)
	if (is (ElementType!Range : Variable))
{
	import std.algorithm : filter;
	return vs.filter!(v => v.type == IdentifierType.Parameter);
}
class Branch
{
	Scope scp;
	Branch parent; 		/// The parent branch (can be null but only when starting the scope)
	Branch[] children;	/// Any nested branches
	Node entry;			/// The node that starts this branch
	int hints;
	this(Scope s, Branch p = null, Node e = null)
	{
		scp = s;
		parent = p;
		entry = e;
	}
	Branch newBranch(Node entry)
	{
		auto b = new Branch(scp,this,entry);
		this.children ~= b;
		return b;
	}
	Branch newBranchAfter(Branch sibling, Node entry)
	{
		import std.array : insertInPlace;
		auto idx = children.countUntil!(c=>c is sibling);
		if (idx == -1)
			return newBranch(entry);
		auto b = new Branch(scp,this,entry);
		children.insertInPlace(idx+1, b);
		return b;
	}
	void prepend(Branch b)
	{
		insertAt(0,b);
	}
	void insertAt(ptrdiff_t idx, Branch b)
	{
		import std.array : insertInPlace;
		children.insertInPlace(idx, b);
		if (b.parent)
			b.remove();
		b.parent = this;
	}
	void insertAfter(Branch sibling, Branch b)
	{
		import std.array : insertInPlace;
		auto idx = children.countUntil!(c=>c is sibling);
		insertAt(idx + 1, b);
	}
	Branch newSiblingAfter(Node entry)
	{
		assert(this.parent);
		return this.parent.newBranchAfter(this, entry);
	}
	bool hasHint(Hint h)()
	{
		return (hints & h) == h;
	}
	void addHint(int h)
	{
		hints |= h;
	}
	void removeHint(Hint h)()
	{
		hints = hints & ~h;
	}
	void remove()
	{
		if (parent is null)
			return;
		parent.removeChild(this);
		parent = null;
	}
	void removeChild(Branch child)
	{
		import std.algorithm : countUntil;
		import std.algorithm : remove;
		auto idx = children.countUntil!(c=>c is child);
		assert(idx != -1);
		children.remove(idx);
		children = children[0..$-1];
	}
	Branch[] detachChildrenAfter(Branch b)
	{
		return this.removeChildrenAfter(b);
	}
	Branch[] detachSiblingsAfter() {
		if (this.parent)
			return this.parent.removeChildrenAfter(this);
		assert(false);
	}
	void swapWithNext()
	{
		assert(parent !is null);
		auto idx = parent.children.countUntil!(c=>c is this);
		assert(parent.children.length > idx + 1);
		assert(idx != -1);
		parent.children[idx] = parent.children[idx+1];
		parent.children[idx+1] = this;
	}
	void addChildren(Branch[] bs)
	{
		this.children ~= bs;
		bs.each!(b => b.parent = this);
	}
	void toString(scope void delegate(const(char)[]) @safe sink) const
	{
		prettyPrint(PrettyPrintSink(sink));
	}
	void prettyPrint(PrettyPrintSink sink, int level = 0) const
	{
		sink.indent(level);
		entry.prettyPrint(sink,level);
		sink.formattedWrite("------\n");
		sink.print(children,level+1);
	}
	Node getParentBranchEntry()
	{
		Node helper()
		{
			switch(entry.parent.type)
			{
				case NodeType.SwitchStatementNode:
					return entry.parent.parent;
				case NodeType.IfStatementNode:
				case NodeType.ForStatementNode:
				case NodeType.DoWhileStatementNode:
				case NodeType.WhileStatementNode:
					return entry.parent;
				case NodeType.DefaultNode:
					return entry.parent.parent;
				case NodeType.CaseNode:
					return entry.parent.parent; default: assert(0);
			}
		}
		auto parentEntry = helper;
		if (parentEntry.parent.type == NodeType.LabelledStatementNode)
			return parentEntry.parent;
		return parentEntry;
	}
}
void linkIdentifierToDefinitions(Scope scp)
{
	scp.identifiers.each!((ref i) => scp.linkToDefinition(i));
	foreach(s; scp.children)
		s.linkIdentifierToDefinitions();
}
void findGlobals(Scope scp)
{
	import std.algorithm : canFind, each, filter;
	foreach(s; scp.children)
	{
		s.findGlobals();
		s.globals
			.each!(id=>scp.globals ~= id);
	}
	foreach(g; scp.identifiers.filter!(i=>i.definition is null || i.definition.branch.scp.parent is null))
		scp.globals ~= g;
}
@("findGlobals")
unittest
{
	getScope(`var a,b,c;`).assertGlobals([]);
	getScope(`a,b,c;`).assertGlobals(["a","b","c"]);
	auto s = getScope(`var a,b; function e(c){ var d; return a*b*c*d; }`);
	s.assertGlobals(["a","b"]);
	s.children[0].assertGlobals(["a","b"]);
	s = getScope(`function e(c){ var d; return a*b*c*d; }`);
	s.assertGlobals(["a","b"]);
	s.children[0].assertGlobals(["a","b"]);
	s = getScope(`function bla(a){function c(c){return a*c*d*b}var d,e}`);
	s.assertGlobals(["b"]);
	s.children[0].assertGlobals(["b"]);
	s.children[0].children[0].assertGlobals(["b"]);
}
string getFreeIdentifier(Scope s, ref int cnt)
{
	bool isUsedIdentifier(Scope s, string id)
	{
		import std.algorithm : any;
		if (s.identifiers.any!(i => i.node.identifier == cast(const(ubyte)[])id))
			return true;
		if (s.variables.any!(i => i.node.identifier == cast(const(ubyte)[])id))
			return true;
		foreach (c; s.children)
			if (isUsedIdentifier(c, id))
				return true;
		return false;
	}
	string id;
	for(;;cnt++)
	{
		id = generateValidIdentifier(cnt);
		if (!isUsedIdentifier(s, id))
			break;
	}
	return id;
}
@("isGlobal")
unittest
{
	void assertGlobals(Scope scp, string[] expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		foreach(e; expected)
			scp.isGlobal(e).shouldBeTrue(file,line);
	}
	void assertNonGlobals(Scope scp, string[] expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		foreach(e; expected)
			scp.isGlobal(e).shouldBeFalse(file,line);
	}
	assertNonGlobals(getScope(`var a,b,c;`),["a","b","c"]);
	assertGlobals(getScope(`a,b,c;`),["a","b","c"]);
	auto s = getScope(`var a,b; function e(c){ var d; return a*b*c*d; }`);
	assertNonGlobals(s,["a","b"]);
	assertGlobals(s.children[0],["a","b"]);
	s = getScope(`function e(c){ var d; return a*b*c*d; }`);
	assertGlobals(s,["a","b"]);
	assertGlobals(s.children[0],["a","b"]);
	s = getScope(`function bla(a){function c(c){return a*c*d*b}var d,e}`);
	assertGlobals(s,["b"]);
	assertNonGlobals(s,["a","d"]);
	assertGlobals(s.children[0],["b"]);
	assertNonGlobals(s.children[0],["a","d"]);
	assertGlobals(s.children[0].children[0],["a","d","b"]);
	s = getScope(`function bla(){for (var a in b);}`);
	assertGlobals(s.children[0],["b"]);
}

@("linkToDefinition")
unittest
{
	auto s = getScope(`var a,b,c; a = 6`);
	s.identifiers.length.shouldEqual(1);
	s.variables.length.shouldEqual(3);
	assert(s.variables[0].node is s.identifiers[0].definition);
	s.variables[0].references.length.shouldEqual(1);
	assert(s.variables[0].references[0] is s.identifiers[0].node);

	s = getScope(`var a,b; function c() { return a; }`);
	s.children[0].identifiers.length.shouldEqual(1);
	assert(s.variables[0].node is s.children[0].identifiers[0].definition);
	s.variables[0].references.length.shouldEqual(1);
	assert(s.variables[0].references[0] is s.children[0].identifiers[0].node);

	s = getScope(`function c(a) { return a; }`);
	s.children[0].identifiers.length.shouldEqual(1);
	auto ss = s.children[0];
	assert(ss.variables[0].node is ss.identifiers[0].definition);
	ss.variables[0].references.length.shouldEqual(1);
	assert(ss.variables[0].references[0] is ss.identifiers[0].node);

	s = getScope(`function c(a) { return a*c(); } c();`);
	s.identifiers.length.shouldEqual(1);
	ss = s.children[0];
	assert(s.variables[0].node is ss.identifiers[1].definition);
	s.variables.length.shouldEqual(1);
	s.variables[0].type.shouldEqual(IdentifierType.Function);
	s.variables[0].node.identifier.shouldEqual("c");
	s.variables[0].references.length.shouldEqual(2);
	s.identifiers.length.shouldEqual(1);
	s.identifiers[0].node.identifier.shouldEqual("c");
	assert(s.identifiers[0].definition is s.variables[0].node);
	assert(s.variables[0].references[0] is s.identifiers[0].node);

	s = getScope(`function b() { for (var a in b) p(a); }`);
	s.children[0].identifiers.length.shouldEqual(3);
	s.children[0].identifiers[0].node.identifier.shouldEqual("b");
	s.children[0].variables[0].node.identifier.shouldEqual("a");
	s.children[0].variables[0].references.length.shouldEqual(1);
	assert(s.children[0].variables[0].references[0] is s.children[0].identifiers[2].node);

	s = getScope(`function b() { var a; var a; }`);
	s.children[0].variables[0].definition.shouldBeNull();
	assert(s.children[0].variables[1].definition is s.children[0].variables[0].node);

	s = getScope(`function b(a) { var a; var a; }`);
	s.children[0].variables[0].definition.shouldBeNull();
	assert(s.children[0].variables[1].definition is s.children[0].variables[0].node);
	assert(s.children[0].variables[2].definition is s.children[0].variables[0].node);

	s = getScope(`function b(a) { for (var a in b); }`);
	s.children[0].variables[0].definition.shouldBeNull();
	assert(s.children[0].variables[1].definition is s.children[0].variables[0].node);
}
string generateIdentifier(int idx, string charMap = "nterioaucslfhpdgvmybwxkjqzNTERIOAUCSLFHPDGVMYBWXKJQZ_$")
{
	auto len = charMap.length;
	import std.conv : to;
	if (idx < len)
		return charMap[idx..idx+1];

	return generateIdentifier(cast(int)((idx / (len+0))-1), charMap)~generateIdentifier(cast(int)(idx % (len+0)), charMap);
}
bool isValidIdentifier(string id)
{
	import es6.lexer;
	return !isReservedKeyword(cast(const(ubyte)[])id);
}
string generateValidIdentifier(int start, string charMap = "nterioaucslfhpdgvmybwxkjqzNTERIOAUCSLFHPDGVMYBWXKJQZ_$")
{
	import std.range : iota, front;
	import std.algorithm : find, map;
	return iota(start,int.max).map!(idx => generateIdentifier(idx, charMap)).find!(isValidIdentifier).front();
}
@("generateIdentifier")
unittest
{
	void assertGeneratedName(int idx, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		generateIdentifier(idx,"abcdefghijklmnopqrstuvwxywABCDEFGHIJKLMNOPQRSTUVWXYZ").shouldEqual(expected,file,line);
	}
	assertGeneratedName(0,		"a");
	assertGeneratedName(1,		"b");
	assertGeneratedName(26,		"A");
	assertGeneratedName(51,		"Z");
	assertGeneratedName(52,		"aa");
	assertGeneratedName(103,	"aZ");
	assertGeneratedName(104,	"ba");
	assertGeneratedName(2755,	"ZZ");
	assertGeneratedName(2756,	"aaa");
	assert(!isValidIdentifier("do"));
	assert(isValidIdentifier("doda"));
	assert(!isValidIdentifier("function"));
	assert(isValidIdentifier("functioned"));
}
@("generateValidIdentifier")
unittest
{
	generateIdentifier((4*52)+14,"abcdefghijklmnopqrstuvwxywABCDEFGHIJKLMNOPQRSTUVWXYZ").shouldEqual("do");
	generateValidIdentifier((4*52)+14,"abcdefghijklmnopqrstuvwxywABCDEFGHIJKLMNOPQRSTUVWXYZ").shouldEqual("dp");

}