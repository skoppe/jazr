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

import es6.nodes;
import es6.utils;
import std.format : formattedWrite;
import std.algorithm : each, countUntil;
import option;

version (unittest)
{
	import unit_threaded;
	import std.stdio;
	import es6.parser;
	import es6.analyse;
	void assertGlobals(Scope s, string[] expecteds, in string file = __FILE__, in size_t line = __LINE__)
	{
		expecteds.length.shouldEqual(s.globals.length,file,line);
		foreach(got, expected; lockstep(s.globals,expecteds))
			got.node.identifier.shouldEqual(expected,file,line);
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
	Node[] references;
	this(IdentifierNode n, IdentifierType t, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format;
		assert(t != IdentifierType.Identifier,format("At %s:%s",file,line));
		type = t;
		node = n;
	}
}
struct Identifier
{
	IdentifierNode node;
	Node definition;
	this(IdentifierNode n)
	{
		node = n;
	}
}
private void linkToDefinition(Scope s, ref Identifier i)
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
class Scope
{
	Scope parent;
	Scope[] children;
	Branch branch;
	Node entry;
	/*private*/ Variable[] variables;
	/*private*/ Identifier[] identifiers;
	private Identifier[] globals;
	private int nextFreeIdentifier = -1;
	this(Node e, Scope p = null)
	{
		parent = p;
		entry = e;
		branch = new Branch(this,null,e);
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
	void addVariable(Variable v)
	{
		variables ~= v;
	}
	void addIdentifier(Identifier i)
	{
		this.linkToDefinition(i);
		identifiers ~= i;
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
		import std.algorithm : remove;
		variables = variables.remove!(v => v.node is node);
		node.detach();
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
	void addHint(Hint h)
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
	void toString(scope void delegate(const(char)[]) sink) const
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
}
void findGlobals(Scope scp)
{
	import std.algorithm : canFind, each, filter;
	foreach(s; scp.children)
	{
		s.findGlobals();
		s.globals
			.filter!(id=>!scp.variables.canFind!"a.node.identifier == b"(id.node.identifier))
			.each!(id=>scp.globals ~= id);
	}
	foreach(g; scp.identifiers.filter!(a=>!scp.variables.canFind!"a.node.identifier == b"(a.node.identifier)))
		scp.globals ~= g;
}
@("findGlobals")
unittest
{
	getScope(`var a,b,c;`).assertGlobals([]);
	getScope(`a,b,c;`).assertGlobals(["a","b","c"]);
	auto s = getScope(`var a,b; function e(c){ var d; return a*b*c*d; }`);
	s.assertGlobals([]);
	s.children[0].assertGlobals(["a","b"]);
	s = getScope(`function e(c){ var d; return a*b*c*d; }`);
	s.assertGlobals(["a","b"]);
	s.children[0].assertGlobals(["a","b"]);
	s = getScope(`function bla(a){function c(c){return a*c*d*b}var d,e}`);
	s.assertGlobals(["b"]);
	s.children[0].assertGlobals(["b"]);
	s.children[0].children[0].assertGlobals(["a","d","b"]);
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
}

// TODO: we can use the other generateIdentifier from one of es5-min's branches
string generateIdentifier(int idx)
{
	import std.conv : to;
	if (idx < 26)
		return (cast(char)('a'+idx)).to!string;
	if (idx < 52)
		return (cast(char)('A'+(idx-26))).to!string;
	return generateIdentifier(cast(int)((idx / 52)-1))~generateIdentifier(idx % 52);
}
bool isValidIdentifier(string id)
{
	import es6.lexer;
	return !isReservedKeyword(id);
}
string generateValidIdentifier(int start)
{
	import std.range : iota, front;
	import std.algorithm : find, map;
	return iota(start,int.max).map!(generateIdentifier).find!(isValidIdentifier).front();
}
@("generateIdentifier")
unittest
{
	void assertGeneratedName(int idx, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		generateIdentifier(idx).shouldEqual(expected,file,line);
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
	generateIdentifier((4*52)+14).shouldEqual("do");
	generateValidIdentifier((4*52)+14).shouldEqual("dp");

}