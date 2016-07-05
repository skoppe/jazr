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
import std.format : formattedWrite;

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
	this(IdentifierNode n)
	{
		node = n;
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
	void addHint(Hint h)
	{
		hints |= h;
	}
	void remove()
	{
		if (parent is null)
			return;
		parent.removeChild(this);
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
}
void assignBranch(Node n, Branch b)
{
	n.branch = b;
	foreach(c; n.children)
		c.assignBranch(b);
}
T withBranch(T : Node)(T n, Branch b)
{
	n.assignBranch(b);
	return n;
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
import std.regex;
auto reservedKeyword = ctRegex!`^(break|do|in|typeof|case|else|instanceof|var|catch|export|new|void|class|extends|return|while|const|finally|super|with|continue|for|switch|yield|debugger|function|this|default|if|throw|delete|import|try|enum|await|null|true|false)$`;
bool isValidIdentifier(string id)
{
	return id.matchFirst(reservedKeyword).empty();
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