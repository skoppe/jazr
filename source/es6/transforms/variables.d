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
module es6.transforms.variables;

import es6.nodes;
import es6.scopes;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import es6.analyse;
	import unit_threaded;
	import std.stdio;
}
class BookKeepingList
{
	import std.array : Appender;
	private Appender!(Item[]) app;
	struct Item
	{
		string name, shortName;
	}
	void add(string name, string shortName)
	{
		app.put(Item(name,shortName));
	}
	string get(string name)
	{
		import std.range : empty, front, retro;
		import std.algorithm : find;
		auto p = app.data.retro.find!"a.name == b"(name);
		if (p.empty)
			return name;
		return p.front().shortName;
	}
	size_t enter()
	{
		return app.data.length;
	}
	void leave(size_t p)
	{
		app.shrinkTo(p);
	}
}

@("BookKeepingList")
unittest
{
	import unit_threaded;
	BookKeepingList list = new BookKeepingList();
	list.add("x","a");
	list.get("x").shouldEqual("a");
	auto s = list.enter();
	list.add("x","b");
	list.get("x").shouldEqual("b");
	list.leave(s);
	list.get("x").shouldEqual("a");
	s = list.enter();
	list.add("z","c");
	list.get("x").shouldEqual("a");
	list.get("z").shouldEqual("c");
	auto s2 = list.enter();
	list.add("z","a");
	list.get("z").shouldEqual("a");
	list.leave(s2);
	list.get("x").shouldEqual("a");
	list.get("z").shouldEqual("c");
	list.leave(s);
	list.get("x").shouldEqual("a");
}

void shortenVariables(Scope scp, BookKeepingList list = new BookKeepingList())
{
	void renameIdentifiers(Node node, BookKeepingList list)
	{
		if (node.startsNewScope)
			return;
		if (node.type == NodeType.IdentifierNode)
		{
			auto id = node.as!(IdentifierNode);
			id.identifier = list.get(id.identifier);
			return;
		}
		foreach(c; node.children)
			renameIdentifiers(c,list);
	}
	void walkScope(Scope s, BookKeepingList list)
	{
		auto pin = list.enter();
		scope(exit) list.leave(pin);
		int idCounter = 0;
		foreach(v; s.getVariables)
		{
			IdentifierNode node = v.node;
			string id = s.generateFreeIdentifier(idCounter, list);
			list.add(node.identifier,id);
			if (v.type == IdentifierType.Parameter || v.type == IdentifierType.Function)
				node.identifier = id;
		}
		renameIdentifiers(s.entry,list);

		foreach(c; s.children)
			walkScope(c,list);
	}
	foreach (s; scp.children)
		walkScope(s, list);
}
private bool isGlobal(Scope s, string identifier, BookKeepingList list)
{
	import std.algorithm : canFind;
	return s.getGlobals().canFind!((a,b)=>list.get(a.node.identifier) == b)(identifier);
}
private string generateFreeIdentifier(Scope s, ref int idCounter, BookKeepingList list)
{
	string id;
	do
	{
		id = generateValidIdentifier(idCounter++);
	} while(s.isGlobal(id,list));
	return id;
}
@("shortenVariables")
unittest
{
	void assertVariableRenaming(string js, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto root = parseModule(js,file,line);
		auto ar = analyseNode(root);
		ar.scp.findGlobals();
		ar.scp.shortenVariables();
		auto got = root.emit();
		got.shouldEqual(expected,file,line);
	}
	assertVariableRenaming(
		`var x,y,z;`,
		`var x,y,z;`
	);
	assertVariableRenaming(
		`function bla(){var x,y,z}`,
		`function bla(){var a,b,c}`
	);
	assertVariableRenaming(
		`function bla(){var c,b,a}`,
		`function bla(){var a,b,c}`
	);
	assertVariableRenaming(
		`function bla(x){}`,
		`function bla(a){}`
	);
	assertVariableRenaming(
		`function bla(){function w(){return 6}}`,
		`function bla(){function a(){return 6}}`
	);
	assertVariableRenaming(
		`function bla(){function w(){var y;return 6*y}}`,
		`function bla(){function a(){var a;return 6*a}}`
	);
	assertVariableRenaming(
		`function bla(w){function z(){return 6*a*w}}`,
		`function bla(b){function c(){return 6*a*b}}`
	);
	assertVariableRenaming(
		`function bla(x){function w(a){return x*a*y*b}var y,z}`,
		`function bla(a){function c(c){return a*c*d*b}var d,e}`
	);
	assertVariableRenaming(
		`function bla(c,b,a){function w(p){function k(p){return p*2}return a.a*b*f*e*p}function r(h,i,j){return h*i*j*g*e*d}var f,e,d}`,
		`function bla(a,b,c){function d(a){function d(a){return a*2}return c.a*b*f*h*a}function e(a,b,c){return a*b*c*g*h*i}var f,h,i}`
	);
	assertVariableRenaming(
		`function s(o,u){var a=c;t(function(e){var n=t[o][e]})}`,
		`function s(a,b){var d=c;t(function(b){var c=t[a][b]})}`
	);
}
