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
import es6.analyse;
import std.algorithm : map;
import std.array : array;

version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
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
	size_t length()
	{
		return app.data.length;
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

/*void shortenVariables(Scope scp, BookKeepingList list = new BookKeepingList())
{
	void renameIdentifiers(Node node, BookKeepingList list)
	{
		if (node.startsNewScope)
			return;
		if (node.type == NodeType.IdentifierReferenceNode)
		{
			auto id = node.as!(IdentifierReferenceNode);
			id.identifier = list.get(id.identifier);
			return;
		}
		foreach(c; node.children)
			renameIdentifiers(c,list);
	}
	void walkScope(Scope s, BookKeepingList list)
	{
		import std.algorithm : map, canFind;
		import std.array : array;
		auto globals = s.getGlobals().map!(g => list.get(g.node.identifier)).array;
		auto pin = list.enter();
		scope(exit) list.leave(pin);
		int idCounter = 0;
		foreach(v; s.getVariables)
		{
			auto node = v.node;
			string id = s.generateFreeIdentifier(globals, idCounter, list);
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
}*/
struct VariableAlloc
{
	int counter;
	int[] freeList;
}
void shortenVariables(Scope scp)//, BookKeepingList list = new BookKeepingList())
{
	import std.algorithm : map, canFind, sort, reduce, max;
	import std.array : appender;
	auto vars = appender!(Variable[]);
	void collectVars(Scope s)
	{
		vars.put(s.getVariables);
		foreach (c; s.children)
			collectVars(c);
	}
	foreach(c; scp.children)
		collectVars(c);
	auto sortedVars = vars.data.sort!((a,b) => b.references.length < a.references.length);
	struct VariableAlloc
	{
		int counter;
		int[] freeList;
	}
	VariableAlloc[void*] counters;
	foreach(v; sortedVars)
	{
		auto node = v.node;
		Scope s = node.branch.scp;
		VariableAlloc scopeAllocations = counters.get(cast(void*)s,VariableAlloc.init);
		int nestedCounter = reduce!(max)(0,v.references.map!(r => counters.get(cast(void*)r.branch.scp,VariableAlloc.init).counter));
		string id = s.generateFreeIdentifier(nestedCounter, scopeAllocations);
		counters[cast(void*)s] = scopeAllocations;
		node.identifier = cast(const(ubyte)[])id;
		foreach(n; v.references)
			n.as!(IdentifierReferenceNode).identifier = cast(const(ubyte)[])id;
	}
}
private string generateFreeIdentifier(VAlloc)(Scope s, int nestedCounter, ref VAlloc varAlloc)
{
	import std.algorithm : canFind, max, filter, remove;
	import std.range : chain, iota, enumerate;
	string id;
	int cnt;
	auto globals = s.getGlobals().map!(g => g.node.identifier).array;
	auto frees = varAlloc.freeList.enumerate.filter!(i => i.value >= nestedCounter).array;
	foreach(freeItem; frees)
	{
		id = generateValidIdentifier(freeItem.value);
		if (!globals.canFind(id))
		{
			varAlloc.freeList = varAlloc.freeList.remove(freeItem.index);
			return id;
		}
	}
	cnt = max(nestedCounter, varAlloc.counter);
	if (varAlloc.counter != cnt)
	{
		if (varAlloc.freeList.length > 0)
			assert(varAlloc.counter > varAlloc.freeList[$-1]);
		varAlloc.freeList = chain(varAlloc.freeList,iota(varAlloc.counter, cnt)).array();
	}
	for(;;)
	{
		id = generateValidIdentifier(cnt++);
		if (!globals.canFind(id))
			break;
	}
	varAlloc.counter = cnt;
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
		`function bla(x){function w(p){return x*p*y*p*b}var y,z}`,
		`function bla(c){function a(a){return c*a*d*a*b}var d,e}`
	);
	assertVariableRenaming(
		`function bla(c,b,a){function w(p){function k(p){return p*2}return a.a*b*f*e*p}function r(h,i,j){return h*i*j*g*e*d}var f,e,d}`,
		`function bla(f,b,c){function h(e){function f(a){return a*2}return c.a*b*d*a*e}function i(b,c,d){return b*c*d*g*a*e}var d,a,e}`
	);
	assertVariableRenaming(
		`function s(o,u){var a=c;t(function(e){var n=t[o][e]})}`,
		`function s(a,b){var d=c;t(function(b){var c=t[a][b]})}`
	);
	assertVariableRenaming(
		`function s(){var x,y,z=function(t){return t*t*t*t*x*x*x}}`,
		`function s(){var b,a,c=function(a){return a*a*a*a*b*b*b}}`,
	);
	assertVariableRenaming(
		`function a(){var a,b,c,d,e,f,g,h,i;return function(){var d,e,f; return d*d*d*d*e*e*f*a*a*a*b*c}}`,
		`function a(){var e,g,h,a,b,c,d,f,i;return function(){var d,f,i;return d*d*d*d*f*f*i*e*e*e*g*h}}` //TODO: d should be a, e -> b, f -> c, etc. This doens't happen because a is considered a global inside the inner function, and therefor the next free one is d.
	);
	// TODO: All this stuff should also work on let and consts
}
void removeUnusedParameters(Scope scp)
{
	import std.range : retro;
	auto params = scp.variables.retro.onlyParameters;
	foreach(param; params)
	{
		if (param.references.length !=0 )
			return;
		scp.removeVariable(param.node);
	}
}
@("removeUnusedParameters")
unittest
{
	alias assertRemoveUnused = assertTransformations!(removeUnusedParameters);
	assertRemoveUnused(
		`function bla(a, b, c) { return a * b }`,
		"function bla(a, b) { return a * b }"
	);
	assertRemoveUnused(
		`function bla(a, b, c) { return a * c }`,
		"function bla(a, b, c) { return a * c }"
	);
	assertRemoveUnused(
		`function bla(a, b, c) { function n() { return 7 * c } return a * b * n() }`,
		"function bla(a, b, c) { function n() { return 7 * c } return a * b * n() }"
	);
	assertRemoveUnused(
		`function bla(a, b, c) { var d = function () { return 7 * c } return a * b * d() }`,
		"function bla(a, b, c) { var d = function () { return 7 * c } return a * b * d() }"
	);
	assertRemoveUnused(
		`function bla(a, b, c) { return 7 }`,
		"function bla() { return 7 }"
	);
}

void mergeNeighbouringVariableDeclarationStatements(VariableStatementNode stmt, out Node replacedWith)
{
	auto idx = stmt.parent.getIndexOfChild(stmt);
	if (idx == 0)
		return;
	if (stmt.parent.children[idx-1].type != NodeType.VariableStatementNode)
		return;
	auto sibling = stmt.parent.children[idx-1].as!VariableStatementNode;

	sibling.addChildren(stmt.children);
	stmt.detach();
	replacedWith = sibling;
}
void mergeNeighbouringLexicalDeclarationStatements(LexicalDeclarationNode stmt, out Node replacedWith)
{
	auto idx = stmt.parent.getIndexOfChild(stmt);
	if (idx == 0)
		return;
	if (stmt.parent.children[idx-1].type != NodeType.LexicalDeclarationNode)
		return;
	auto sibling = stmt.parent.children[idx-1].as!LexicalDeclarationNode;

	sibling.addChildren(stmt.children);
	stmt.detach();
	replacedWith = sibling;
}

@("mergeNeighbouringVariableDeclarationStatements")
unittest
{
	alias assertMergeVars = assertTransformations!(mergeNeighbouringVariableDeclarationStatements, mergeNeighbouringLexicalDeclarationStatements);
	assertMergeVars(
		`var a; var b;`,
		`var a, b;`
	);
	assertMergeVars(
		`var a; var b; var c;`,
		`var a, b, c;`
	);
	assertMergeVars(
		`var a = 1; var b; var c = 3;`,
		`var a = 1, b, c = 3;`
	);
	assertMergeVars(
		`var a = 1; bla(); var b; var c = 3;`,
		`var a = 1; bla(); var b, c = 3;`
	);
	assertMergeVars(
		`let a; let b;`,
		`let a, b;`
	);
	// TODO: All this stuff should also work on let and consts
}

bool mergeVariableDeclarationStatements(Scope scp)
{
	import std.algorithm : filter;
	import std.array : array;
	bool doneWork = false;
	auto vars = scp.variables.filter!(v => v.type == IdentifierType.Variable).array;
	if (vars.length < 2)
		return doneWork;
	auto firstStmt = vars[0].node.parent.parent;
	foreach(v; vars[1..$])
	{
		auto stmt = v.node.parent.parent;
		if (stmt is firstStmt)
			continue;
		auto varDecl = v.node.parent;
		// either when variable is only child of stmt
		if (stmt.children.length == 1)
		{
			doneWork = true;
			varDecl.detach();
			if (varDecl.children.length == 2)
			{
				auto rhs = varDecl.children[1];
				varDecl.children = varDecl.children[0..1];
				auto lhs = new IdentifierReferenceNode(varDecl.children[0].as!(IdentifierReferenceNode).identifier);
				auto assignOp = new AssignmentOperatorNode(Assignment.Assignment);
				auto assignExpr = new AssignmentExpressionNode([lhs,assignOp,rhs]);
				auto iden = Identifier(lhs,v.node);
				scp.addIdentifier(iden);
				scp.linkToDefinition(iden);
				stmt.replaceWith(assignExpr);
				assignOp.reanalyseHints();
			} else
				stmt.detach();
			firstStmt.addChild(varDecl);
		// or if variables has no initializer
		} else if (varDecl.children.length == 1)
		{
			doneWork = true;
			varDecl.detach();
			firstStmt.addChild(varDecl);
		}
	}
	return doneWork;
}

@("mergeVariableDeclarationStatements")
unittest
{
	alias assertMergeVars = assertTransformations!(mergeVariableDeclarationStatements);

	assertMergeVars(
		`var a; c(); var b;`,
		`var a, b; c();`
	);
	assertMergeVars(
		`var a; c(); var b, d, e;`,
		`var a, b, d, e; c();`
	);
	assertMergeVars(
		`var a; c(); var b = 6;`,
		`var a, b; c(); b = 6;`
	);
	assertMergeVars(
		`var a; if (b) { var c = 5; doBla(); }`,
		`var a, c; if (b) { c = 5; doBla(); }`
	);
	// TODO: All this stuff should also work on let and consts
}

void mergeDuplicateVariableDeclarations(Scope scp)
{
	import std.typecons : tuple;
	import std.algorithm : sort, group, filter, remove, each, find, SwapStrategy;
	import std.range : drop, take;
	auto duplicateVars = scp.variables
		.filter!(v => v.type == IdentifierType.Variable || v.type == IdentifierType.Parameter)
		.map!(v => tuple!("name","node","variable")(v.node.identifier, v.node, v))
		.array
		.sort!((a,b) => a.name < b.name, SwapStrategy.stable)
		.groupBy;
	foreach(duplicateVar; duplicateVars)
	{
		auto firstDecl = duplicateVar.front;
		foreach(duplicate; duplicateVar.drop(1))
		{
			auto varDecl = duplicate.node.parent;
			auto varStmt = varDecl.parent;
			// if the variable declaration item has no initializer
			if (varDecl.children.length == 1)
			{
				if (varStmt.children.length == 1)
				{
					auto parent = varStmt.parent;
					varStmt.detach();
					parent.reanalyseHints();
				}
				else
				{
					auto parent = varDecl.parent;
					varDecl.detach();
					parent.reanalyseHints();
				}
			} else
			{
				auto rhs = varDecl.children[1];
				auto assignOp = new AssignmentOperatorNode(Assignment.Assignment);
				Node[] children = [duplicate.node,assignOp];
				scp.addIdentifier(Identifier(duplicate.node,firstDecl.node));
				scp.variables.find!((ref v){return v.node is firstDecl.node;}).take(1).each!((ref v){v.references ~= duplicate.node;});
				//firstDecl.variable.references ~= duplicate.node;
				if (rhs.type == NodeType.AssignmentExpressionNode)
					children ~= rhs.children;
				else
					children ~= rhs;
				auto assignExpr = new AssignmentExpressionNode(children);
				// if the variable declaration item is only child of the variable statement
				if (varStmt.children.length == 1)
				{
					varDecl.parent.replaceWith(assignExpr);
				} else
				{
					auto idx = varStmt.getIndexOfChild(varDecl);
					// if the variable declaration is the first child of the variable statement
					if (idx == 0)
					{
						varDecl.detach();
						varStmt.insertBefore(assignExpr);
					// if the variable declaration is the middle child of the variable statement
					} else if (idx == varStmt.children.length - 1)
					{
						varDecl.detach();
						varStmt.insertAfter(assignExpr);
					// if the variable declaration is the last child of the variable statement
					} else
					{
						auto transfers = varStmt.children[idx+1..$];
						varStmt.children = varStmt.children[0..idx];
						auto varStmtTail = new VariableStatementNode(transfers);
						varStmt.insertAfter(varStmtTail);
						varStmt.insertAfter(assignExpr);
						varStmtTail.reanalyseHints();
					}
				}
				assignOp.reanalyseHints();
			}
			// update variables state in scope
			foreach(reference; duplicate.variable.references)
			{
				// find identifier, update definition
				auto scp2 = reference.branch.scp;
				auto idx = scp2.identifiers.countUntil!(i => i.node is reference);
				assert(idx != -1);
				scp2.identifiers[idx].definition = firstDecl.node;

			}
			scp.variables = scp.variables.remove!(v => v.node is duplicate.node);
		}
	}
}

@("mergeDuplicateVariableDeclarations")
unittest
{
	alias assertMergeDuplicateVars = assertTransformations!(mergeDuplicateVariableDeclarations);
	assertMergeDuplicateVars(
		`var a = 6; var a;`,
		`var a = 6;`
	);
	assertMergeDuplicateVars(
		`var a = 6; var a = 7;`,
		`var a = 6; a = 7;`
	);
	assertMergeDuplicateVars(
		`var a = 6; var a = b = 88;`,
		`var a = 6; a = b = 88;`
	);
	assertMergeDuplicateVars(
		`var a = 6; var a = b = 88, d;`,
		`var a = 6; a = b = 88; var d;`
	);
	assertMergeDuplicateVars(
		`var a = 6; if (t) var a = b = 88, d;`,
		`var a = 6; if (t) { a = b = 88; var d; }`
	);
	assertMergeDuplicateVars(
		`var a = 6; var b = d(), a = b ? 0 : 1, d;`,
		`var a = 6; var b = d(); a = b ? 0 : 1; var d;`
	);
	assertMergeDuplicateVars(
		`var a = 6; if (t) var b = d(), a = b ? 0 : 1, d;`,
		`var a = 6; if (t) { var b = d(); a = b ? 0 : 1; var d; }`
	);
	assertMergeDuplicateVars(
		`var a = 6; var b = d(), a = b ? 0 : 1;`,
		`var a = 6; var b = d(); a = b ? 0 : 1;`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var a = 6; }`,
		`function bla(a) { a = 6; }`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var a; }`,
		`function bla(a) { }`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var b = 5, a, d = 6; }`,
		`function bla(a) { var b = 5, d = 6; }`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var b = 5, a = 3, d = 6; }`,
		`function bla(a) { var b = 5; a = 3; var d = 6; }`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var b = 5, a = 3; }`,
		`function bla(a) { var b = 5; a = 3; }`
	);
	assertMergeDuplicateVars(
		`function bla(a) { var a = 3, b = 5; }`,
		`function bla(a) { a = 3; var b = 5; }`
	);
	// TODO: All this stuff should also work on let and consts
}


