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
import std.algorithm : map, countUntil, each, filter, remove;
import std.array : array;
import es6.bench;

version(tracing)
{
	import es6.transformer;
	import std.datetime : StopWatch;
	import es6.bench;
}
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

bool isUnusedItem(Range)(Range ranges, int item)
	if (is(ElementType!(Range) : int[]))
{
	import std.algorithm : any, find;
	import std.range : front, empty;
	return !ranges.any!((r){
		auto loc = r.find!(i => i >= item);
		return !loc.empty && loc.front == item;
	});
}

@("isUnusedItem")
unittest
{
	auto rr = [[0,1,2,3,4,8,9,11],[0,2,5,6],[7],[]];
	assert(!isUnusedItem(rr,0));
	assert(!isUnusedItem(rr,1));
	assert(!isUnusedItem(rr,6));
	assert(!isUnusedItem(rr,7));
	assert(isUnusedItem(rr,10));
	assert(isUnusedItem(rr,12));
}

auto findUnusedItem(Range)(Range ranges)
	if (is(ElementType!(Range) : int[]))
{
	import std.range : chain, iota, only, enumerate, empty, front;
	import std.algorithm : map, canFind, sort, reduce, max, filter;
	auto lists = ranges.array;
	foreach(i; iota(0,int.max))
	{
		auto itemInLists = lists.map!(l => !l.empty && l.front == i).enumerate.filter!(a => a.value);
		if (itemInLists.empty) {
			return i;
		} else {
			foreach(list; itemInLists) {
				lists[list.index] = lists[list.index][1..$];
			}
		}

	}
	assert(0);
}
@("findUnusedItem")
unittest
{
	auto rr = [[0,1,2,3,4,8,9,11],[0,2,5,6],[7],[]];
	assert(findUnusedItem(rr) == 10);
	assert(findUnusedItem(rr[1..$]) == 1);
	assert(findUnusedItem(rr[2..$]) == 0);
}
// range has to be sorted, very simple impl. can do better (but only if need be)
auto insertItem(ref int[] range, int item)
{
	import std.array : insertInPlace;
	auto idx = range.countUntil!(i => i > item);
	if (idx == -1)
		idx = range.length;
	range.insertInPlace(idx, item);
}
@("insertItem")
unittest
{
	auto rr = [[0,1,2,3,4,8,9,11],[0,2,5,6],[7],[]];
	rr[0].insertItem(10);
	assert(rr[0] == [0,1,2,3,4,8,9,10,11]);
	rr[1].insertItem(10);
	assert(rr[1] == [0,2,5,6,10]);
	rr[2].insertItem(10);
	assert(rr[2] == [7,10]);
	rr[3].insertItem(10);
	assert(rr[3] == [10]);
}

struct VariableAlloc
{
	int[] usedList;
}
void shortenVariables(Scope scp, string charMap = "nterioaucslfhpdgvmybwxkjqz_$NTERIOAUCSLFHPDGVMYBWXKJQZ")//, BookKeepingList list = new BookKeepingList())
{
	import std.algorithm : map, canFind, sort, reduce, max, filter, any, uniq, partition;
	import std.array : appender, Appender;
	import std.range : chain, iota, only, zip;
	import std.uni : toUpper;

	auto vars = appender!(Variable[]);

	void collectVars(Scope s)
	{
		auto start = vars.data.length;
		vars.put(s.getVariables);
		foreach (c; s.children)
			collectVars(c);
	}
	foreach(c; scp.children)
		collectVars(c);

	/* Here we have some strategies to determine which variables gets which name when renaming them.
	   Funnily enough, when gzipping, the size is minified if we just rename them in the order they
	   appear in the AST, eventhough, some of the other strategies produce smaller minified files.
	*/
	/* 
	// Strategy 1: locals params sorted by index, non locals params sorted by ref-count, anything else sorted by ref-count
	auto others = vars.data.partition!(v => v.type == IdentifierType.Parameter);
	others.sort!"a.references.length > b.references.length";
	auto parameters = vars.data[0..$-others.length];
	auto nonLocals = parameters.partition!(v => !v.isLocal);
	auto locals = parameters[0..$-nonLocals.length];
	locals.sort!((a,b){
		return a.node.getIndex < b.node.getIndex;
	});
	nonLocals.sort!"a.references.length > b.references.length";
	*/

	/*
	// Strategy 2: locals params sorted by index, local variables sorted by index, local functions by ref-count, rest by ref-count
	auto nonLocals = vars.data.partition!(v => v.isLocal);
	auto locals = vars.data[0..$-nonLocals.length];
	auto nonParams = locals.partition!(v => v.type == IdentifierType.Parameter);
	auto funs = nonParams.partition!(v => v.type == IdentifierType.Variable);
	auto variables = nonParams[0..$-funs.length];
	auto parameters = locals[0..$-nonParams.length];
	
	nonLocals.sort!"a.references.length > b.references.length";
	variables.sort!"a.node.getIndex < b.node.getIndex";
	funs.sort!"a.references.length > b.references.length";
	parameters.sort!"a.node.getIndex < b.node.getIndex";
	*/

	/*
	// Strategy 3: locals params sorted by index, local variables sorted by index, rest by ref-count
	auto nonLocals = vars.data.partition!(v => v.isLocal);
	auto locals = vars.data[0..$-nonLocals.length];
	auto nonParams = locals.partition!(v => v.type == IdentifierType.Parameter);
	auto params = locals[0..$-nonParams.length];
	auto nonVariables = nonParams.partition!(v => v.type == IdentifierType.Variable);
	auto variables = nonParams[0..$-nonVariables.length];
	auto rest = vars.data[params.length+variables.length..$];

	params.sort!"a.node.getIndex < b.node.getIndex";
	variables.sort!"a.node.getIndex < b.node.getIndex";
	rest.sort!"a.references.length > b.references.length";
	*/

	/*
	// Strategy 4: assign the most used character to the most used variable (produces smallest minified file)
	vars.data.sort!"a.references.length > b.references.length";
	*/

	auto sortedVars = vars.data;
	VariableAlloc[void*] counters;
	auto scopeSink = appender!(Scope[]);

	foreach(v; sortedVars)
	{
		if (v.definition !is null)
			continue;
		auto node = v.node;
		Scope startingScope = node.branch.scp;
		
		auto referencedScopes = v.references.map!(r => r.branch.scp).uniq;

		alias pluckParent = s => s.parent;
		void getIntermediateScopes(Scopes)(Scopes ss, ref Appender!(Scope[]) sink)
		{
			auto startIdx = sink.data.length;
			foreach(s; ss.filter!(s => !sink.data.canFind(s) && s !is startingScope))
				sink.put(s);
			auto endIdx = sink.data.length;
			if (endIdx == startIdx)
				return;
			getIntermediateScopes(sink.data[startIdx..endIdx].map!pluckParent, sink);
		}

		scopeSink.put(startingScope);
		getIntermediateScopes(referencedScopes, scopeSink);

		auto scopes = scopeSink.data;
		auto allocs = scopes.map!(s => counters.get(cast(void*)s,VariableAlloc.init)).array();

		int item;
		string id;

		int preferredItem = cast(int)node.getIndex();
		if (v.type == IdentifierType.Parameter && allocs.map!(s => s.usedList).isUnusedItem(preferredItem))
			item = preferredItem;
		else
			item = allocs.map!(s => s.usedList).findUnusedItem();

		do {
			id = generateValidIdentifier(item, charMap);

			foreach(ref a; allocs)
				a.usedList.insertItem(item);

			if (!startingScope.getGlobals.map!(g => g.node.identifier).any!(i => i == id))
				break;
			
			item = allocs.map!(s => s.usedList).findUnusedItem();
		} while(true);

		allocs.zip(scopes).each!((z){
			counters[cast(void*)(z[1])] = z[0];
		});

		node.identifier = cast(const(ubyte)[])id;
		foreach(n; v.references)
			n.as!(IdentifierReferenceNode).identifier = cast(const(ubyte)[])id;

		scopeSink.clear();
	}
}
@("shortenVariables")
unittest
{
	void assertVariableRenaming(string js, string expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto root = parseModule(js,true,file,line);
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
		`function bla(){var n,t,e}`
	);
	assertVariableRenaming(
		`function bla(){var e,t,n}`,
		`function bla(){var n,t,e}`
	);
	assertVariableRenaming(
		`function bla(x){}`,
		`function bla(n){}`
	);
	assertVariableRenaming(
		`function bla(t,y,u){var c,b,a}`,
		`function bla(n,t,e){var r,i,o}`
	);
	assertVariableRenaming(
		`function bla(){function w(){return 6}}`,
		`function bla(){function n(){return 6}}`
	);
	assertVariableRenaming(
		`function bla(){function w(){var y;return 6*y}}`,
		`function bla(){function n(){var n;return 6*n}}`
	);
	assertVariableRenaming(
		`function bla(w){function z(){return 6*a*w}}`,
		`function bla(n){function t(){return 6*a*n}}`
	);
	assertVariableRenaming(
		`function bla(x){function w(p){return x*p*y*p*b}var y,z}`,
		`function bla(n){function t(t){return n*t*e*t*b}var e,r}`
	);
	assertVariableRenaming(
		`function bla(c,b,a){function w(p){function k(p){return p*2}return a.a*b*f*e*p}function r(h,i,j){return h*i*j*g*e*d}var f,e,d}`,
		`function bla(n,t,e){function r(n){function r(n){return n*2}return e.a*t*o*a*n}function i(n,t,e){return n*t*e*g*a*u}var o,a,u}`
	);
	assertVariableRenaming(
		`function s(o,u){var a=c;t(function(e){var n=t[o][e]})}`,
		`function s(n,e){var r=c;t(function(e){var r=t[n][e]})}`
	);
	assertVariableRenaming(
		`function s(){var x,y,z=function(t){return t*t*t*t*x*x*x}}`,
		`function s(){var n,t,e=function(t){return t*t*t*t*n*n*n}}`,
	);
	assertVariableRenaming(
		`function a(){var a,b,c,d,e,f,g,h,i;return function(){var d,e,f;return d*d*d*d*e*e*f*a*a*a*b*c}}`,
		`function a(){var n,t,e,r,i,o,a,u,c;return function(){var r,i,o;return r*r*r*r*i*i*o*n*n*n*t*e}}`
	);
	assertVariableRenaming(
		`function bla(s){ var w = [s]; w.on('pre-require', function(x){ x.a = function(){ w[0](x, w, x, x) } }) }`,
		`function bla(n){var t=[n];t.on('pre-require',function(n){n.a=function(){t[0](n,t,n,n)}})}`
	);
	assertVariableRenaming(
		`function bla(suite){ var suites = [suite]; suite.on(function(context){ context.w = function(){ suites[0].c(suites, suites); }; }); };`,
		`function bla(n){var t=[n];n.on(function(n){n.w=function(){t[0].c(t,t)}})}`
	);
	assertVariableRenaming(
		`function bla(suite){ var suites = [suite]; suite.on(function(context){ context.p = context.g; context.s(); context.w = function(){ suites[0].c(suites, suites); }; }); };`,
		`function bla(n){var t=[n];n.on(function(n){n.p=n.g;n.s();n.w=function(){t[0].c(t,t)}})}`
	);
	assertVariableRenaming(
		`function bla(){for(var g in a);}`,
		`function bla(){for(var n in a);}`
	);
	assertVariableRenaming(
		`var a=6;function bla(d){d=a}`,
		`var a=6;function bla(n){n=a}`,
	);
	assertVariableRenaming(
		`function b() { var a; var a; }`,
		`function b(){var n;var n}`
	);
	assertVariableRenaming(
		`function b(a) { var a; }`,
		`function b(n){var n}`
	);
	assertVariableRenaming(
		`function b(a) { for (var a in b); }`,
		`function b(n){for(var n in b);}`
	);
	assertVariableRenaming(
		`function b() { var a; for (var a in b); }`,
		`function b(){var n;for(var n in b);}`
	);
	assertVariableRenaming(
		`function b() { for (var a in b); var a; }`,
		`function b(){for(var n in b);var n}`
	);
	// TODO: All this stuff should also work on let and consts
}
void removeUnusedParameters(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

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
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	auto idx = stmt.parent.getIndexOfChild(stmt);
	if (idx == 0)
		return;
	if (stmt.parent.children[idx-1].type != NodeType.VariableStatementNode)
		return;
	auto sibling = stmt.parent.children[idx-1].as!VariableStatementNode;

	sibling.addChildren(stmt.children);
	sibling.sortOnUnitializedUsage();
	stmt.detach();
	replacedWith = sibling;
}
void mergeNeighbouringLexicalDeclarationStatements(LexicalDeclarationNode stmt, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

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
	assertMergeVars(
		`for (var a = 0; a < 10; a++) { var b = 6; d() };`,
		`for (var a = 0; a < 10; a++) { var b = 6; d() };`
	);
	assertMergeVars(
		`var a; var b; var c;`,
		`var a, b, c;`
	);
	assertMergeVars(
		`var a = 1; var b = 2; var c = 3;`,
		`var a=1, b=2, c=3;`
	);
	// TODO: All this stuff should also work on let and consts
}

bool mergeVariableDeclarationStatements(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	import std.algorithm : filter;
	import std.array : array;
	bool doneWork = false;
	auto vars = scp.variables.filter!(v => v.type == IdentifierType.Variable && v.node.parent.type != NodeType.ForStatementNode).array;
	if (vars.length < 2)
		return doneWork;
	auto firstStmt = vars[0].node.parent.parent;
	auto firstStmtIdx = firstStmt.getIndex();
	foreach(v; vars[1..$])
	{
		auto stmt = v.node.parent.parent;
		if (stmt is firstStmt)
			continue;
		if (v.node.parent.type == NodeType.ForStatementNode)
			continue;
		bool isForLoop = stmt.parent.type == NodeType.ForStatementNode;
		auto varDecl = v.node.parent;
		auto stmtIdx = stmt.getIndex();
		assert(firstStmtIdx < stmtIdx || firstStmt.parent !is stmt.parent);
		auto areNeighbouring = firstStmt.parent is stmt.parent && (stmtIdx - firstStmtIdx) == 1;
		// either when variable is only child of stmt
		if (stmt.children.length == 1 || areNeighbouring)
		{
			doneWork = true;
			varDecl.detach();
			// when there is an initializer
			if (varDecl.children.length == 2 && !areNeighbouring)
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
				if (isForLoop)
					assignExpr.parent.as!(ForStatementNode).loopType = ForLoop.ExprCStyle;
			} else if (stmt.children.length == 0)
			{
				if (isForLoop)
					stmt.parent.as!(ForStatementNode).loopType = ForLoop.ExprCStyle;
				stmt.detach();
			}
			firstStmt.addChild(varDecl);
		// or if variables has no initializer
		} else if (varDecl.children.length == 1)
		{
			doneWork = true;
			varDecl.detach();
			firstStmt.addChild(varDecl);
		}
	}
	if (doneWork)
		firstStmt.as!(VariableStatementNode).sortOnUnitializedUsage();
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
		`var a = 1, b, c; bla(); c = 3;`
	);
	assertMergeVars(
		`var a; for (var b = 0;;);`,
		`var a, b; for (b = 0;;);`
	);
	assertMergeVars(
		`var a, k = 5; for (var c, b = 0;;);`,
		`var a, k = 5, c, b; for (b = 0;;);`
	);
	assertMergeVars(
		`var a; for (var c = 0, b = 0;;);`,
		`var a; for (var c = 0, b = 0;;);`
	);
	assertMergeVars(
		`for (var a = 0; a < 10; a++) { var b = 6; d() };`,
		`for (var a = 0, b; a < 10; a++) { b = 6; d() };`
	);
	assertMergeVars(
		`function a(node){var parents=[];for(;!node;)b();var closest;for(var inst;node;)closest=inst}`,
		`function a(node){var parents=[],closest,inst;for(;!node;)b();for(;node;)closest=inst}`
	);
	assertMergeVars(
		`function nativeKeysIn(object){var result=[];if(object!=null)for(var key in Object(object))result.push(key);return result}`,
		`function nativeKeysIn(object){var result=[];if(object!=null)for(var key in Object(object))result.push(key);return result}`
	);
	assertMergeVars(
		`function checkPropTypes(componentName, propTypes, props, location) { for (var propName in propTypes) { if (propTypes.hasOwnProperty(propName)) { var error; } } }`,
		`function checkPropTypes(componentName,propTypes,props,location){for(var propName in propTypes){if(propTypes.hasOwnProperty(propName)){var error}}}`
	);
	// TODO: All this stuff should also work on let and consts
}

bool mergeDuplicateVariableDeclarations(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	import std.typecons : tuple;
	import std.algorithm : sort, group, filter, remove, each, find, SwapStrategy;
	import std.range : drop, take;
	auto duplicateVars = scp.variables
		.filter!(v => (v.type == IdentifierType.Variable || v.type == IdentifierType.Parameter) && v.node.parent.type != NodeType.ForStatementNode)
		.map!(v => tuple!("name","node","variable")(v.node.identifier, v.node, v))
		.array
		.sort!((a,b) => a.name < b.name, SwapStrategy.stable)
		.groupBy;
	bool didWork = false;
	foreach(duplicateVar; duplicateVars)
	{
		auto firstDecl = duplicateVar.front;
		foreach(duplicate; duplicateVar.drop(1))
		{
			auto varDecl = duplicate.node.parent;
			auto varStmt = varDecl.parent;
			if (varStmt.parent.type == NodeType.ForStatementNode && varStmt.children.length > 1)
				continue;
			didWork = true;
			// if the variable declaration item has no initializer
			if (varDecl.children.length == 1)
			{
				if (varStmt.children.length == 1)
				{
					if (varStmt.parent.type == NodeType.ForStatementNode)
						varStmt.parent.as!(ForStatementNode).loopType = ForLoop.ExprCStyle;
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
				scp.variables.find!((ref v){return v.node is firstDecl.node;}).take(1).each!((ref v){v.insertReference(duplicate.node);});
				//firstDecl.variable.references ~= duplicate.node;
				if (rhs.type == NodeType.AssignmentExpressionNode)
					children ~= rhs.children;
				else
					children ~= rhs;
				auto assignExpr = new AssignmentExpressionNode(children);
				// if the variable declaration item is only child of the variable statement
				if (varStmt.children.length == 1)
				{
					if (varStmt.parent.type == NodeType.ForStatementNode)
						varStmt.parent.as!(ForStatementNode).loopType = ForLoop.ExprCStyle;
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
	return didWork;
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
	assertMergeDuplicateVars(
		`var n; for (var n = 1, r = arguments, a = r.length;;) i(o);`,
        `var n; for (var n = 1, r = arguments, a = r.length;;) i(o);`
	);
	assertMergeDuplicateVars(
		`var n; for (var n = 1;;) i(o);`,
        `var n; for (n = 1;;) i(o);`
	);
	assertMergeDuplicateVars(
		`var n; for (var n;;) i(o);`,
        `var n; for (;;) i(o);`
	);
	assertMergeDuplicateVars(
		`b.merge = function (h, p) { for (var g in h) { } for (var g in p) { } return m; };`,
		`b.merge=function(h,p){for(var g in h){}for(var g in p){}return m};`
	);
	assertMergeDuplicateVars(
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (var a = 0, c = 6; a < 10; a++) bla(a); }`,
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (var a = 0, c = 6; a < 10; a++) bla(a); }`
	);
	// TODO: All this stuff should also work on let and consts
}

bool reuseVariable(Scope scp)
{
	import std.range : enumerate;
	import std.typecons : tuple;
	size_t indexOnParent(Node child, Node parent)
	{
		assert(parent !is null);
		assert(child.parent !is null);
		if (child.parent is parent)
			return child.getIndex();
		return indexOnParent(child.parent,parent);
	}
	struct Helper
	{
		Variable v;
		size_t idx;
		Node firstUse;
		Node lastUse;
		size_t firstIdx;
		size_t lastIdx;
		bool done;
		bool breakUpVar;
	}
	auto items = scp.variables
		.enumerate
		.filter!(v=>v.value.type == IdentifierType.Variable && v.value.isLocal)
		.map!((v){
			auto first = v.value.node;
			auto last = v.value.references.length > 0 ? v.value.references[$-1] : v.value.node;
			auto firstIdx = indexOnParent(first,scp.entry);
			auto lastIdx = indexOnParent(last,scp.entry);
			return Helper(v.value,v.index,first,last,firstIdx,lastIdx);
		}).array();

	bool didWork = false;
	if (items.length < 2)
		return false;

	foreach(idx, ref current; items[0..$-1])
	{
		if (current.done || current.v.definition !is null)
			continue;
		auto candidates = items[idx+1..$];
		foreach(ref candidate; candidates)
		{
			if (candidate.done)
				continue;
			if (current.v.node.identifier == candidate.v.node.identifier)
				continue;
			if (candidate.v.definition !is null)
				continue;
			if (current.lastIdx < candidate.firstIdx)
			{
				// break up var stmt
				if (candidate.v.node.parent.type == NodeType.ForStatementNode)
				{
					candidate.breakUpVar = true;
					// add references to current variable
					scp.variables[current.idx].references ~= candidate.v.node;
					candidate.v.node.parent.as!(ForStatementNode).loopType = ForLoop.ExprIn;
				} else
				{
					auto varDeclNode = candidate.v.node.parent.as!(VariableDeclarationNode);
					if (varDeclNode.children.length == 1)
					{
						//when we keep the var decl we need to set the definition (as it is now a redeclared var)
						continue;
					}
					auto varStmtNode = varDeclNode.parent;
					if (varStmtNode.children.length == 1)
					{
						scp.addIdentifier(Identifier(candidate.v.node, current.v.node));
						candidate.breakUpVar = true;
						// add references to current variable
						scp.variables[current.idx].references ~= candidate.v.node;
						if (varStmtNode.parent.type == NodeType.ForStatementNode)
						{
							varStmtNode.parent.as!(ForStatementNode).loopType = ForLoop.ExprCStyle;
						}
						Node rhs;
						if (varDeclNode.children.length == 1)
						{
							// NOTE: this we don't do this anymore as we bail out early when length == 1
							rhs = new IdentifierReferenceNode(cast(const(ubyte)[])"undefined");
							scp.addIdentifier(Identifier(rhs.as!IdentifierNode));
						}
						else
							rhs = varDeclNode.children[1];

						auto lhs = candidate.v.node;
						auto assignOp = new AssignmentOperatorNode(Assignment.Assignment);
						auto stmt = new AssignmentExpressionNode([lhs,assignOp,rhs]);
						varStmtNode.replaceWith(stmt);
						assignOp.reanalyseHints();
					}
				}
				didWork = true;
				// adjust all identifiers
				candidate.v.node.identifier = current.v.node.identifier;
				foreach(r; candidate.v.references) {
					r.as!(IdentifierNode).identifier = current.v.node.identifier;
				}
				
				assert(scp.variables[current.idx].node is current.v.node);
				assert(scp.variables[candidate.idx].node is candidate.v.node);

				if (!candidate.breakUpVar) {
					scp.variables[candidate.idx].definition = current.v.node;
					scp.variables[current.idx].insertReference(candidate.v.node);
				}

				scp.variables[current.idx].references ~= candidate.v.references;
				scp.variables[candidate.idx].references = [];

				candidate.done = true;
				break;
			}
		}
	}
	size_t offset = 0;
	foreach(candidate; items[1..$])
	{
		if (candidate.done && candidate.breakUpVar)
		{
			// remove candidate variable from scope
			assert(scp.variables[candidate.idx - offset].node is candidate.v.node);
			scp.variables = scp.variables.remove(candidate.idx - offset);
			offset++;
		}
	}
	return didWork;
}

@("reuseVariable")
unittest
{
	alias assertReuseVariable = assertTransformations!(reuseVariable);

	assertReuseVariable(
		`function b(){ var a = 5; doBla(a); var b = 7; doBla(b); }`,
		`function b(){ var a = 5; doBla(a); a = 7; doBla(a); }`
	);
	assertReuseVariable(
		`function b(d,e){ var a = 5; doBla(a); var b = 7; doBla(b); }`,
		`function b(d,e){ var a = 5; doBla(a); a = 7; doBla(a); }`
	);
	assertReuseVariable(
		`function b(){ var a = 5; doBla(a); var b = 7, k = 5; doBla(b); }`,
		`function b(){ var a = 5; doBla(a); var a = 7, k = 5; doBla(a); }`
	);
	assertReuseVariable(
		`function b(){ var a = 5; doBla(a); var b; doBla(b); }`,
		`function b(){ var a = 5; doBla(a); var b; doBla(b); }` // TODO: could do variable inlining here (doBla(void 0))
	);
	assertReuseVariable(
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (var b = 0; b < 10; b++) bla(b); }`,
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (a = 0; a < 10; a++) bla(a); }`
	);
	assertReuseVariable(
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (var b = 0, c = 6; b < 10; b++) bla(b); }`,
		`function b(){ for (var a = 0; a < 10; a++) bla(a); for (var a = 0, c = 6; a < 10; a++) bla(a); }`
	);
	assertReuseVariable(
		`function b(){ for (var a = 0; a < 10; a++) { var b = 7; bla(b); } }`,
		`function b(){ for (var a = 0; a < 10; a++) { var b = 7; bla(b); } }`
	);
	// TODO: here vars oldKeyToIdx, idxInOld, elmToMove, refElm aren't initialized till after removeOnly last use, therefor one can use removeOnly's spot
	/*assertReuseVariable(
		`function updateChildren (parentElm, oldCh, newCh, insertedVnodeQueue, removeOnly) {
    var newStartIdx = 0;
    var oldStartVnode = oldCh[0];
    var newStartVnode = newCh[0];
    var oldKeyToIdx, idxInOld, elmToMove, refElm;

    // removeOnly is a special flag used only by <transition-group>
    // to ensure removed elements stay in correct relative positions
    // during leaving transitions
    var canMove = !removeOnly;

    while (a) {
      if (sameVnode(elmToMove, newStartVnode)) {
        patchVnode(elmToMove, newStartVnode, insertedVnodeQueue);
        oldCh[idxInOld] = undefined;
        canMove && nodeOps.insertBefore(parentElm, newStartVnode.elm, oldStartVnode.elm);
        newStartVnode = newCh[++newStartIdx];
      } else {
        // same key but different element. treat as new element
        createElm(newStartVnode, insertedVnodeQueue, parentElm, oldStartVnode.elm);
        newStartVnode = newCh[++newStartIdx];
      }
    }
  }
  updateChildren();`,
      `function updateChildren(e,n,d,t,o){var r=0,l=n[0],w=d[0],m,h,i,v,u=!o;while(a)w=sameVnode(i,w)?(patchVnode(i,w,t),n[h]=void 0,u&&nodeOps.insertBefore(e,w.elm,l.elm),d[++r]):(createElm(w,t,e,l.elm),d[++r])}updateChildren();`
	);*/
}




