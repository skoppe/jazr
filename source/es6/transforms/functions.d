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
module es6.transforms.functions;

import es6.nodes;
import es6.scopes;
import es6.transforms.conditionals;
import option;
import es6.analyse;
import es6.eval;
import std.range : enumerate, tee;
import std.algorithm : filter, each, remove;
import std.array : array;

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

void hoistFunctions(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	size_t insertionPoint = 0;
	Node entry = scp.entry;
	void hoistFunction(Node node)
	{
		node.detach();
		entry.insertInPlace(insertionPoint++,node);
	}
	void walkTree(Node node)
	{
		foreach(child; node.children)
		{
			switch(child.type)
			{
				case NodeType.ExportDeclarationNode:
				case NodeType.ExportDefaultDeclarationNode:
					if (child.children[0].type == NodeType.FunctionDeclarationNode ||
						child.children[0].type == NodeType.GeneratorDeclarationNode)
					{
						hoistFunction(child);
						break;
					}
					goto default;
				case NodeType.GeneratorDeclarationNode:
				case NodeType.FunctionDeclarationNode:
					hoistFunction(child);
					break;
				default:
					if (!child.startsNewScope)
						walkTree(child);
			}
		}
	}
	walkTree(entry);
}

@("hoistFunctions")
unittest
{
	alias assertHoistFunctions = assertTransformations!(hoistFunctions);

	assertHoistFunctions(
		`function a(){}`,
		`function a(){}`
	);
	assertHoistFunctions(
		`var a = 0; function a(){}`,
		`function a(){}; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; function a(){ if (b) return 5; function c(){}}`,
		`function a(){ function c(){} if (b) return 5; }; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; a = function(){};`,
		`var a = 0; a = function(){};`
	);
	assertHoistFunctions(
		`function *a(){}`,
		`function *a(){}`
	);
	assertHoistFunctions(
		`var a = 0; function *a(){}`,
		`function *a(){}; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; function *a(){ if (b) return 5; function *c(){}}`,
		`function *a(){ function *c(){} if (b) return 5; }; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; a = function*(){};`,
		`var a = 0; a = function*(){};`
	);
	assertHoistFunctions(
		`var a = 0; export function abc() {};`,
		`export function abc() {}; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; export function *abc() {};`,
		`export function *abc() {}; var a = 0;`
	);
	assertHoistFunctions(
		`var a = 0; export { a };`,
		`var a = 0; export { a };`
	);
}

void removeFunctionExpressionUnusedName(Scope scp)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	size_t popped = 0;
	auto funcs = scp.variables
		.enumerate
		.filter!(v => v.value.type == IdentifierType.Function)
		.filter!(v => v.value.references.length == 0)
		.filter!(v => v.value.node.parent.type == NodeType.FunctionExpressionNode)
		.array();
	foreach(fun; funcs)
	{
			auto func = fun.value.node.parent.as!FunctionExpressionNode;
			func.children = func.children[1..$];
			scp.variables = scp.variables.remove(fun.index - popped++);
	}
}

@("removeFunctionExpressionUnusedName")
unittest
{
	alias assertRemoveFuncExprName = assertTransformations!(removeFunctionExpressionUnusedName);

	assertRemoveFuncExprName(
		`a = function bla() { };`,
		`a = function() { };`
	);
	assertRemoveFuncExprName(
		`a = function bla() { }; c = bla;`,
		`a = function bla() { }; c = bla;`
	);
	assertRemoveFuncExprName(
		`a = function bla() { }; c = function hup() { return bla()*2 };`,
		`a = function bla() { }; c = function() { return bla()*2 };`
	);
}

void removeUnusedFunctions(Scope scp)
{
	import std.algorithm : filter;
	import std.array : array;
	bool didWork;
	do {
		didWork = false;
		auto funs = scp.variables.filter!(v => v.type == IdentifierType.Function).array;
		foreach(funName; funs)
		{
			if (funName.node.parent.parent.type == NodeType.ExportDeclarationNode)
				continue;
			if (funName.references.length == 0)
			{
				didWork = true;
				funName.node.parent.shread();
			}
		}
	} while(didWork);
}

@("removeUnusedFunctions")
unittest
{
	alias assertRemoveUnusedFunctions = assertTransformations!(removeUnusedFunctions);

	assertRemoveUnusedFunctions(
		`function a(){}`,
		``
	);
	assertRemoveUnusedFunctions(
		`function a(){}; a();`,
		`function a(){}; a();`
	);
	assertRemoveUnusedFunctions(
		`function a(){};function b(){a()}`,
		``
	);
}




