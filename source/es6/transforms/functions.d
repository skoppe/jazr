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
}


