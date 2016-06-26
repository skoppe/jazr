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
module es6.transformer;

import es6.nodes;

void runTransform(fun...)(Node node, in string file = __FILE__, in size_t line = __LINE__)
{
	import std.typetuple : staticMap;
	import std.functional : unaryFun;
	import std.traits : ReturnType, isBoolean, hasUDA;

	alias _funs = staticMap!(unaryFun, fun);

	Node[] todo = [node];
	bool runNodes(Node node, bool entry = true)
	{
		bool r = false;
		foreach(c; node.children.dup)
		{
			if (c.type == NodeType.FunctionBodyNode)
				todo ~= c;
			else
				r |= runNodes(c,false);
		}
		foreach(_fun; _funs)
		{
			if (_fun(node))
				r |= true;
		}
		return r;
	}
	for (auto i = 0; i < todo.length; i++)
		while(runNodes(todo[i]))
		{
		}
}