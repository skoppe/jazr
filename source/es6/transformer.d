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
import es6.scopes;

version (unittest) {
	import std.stdio;
	import std.traits : fullyQualifiedName;
	import es6.parser;
	import es6.analyse;
	import es6.emitter;
	import unit_threaded;

	void assertTransformations(fun...)(string input, string output, in string file = __FILE__, in size_t line = __LINE__)
	{
		Node got = parseModule(input);
		Node expected = parseModule(output);
		got.analyseNode();
		expected.analyseNode();
		got.runTransform!(fun);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected)); throw new UnitTestException([diff.getDiffMessage()], file, line);
	}
}
// NOTE: Can be replaced with `import std.traits : Parameters;` when gdc/ldc support it
import std.traits : isCallable, FunctionTypeOf;
template Parameters(func...)
	if (func.length == 1 && isCallable!func)
{
	static if (is(FunctionTypeOf!func P == function))
		alias Parameters = P;
	else
		static assert(0, "argument has no parameters");
}
void runTransform(fun...)(Node node, in string file = __FILE__, in size_t line = __LINE__)
{
	import std.typetuple : staticMap;
	import std.functional : unaryFun;
	import std.traits : ReturnType, isBoolean, hasUDA;

	alias _funs = staticMap!(unaryFun, fun);

	auto root = node;
	Node[] todo = [node];
	bool runNodes(Node node, bool entry = true, bool first = true)
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
			static if (is(Parameters!_fun[0] : Node))
			{
				static if (is(ReturnType!_fun : void))
				{
					if (first)
						_fun(node);
				} else {
					if (_fun(node))
					{
						version (unittest) {
							import es6.emitter;
							import es6.analyse;
							import unit_threaded;
							auto str = emit(root);
							auto expected = parseModule(str);
							expected.analyseNode();
							auto diff = diffTree(root,expected);
							if (diff.type != Diff.No)
								throw new UnitTestException(["Error in transformation",diff.getDiffMessage()], file, line);
						}
						r |= true;
					}
				}
			} else static if (is(Parameters!_fun[0] : Scope))
			{
				static if (is(ReturnType!_fun : void))
				{
					if (entry && first)
						_fun(node.branch.scp);
				} else {
					if (entry && _fun(node.branch.scp))
					{
						r |= true;
					}
				}
			}
		}
		return r;
	}
	for (auto i = 0; i < todo.length; i++)
	{
		bool entry = true, first = true;
		while(runNodes(todo[i], entry, first))
			first = false;
	}
}