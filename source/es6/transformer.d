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
	import std.traits : fullyQualifiedName;

version (chatty) {
	import std.traits : fullyQualifiedName;
	import std.stdio;
}

version (unittest) {
	import std.stdio;
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
		got.runTransform!(fun)(file,line);
		got.assertTreeInternals(file,line);
		auto diff = diffTree(got,expected);
		if (diff.type == Diff.No)
			return;
		emit(got).shouldEqual(emit(expected),file,line); throw new UnitTestException([diff.getDiffMessage()], file, line);
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
template firstArg(alias fun)
{
	alias firstArg = Parameters!fun[0];
}
template isAstNode(alias N)
{
	enum isAstNode = is(N : Node);
}
template getNodeTypeTuple(AstNode : Node)
{
	import std.typecons : AliasSeq;
	alias getNodeTypeTuple = AliasSeq!(AstNode, getNodeType!AstNode);
}
template isSameType(T, H)
{
	enum isSameType = is(T == H);
}
version (unittest)
{
	private void checkInternals(Node node, in string file = __FILE__, in size_t line = __LINE__)
	{
		import es6.emitter;
		import es6.analyse;
		import unit_threaded;
		node.assertTreeInternals(file,line);
		auto str = emit(node);
		writeln(str);
		auto expected = parseModule(str,file,line);
		expected.analyseNode();
		auto diff = diffTree(node,expected);
		if (diff.type != Diff.No)
			throw new UnitTestException(["Error in transformation",diff.getDiffMessage()], file, line);
	}
}

// TODO: when a transformer replaces the currently iterated node (with a new one), the transformer stops handling any siblings
// and instead starts transformation at the newly inserted node.
// This is inefficient in that the transformer will transform ALL child nodes of the new node. However, at some point those children are already processed.
auto runTransform(fun...)(Node node, in string file = __FILE__, in size_t line = __LINE__)
{
	import std.typetuple : staticMap, Filter, templateAnd, templateNot, NoDuplicates;
	import std.functional : unaryFun;
	import std.traits : ReturnType, isBoolean, hasUDA, allSameType;
	import std.meta : ApplyLeft;

	alias _funs = staticMap!(unaryFun, fun);
	alias _inputTypes = staticMap!(firstArg, _funs);
	alias isTypeofNode = ApplyLeft!(allSameType,Node);
	alias _generalNodeTransforms = NoDuplicates!(Filter!(isTypeofNode,_inputTypes));
	alias _typedNodeTransforms = NoDuplicates!(Filter!(templateAnd!(isAstNode,templateNot!isTypeofNode), _inputTypes));
	alias _otherTransforms = NoDuplicates!(Filter!(templateNot!isAstNode, _inputTypes));

	auto root = node;
	bool runScopes(Scope scp, bool first = true)
	{
		bool r = false;
		foreach(_fun; _funs)
		{
			static if (is(Parameters!_fun[0] : Scope))
			{
				static if (is(ReturnType!_fun : void))
				{
					if (first) {
						version(chatty) { writeln(fullyQualifiedName!_fun); }
						version(unittest) {	writeln(fullyQualifiedName!_fun); }
						_fun(scp);
						version(unittest) {
							root.checkInternals(file,line);
						}
					}
				} else {
					version(chatty) { writeln(fullyQualifiedName!_fun); }
					version(unittest) {	writeln(fullyQualifiedName!_fun); }
					if (_fun(scp))
					{
						r = true;
						version(unittest) {
							root.checkInternals(file,line);
						}
					}
				}
			}
		}
		return r;
	}
	Node transformNode(Node node)
	{
		assert(node !is null);
		bool r = false;
		Node replacedWith;
		switch(node.type)
		{
			foreach(_nodeAstType; _typedNodeTransforms)
			{
				alias _nodeType = getNodeType!(_nodeAstType);
				case _nodeType:
					auto typedNode = node.as!_nodeAstType;
					foreach(_fun; _funs)
					{
						static if (is(firstArg!_fun == _nodeAstType))
						{
							static if (is(ReturnType!_fun : void))
							{
								version(chatty) { writeln(fullyQualifiedName!_fun); }
								version(unittest) {	writeln(fullyQualifiedName!_fun); }
								_fun(typedNode, replacedWith);
								version(unittest) {
									root.checkInternals(file,line);
								}
								if (replacedWith !is null && replacedWith.parent !is node.parent)
									return replacedWith;
								if (typedNode.parent is null)
									return null;
							} else {
								version(chatty) { writeln(fullyQualifiedName!_fun); }
								version(unittest) {	writeln(fullyQualifiedName!_fun); }
								if (_fun(typedNode, replacedWith))
								{
									r = true;
									version (unittest) {
										root.checkInternals(file,line);
									}
									if (replacedWith !is null && replacedWith.parent !is node.parent)
										return replacedWith;
									if (typedNode.parent is null)
										return null;
								}
							}
						}
					}
					break;
			}
			default:
				foreach(_fun; _funs)
				{
					static if (is(firstArg!_fun == Node))
					{
						static if (is(ReturnType!_fun : void))
						{
							version(chatty) { writeln(fullyQualifiedName!_fun); }
							version(unittest) {	writeln(fullyQualifiedName!_fun); }

							_fun(node, replacedWith);
							version(unittest) {
								root.checkInternals(file,line);
							}
							if (replacedWith !is null && replacedWith.parent !is node.parent)
								return replacedWith;
							if (node.parent is null)
								return null;
						} else {
							version(chatty) { writeln(fullyQualifiedName!_fun); }
							version(unittest) {	writeln(fullyQualifiedName!_fun); }
							if (_fun(node, replacedWith))
							{
								r = true;
								version (unittest) {
									root.checkInternals(file,line);
								}
								if (replacedWith !is null && replacedWith.parent !is node.parent)
									return replacedWith;
								if (node.parent is null)
									return null;
							}
						}
					}
				}
		}
		return replacedWith;
	}
	Node transformNodes(Node node)
	{
		assert(node !is null);
		Node replacedWith;
		foreach(c; node.children)
		{
			if (c.startsNewScope)
				continue;
			if (c.parent is null)
				continue;
			replacedWith = transformNodes(c);
			while(replacedWith !is null && replacedWith !is c)
			{
				if (replacedWith.parent !is node)	
				{
					if (replacedWith.parent is null)
						return transformNodes(replacedWith);
					return transformNodes(replacedWith).parent;
				}
				c = replacedWith;
				replacedWith = transformNodes(c);
			}
		}
		replacedWith = node;
		replacedWith = transformNode(node);
		while (replacedWith !is null)
		{
			node = replacedWith;
			replacedWith = transformNode(node);
		}
		return node;
	}
	void transformScope(Scope scp)
	{
		assert(scp !is null);
		foreach(s; scp.children)
			transformScope(s);
		bool first;
		do {
			first = true;
			assert(scp.entry !is null);
			transformNodes(scp.entry);
			while(runScopes(scp, first))
				first = false;
		} while (!first);
	}
	transformScope(node.branch.scp);
}




	// we start at each leaf
	// we run all transforms on each node
	// we collect the replacedWith, and if it has
	// 	we run all transforms on that node
	// only when we are the first/last child do we process the parent
	// 