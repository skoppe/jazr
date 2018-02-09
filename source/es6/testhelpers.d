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
module es6.testhelpers;

version(unittest)
{
	import unit_threaded;
	import es6.nodes;
	import std.format : format;
	Type shouldBeOfType(Type)(Node n, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format : format;
		auto d = cast(Type)n;
		if (d is null)
			throw new UnitTestException([format("Expected Node to be of type %s, instead got %s",Type.stringof,n)],file,line);
		return d;
	}
	auto shouldBeObject(T)(T a, T b, in string file = __FILE__, in size_t line = __LINE__)
		if (is(T == class))
	{
		if (a !is b)
			throw new UnitTestException(["Expected two objects to be the same"],file,line);
	}
	auto shouldNotBeObject(T)(T a, T b, in string file = __FILE__, in size_t line = __LINE__)
		if (is(T == class))
	{
		if (a is b)
			throw new UnitTestException(["Expected two objects to not be the same"],file,line);
	}
	void shouldThrowSaying(E)(lazy E expr, string msg, in string file = __FILE__, in size_t line = __LINE__) @trusted
	{
		import std.format : format;	
		import std.algorithm : find;
		import std.range : empty;
		try
		{
			expr();
		}
		catch (UnitTestException e)
		{
			if (find(e.msg, msg).empty)
				throw new UnitTestException([format("Expected message `%s`, got `%s`",msg,e.msg)],file,line);
			return;
		}
		throw new UnitTestException([format("Expected exception with message `%s`",msg)],file,line);
	}
	unittest
	{
		class A {}
		class B {}
		auto a = new A();
		auto a2 = new A();
		a.shouldBeObject(a);
		a.shouldBeObject(a2).shouldThrow();
		(new Node(NodeType.ErrorNode)).shouldBeOfType!A.shouldThrow();
		(){throw new UnitTestException(["bla"]);}().shouldThrowSaying("not bla").shouldThrow();
	}
	void shouldHaveNoErrors(Node root, in string file = __FILE__, in size_t line = __LINE__) @trusted
	{
		auto errors = root.collectErrors();
		if (errors.length > 0)
			throw new UnitTestException([format("Expected no errors but got: [%s]",errors[0].value)],file,line);
	}
	template expect(alias fun)
	{
		void expect(T)(T t)
		{
			fun(t);
		}
	}
}