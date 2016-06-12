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
}