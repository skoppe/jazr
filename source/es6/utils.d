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

module es6.utils;

// todo: template constraint hasElement("children") and test if children is array T
T[] removeChildrenAfter(T)(T parent, T element)
{
	import std.algorithm : countUntil;
	// TODO: Need to test this
	T[] r;
	auto idx = parent.children.countUntil!(c => c is element);
	if (idx == -1)
		return r;
	if (idx+1 == parent.children.length)
		return r;
	r = parent.children[idx+1..$];
	parent.children = parent.children[0..idx+1];
	return r;
}