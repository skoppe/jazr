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
module es6.reporter;

@safe:

import es6.nodes;

string generateErrorMessage(ErrorNode error, const(char)[] input, int around = 2)
{
	import std.string : lineSplitter;
	import std.algorithm : joiner, max, map;
	import std.range : drop,take;
	import std.uni : isWhite;
	import std.utf : byCodeUnit;
	import std.conv : text;
	import std.format : format;
	auto start = max(0,(cast(int)error.line)-around);
	auto pre = (cast(int)error.line)-start+1;

	auto lines = input.lineSplitter().drop(start).take((around*2)+1);
	auto lead = lines;
	auto tail = lines.drop(pre-1);
	string s = tail.take(1).front.byCodeUnit.take(error.column).map!((c){
			return c.isWhite ? c : ' ';
		}).text;
	
	return format("\n%s\n%s^ %s\n%s at %s:%s",lead.take(pre).joiner("\n"),s,cast(const(char)[])error.value,tail.drop(1).joiner("\n"),error.line,error.column);
}
void reportError(ErrorNode error, const(ubyte)[] input, int around = 2, string prefix = "")
{
	import std.stdio : writeln;
	writeln(prefix,generateErrorMessage(error,cast(const(char)[])input,6));
}