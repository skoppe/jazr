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
module es6.bench;

import std.datetime : StopWatch;
import std.typecons : Yes, Flag;
import core.time : TickDuration;
import std.traits : ReturnType;

version (unittest)
{
	import std.stdio;
}

TickDuration[string] timings;

auto measure(string name, alias func)()
{
	StopWatch sw;
	try {
		sw = StopWatch();
		sw.start();
	} catch (Exception)
	{}
	static if (is(ReturnType!(func) : void))
		func();
	else
		auto r = func();
	try {
		TickDuration a = sw.peek();
		auto t = timings.get(name, TickDuration(0));
		timings[name] = t + a;	
	} catch (Exception)
	{}
	static if (!is(ReturnType!(func) : void))
		return r;
}

auto dumpMeasures()
{
	import std.stdio : writefln;
	foreach(key, value; timings)
	{
		if (value.usecs > 9999)
			writefln("%s: %s msecs", key, value.msecs());
		else
			writefln("%s: %s usecs", key, value.usecs());
	}
}

auto formatMeasures(string[] measures)
{
	import std.algorithm : each, map;
	import std.format : format;
	import std.typecons : tuple;
	import std.array : array;
	return measures.map!(f => tuple!("key","value")(f,timings.get(f,TickDuration(0)))).map!(t => format("%s: %sms", t.key, t.value.msecs)).array;
}

auto getMeasure(string measure)
{
	return timings.get(measure,TickDuration(0));
}

unittest
{
	measure!("test",(){});
	assert(timings["test"] != TickDuration(0));
	assert(dumpMeasures([]) == []);
	assert(dumpMeasures(["test"]) == ["test: 0ms"]);
}