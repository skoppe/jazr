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

private TickDuration[string] timings;
private size_t[string] calls;

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
		timingCounter(name, sw.peek());
	} catch (Exception)
	{}
	static if (!is(ReturnType!(func) : void))
		return r;
}

auto timingCounter(string name, TickDuration a)
{
	auto t = timings.get(name, TickDuration(0));
	timings[name] = t + a;
	auto c = calls.get(name,0);
	calls[name] = c + 1;
}

auto dumpMeasures()
{
	import std.stdio : writefln;
	foreach(key, value; timings)
	{
		auto c = calls[key];
		if (value.usecs > 9999)
			writefln("%s: %s msecs (%d calls)", key, value.msecs(), c);
		else
			writefln("%s: %s usecs (%d calls)", key, value.usecs(), c);
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
	assert(formatMeasures([]) == []);
	assert(formatMeasures(["test"]) == ["test: 0ms"]);
}