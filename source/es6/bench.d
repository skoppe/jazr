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

import std.datetime : StopWatch, Clock;
import std.typecons : Yes, Flag;
import core.time : TickDuration;
import std.traits : ReturnType;

version (unittest)
{
	import std.stdio;
}

private long[string] timings;
private bool[string] open;
private size_t[string] calls;
private size_t[string] counters;

long getTick() @trusted
{
	return Clock.currStdTime();
}

auto measure(string name, alias func)()
{
	if (open.get(name, false))
		return func();
	long start = Clock.currStdTime();
	open[name] = true;
	try {
	} catch (Exception)
	{}
	static if (is(ReturnType!(func) : void))
		func();
	else
		auto r = func();
	open[name] = false;
	try {
		stopCounter(name, start);
	} catch (Exception)
	{}
	static if (!is(ReturnType!(func) : void))
		return r;
}

auto startCounter(string name)
{
	if (open.get(name, false))
		return 0;
	open[name] = true;
	return getTick();
}

auto stopCounter(string name, long start)
{
	auto c = calls.get(name, 0);
	calls[name] = c + 1;
	if (start == 0)
		return;
	open[name] = false;
	auto time = Clock.currStdTime() - start;
	auto t = timings.get(name, 0);
	timings[name] = t + time;
}

auto setCounter(string name, size_t counter)
{
	counters[name] = counter;
}

auto dumpMeasures()
{
	import std.stdio : writefln;
	import std.array : array;
	import std.algorithm : sort;
	auto sorted = timings.byKeyValue.array.sort!((a,b) => a.value < b.value);
	foreach(item; sorted)
	{
		auto key = item.key;
		auto value = item.value;
		auto c = calls.get(key,0);
		if (value > 100000)
			writefln("%s: %s msecs (%d calls)", key, value / 10000, c);
		else
			writefln("%s: %s usecs (%d calls)", key, value / 10, c);
	}
	foreach(key, value; counters)
		writefln("%s: %s", key, value);
}

auto formatMeasures(string[] measures)
{
	import std.algorithm : each, map;
	import std.format : format;
	import std.typecons : tuple;
	import std.array : array;
	return measures.map!(f => tuple!("key","value")(f,timings.get(f,0))).map!(t => format("%s: %sms", t.key, t.value / 10000)).array;
}

auto getMeasure(string measure)
{
	return timings.get(measure,0);
}

unittest
{
	measure!("test",(){});
	assert(timings["test"] != 0);
	assert(formatMeasures([]) == []);
	assert(formatMeasures(["test"]) == ["test: 0ms"]);
}
