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
import es6.parser;
import es6.emitter;
import es6.nodes;
import es6.reporter;
import es6.minifier;
import es6.analyse;

import std.getopt;
import std.stdio;

int main(string[] args)
{
	import std.stdio : stdin;
	import std.file : readText, write, exists;
	import std.algorithm : joiner, map;
	import std.stdio : writeln;
	import std.conv : text, to;
	import std.algorithm : remove;
	import std.path : baseName;
	import std.format : format;
	import std.datetime : StopWatch;
	bool time;
	bool forceStdin;
	bool doMinify;
	string fileIn;
	string fileOut;

	StopWatch swTotal;
	swTotal.start();

	auto helpInformation = getopt(
		args,
		"time", "Show timing information", &time,
		"i|input", "Input file (defaults to stdin)", &fileIn,
		"o|output", "Output file (defaults to stdout)", &fileOut,
		"minify", "Minify js before outputting (default to false)", &doMinify
	);

	if (helpInformation.helpWanted)
	{
		defaultGetoptPrinter("ECMAScript Tool.\n\nUsage:\t"~baseName(args[0])~" [OPTIONS]\n\nOptions:\n", helpInformation.options);
		return 1;
	}

	StopWatch sw;
	string[] timing;
	Node node;
	const(ubyte)[] input;
	if (fileIn.length > 0)
	{
		if (!exists(fileIn))
		{
			writeln("File `",fileIn,"` doesn't exist.");
			return 1;
		}
		auto f = File(fileIn, "r");
		auto fileSize = f.size();
		if (fileSize == 0)
		{
			writeln("File `",fileIn,"` is empty.");
			return 1;
		}
		ubyte[] buffer = new ubyte[fileSize+16];
		buffer[fileSize..$] = 0;
		f.rawRead(buffer[0..fileSize]);
		input = buffer;
		sw.start();
		auto parser = parser(input,true);
		parser.scanToken();
		node = parser.parseModule();
	}
	else
	{
		input = cast(const(ubyte)[])stdin.byChunk(4096).map!(c=>cast(char[])c).joiner.to!string;
		sw.start();
		auto parser = parser(input);
		parser.scanToken();
		node = parser.parseModule();
	}
	sw.stop();
	size_t size = input.length;

	if (time)
		timing ~= format("Parsing: "~sw.peek().msecs.to!string~"ms");
	sw.reset();

	auto errors = node.collectErrors();
	if (errors.length > 0)
	{
		reportError(errors[0].as!ErrorNode,input,0);
		if (time)
			writeln(timing);
		return 1;
	}

	if (doMinify)
	{
		sw.start();
		node.analyseNode();
		sw.stop();
		if (time)
			timing ~= format("Analysing: "~sw.peek().msecs.to!string~"ms");
		sw.reset();

		sw.start();
		node.minify();
		sw.stop();
		if (time)
			timing ~= format("Minifying: "~sw.peek().msecs.to!string~"ms");
		sw.reset();
	}

	sw.start();
	auto min = emit(node);
	sw.stop();
	if (time)
		timing ~= format("Emitting: "~sw.peek().msecs.to!string~"ms");
	sw.reset();

	if (fileOut.length > 0)
	{
		if (fileOut != "/dev/null")
			write(fileOut,min);
	}
	else 
		writeln(min);

	if (time)
	{
		timing ~= format("Total: "~swTotal.peek().msecs.to!string~"ms");
		timing ~= format("Size: %d", size);
		auto mbPerSec = 0.9536 * (cast(double)size)/swTotal.peek().usecs;
		import std.array : insertInPlace;
		timing.insertInPlace(0,format("Mb/s: "~mbPerSec.to!string));
	}


	if (time)
		writeln(timing);
	return 0;
}