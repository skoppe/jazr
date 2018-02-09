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
import es6.bench;
import es6.lexer;

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
	bool bench;
	bool selfCheck;
	bool checkLinesAndColumns;
	bool nodeModules;
	bool pretty;
	string fileIn;
	string fileOut;

	size_t size;
	auto result = measure!("Total",(){

		auto helpInformation = getopt(
			args,
			"time", "Show timing information", &time,
			"i|input", "Input file (defaults to stdin)", &fileIn,
			"o|output", "Output file (defaults to stdout)", &fileOut,
			"b|bench", "Lowlevel timings", &bench,
			"c|check", "Perform check on emitted code", &selfCheck,
			"v|verify", "Verify line and column counts", &checkLinesAndColumns,
			"n|node", "Parse input as meant for NodeJS", &nodeModules,
			"p|pretty", "Output prettified", &pretty,
			"minify", "Minify js before outputting (default to false)", &doMinify
		);

		if (helpInformation.helpWanted)
		{
			defaultGetoptPrinter("ECMAScript Tool.\n\nUsage:\t"~baseName(args[0])~" [OPTIONS]\n\nOptions:\n", helpInformation.options);
			return 1;
		}
		int parserFlags = 0;
		if (nodeModules)
			parserFlags |= Parser.Flags.Node;
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
			measure!("Read File", (){
				f.rawRead(buffer[0..fileSize]);
			});
			input = buffer;
			auto parser = parser(input,true,parserFlags);
			parser.scanToken();
			node = measure!("Parsing", () => parser.parseModule());
		}
		else
		{
			input = cast(const(ubyte)[])stdin.byChunk(4096).joiner.array;
			auto parser = parser(input,false,parserFlags);
			parser.scanToken();
			node = measure!("Parsing", () => parser.parseModule());
		}

		// NOTE: no longer works since change to visitor
		//if (checkLinesAndColumns)
			//checkLineAndColumnCounts(cast(string)input);

		size = input.length;
		auto errors = measure!("Collect Errors", (){
			return node.collectErrors();
		});
		if (errors.length > 0)
		{
			reportError(errors[0].as!ErrorNode,input,0);
			return 1;
		}

		if (doMinify)
		{
			node.analyseNode();
			measure!("Minifying",(){node.minify();});
		}

		auto min = measure!("Emitting",(){
			if (pretty)
				return emitVisitor!(true)(node);
			else
				return emitVisitor!(false)(node);
		});

		if (selfCheck)
		{
			if (!measure!("Selfcheck",(){
					auto parser2 = parser(min,parserFlags);
					parser2.scanToken();
					auto node2 = parser2.parseModule();
					auto errors2 = node2.collectErrors();
					if (errors2.length > 0)
					{
						reportError(errors2[0].as!ErrorNode,cast(const(ubyte)[])min,0,"Found error in emitted code:\n");
						return false;
					}
					return true;
				}))
			return 1;
		}
		if (fileOut.length > 0)
		{
			if (fileOut != "/dev/null")
				write(fileOut,min);
		}
		else 
			writeln(min);
		return 0;
	});
	if (result != 0)
		return result;

	if (bench)
		dumpMeasures();
	else if (time)
	{
		auto timing = formatMeasures(["Parsing","Analysing","Minifying","Emitting","Total"]);
		timing ~= format("Size: %d", size);
		auto total = getMeasure("Total");
		auto mbPerSec = 0.9536 * (cast(double)size)/(total / 10);
		import std.array : insertInPlace;
		timing.insertInPlace(0,format("Mb/s: "~mbPerSec.to!string));
		writeln(timing);
	}

	return 0;
}