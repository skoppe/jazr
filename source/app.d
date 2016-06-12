import es6.parser;
import es6.emitter;
import es6.nodes;
import es6.reporter;

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
	bool time;
	bool forceStdin;
	string fileIn;
	string fileOut;

	auto helpInformation = getopt(
		args,
		"time", "Show timing information", &time,
		"i|input", "Input file (defaults to stdin)", &fileIn,
		"o|output", "Output file (defaults to stdout)", &fileOut
	);

	if (helpInformation.helpWanted)
	{
		defaultGetoptPrinter("ECMAScript Tool.\n\nUsage:\t"~baseName(args[0])~" [OPTIONS]\n\nOptions:\n", helpInformation.options);
		return 1;
	}

	import std.datetime : StopWatch;
	StopWatch sw;
	string[] timing;
	Node node;
	string input;
	if (fileIn.length > 0)
	{
		if (!exists(fileIn))
		{
			writeln("File `",fileIn,"` doesn't exist.");
			assert(false);
		}
		sw.start();
		input = readText(fileIn);
		auto parser = parser(input);
		parser.scanToken();
		node = parser.parseModule();
	}
	else
	{
		input = stdin.byChunk(4096).map!(c=>cast(char[])c).joiner.to!string;
		sw.start();
		auto parser = parser(input);
		parser.scanToken();
		node = parser.parseModule();
	}
	sw.stop();
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

	sw.start();
	auto min = emit(node);
	sw.stop();
	if (time)
		timing ~= format("Emitting: "~sw.peek().msecs.to!string~"ms");
	sw.reset();

	if (fileOut.length > 0)
		write(fileOut,min);
	else 
		writeln(min);

	if (time)
		writeln(timing);
	return 0;
}