module es6.reporter;

import es6.nodes;

string generateErrorMessage(ErrorNode error, string input, int around = 2)
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
	
	return format("\n%s\n%s^ %s\n%s",lead.take(pre).joiner("\n"),s,error.value,tail.drop(1).joiner("\n"));
}
void reportError(ErrorNode error, string input, int around = 2)
{
	import std.stdio : writeln;
	writeln(generateErrorMessage(error,input,around));
}