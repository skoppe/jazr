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
module es6.transforms.propertynames;

import es6.nodes;
import es6.scopes;
import es6.analyse;
import es6.eval;
import es6.lexer;
import es6.transforms.expressions;

version(tracing)
{
	import es6.transformer;
	import std.datetime : StopWatch;
	import es6.bench;
}
version(unittest)
{
	import es6.parser;
	import es6.emitter;
	import unit_threaded;
	import es6.transformer;
	import std.stdio;
}

bool shortenLiteralPropertyNames(PropertyDefinitionNode node, out Node replacedWith)
{
	version(tracing) mixin(traceTransformer!(__FUNCTION__));

	if (node.name.type != NodeType.StringLiteralNode)
		return false;

	auto strLit = node.name.as!StringLiteralNode;
	if (strLit.value.length == 0)
		return false;

	auto raw = strLit.getRawValue();
	auto raw2 = raw.toNumber;

	if (raw2.type == ValueType.Numeric)
	{
		if (strLit.value[0] == '-' || strLit.value[0] == '+')
			return false;
		node.name.replaceWith(new DecimalLiteralNode(strLit.value)).hints.add(Hint.Visited);
		return true;
	}

	if (strLit.value[0] >= '0' && strLit.value[0] <= '9')
		return false;

	size_t idx = strLit.value.getStartIdentifierLength;
	if (idx == 0)
		return false;

	while(idx < strLit.value.length)
	{
		auto len = strLit.value.getTailIdentifierLength(idx);
		if (len == 0)
			return false;
		idx += len;
	}

	node.name.replaceWith(new IdentifierNameNode(strLit.value)).hints.add(Hint.Visited);
	return true;
}
@("shortenLiteralPropertyNames")
unittest
{
	alias assertShortenLiteralPropname = assertTransformations!(shortenLiteralPropertyNames);

	assertShortenLiteralPropname(
		`var a = {"123":123, "": 65, "123b": 77, "abc": "abc", "null": null, "Ä": 5, "a€": 6, "€": 9 };`,
		`var a = { 123: 123, "": 65, "123b": 77, abc: "abc", null: null, Ä: 5, "a€": 6, "€": 9 };`
	);
	assertShortenLiteralPropname(
		"var a = {
			\"&\": \"&amp;\",
			\"<\": \"&lt;\",
			\">\": \"&gt;\",
			'\\\"': \"&quot;\",
			\"'\": \"&#x27;\",
			\"`\": \"&#x60;\",
			\"stuff with spaces\": 4,
			\"stuff-with-nonchars\": 4
		}",
		"var a = {
			\"&\": \"&amp;\",
			\"<\": \"&lt;\",
			\">\": \"&gt;\",
			'\\\"': \"&quot;\",
			\"'\": \"&#x27;\",
			\"`\": \"&#x60;\",
			\"stuff with spaces\": 4,
			\"stuff-with-nonchars\": 4
        }",
	);
}