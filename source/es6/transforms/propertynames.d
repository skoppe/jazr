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
	if (node.name.type != NodeType.StringLiteralNode)
		return false;

	auto strLit = node.name.as!StringLiteralNode;
	if (strLit.value.length == 0)
		return false;

	auto raw = strLit.getRawValue();
	auto raw2 = raw.toNumber;

	if (raw2.type == ValueType.Numeric)
	{
		node.name.replaceWith(new DecimalLiteralNode(strLit.value));
		return true;
	}

	if (strLit.value[0] >= '0' && strLit.value[0] <= '9')
		return false;

	if (!strLit.value[0].isStartIdentifier)
		return false;

	node.name.replaceWith(new IdentifierNode(strLit.value));
	return true;
}
@("shortenLiteralPropertyNames")
unittest
{
	alias assertShortenLiteralPropname = assertTransformations!(shortenLiteralPropertyNames);

	assertShortenLiteralPropname(
		`var a = {"123":123, "": 65, "123b": 77, "abc": "abc", "null": null };`,
		`var a = { 123: 123, "": 65, "123b": 77, abc: "abc", null: null };`
	);
	assertShortenLiteralPropname(
		"var a = {
			\"&\": \"&amp;\",
			\"<\": \"&lt;\",
			\">\": \"&gt;\",
			'\\\"': \"&quot;\",
			\"'\": \"&#x27;\",
			\"`\": \"&#x60;\"
		}",
		"var a = {
			\"&\": \"&amp;\",
			\"<\": \"&lt;\",
			\">\": \"&gt;\",
			'\\\"': \"&quot;\",
			\"'\": \"&#x27;\",
			\"`\": \"&#x60;\"
        }",
	);

}