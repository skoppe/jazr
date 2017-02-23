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
module es6.eval;

import es6.nodes;
import es6.lexer;

version (unittest)
{
	import unit_threaded;
	import es6.nodes;
	import es6.parser;
	import es6.testhelpers;

	void assertEvalBinaryExpression(string input, ValueType type, string expected = "", in string file = __FILE__, in size_t line = __LINE__)
	{
		auto p = parser(input);
		p.scanToken();
		auto node = p.parseRightHandSideExpression().as!BinaryExpressionNode;
		auto raw = node.resolveBinaryExpression();
		raw.type.shouldEqual(type,file,line);

		if (raw.type == ValueType.NotKnownAtCompileTime)
			return;
		raw.value.shouldEqual(expected,file,line);
	}
	void assertExpressionToTernary(string input, Ternary expected, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto p = parser(input);
		p.scanToken();
		auto node = p.parseRightHandSideExpression();
		auto raw = node.getRawValue();
		if (raw.type == ValueType.NotKnownAtCompileTime)
			return expected.shouldEqual(Ternary.None,file,line);
		(raw.coerceAsBoolean ? Ternary.True : Ternary.False).shouldEqual(expected,file,line);
	}
}

enum ValueType
{
	NotKnownAtCompileTime,
	Undefined,
	NaN,
	Infinity,
	Null,
	Bool,
	String,
	Numeric,
	Object
}
enum Ternary
{
	True,
	False,
	None
}
struct RawValue
{
	const(ubyte)[] value;
	ValueType type;
	void setValue(string v)
	{
		this.value = cast(const(ubyte)[])v;
	}
	static RawValue from(string value, ValueType type)
	{
		return RawValue(cast(const(ubyte)[])value, type);
	}
	auto to(Type)()
	{
		import std.conv : to;
		return (cast(const(char)[])this.value).to!Type;
	}
}
auto processPrefixExpressions(RawValue raw, Node[] prefixs)
{
	import std.conv : to;
	import std.range : retro;
	import std.algorithm : all;
	assert(prefixs.all!(a=>a.type == NodeType.PrefixExpressionNode));
	foreach (p; prefixs.retro)
	{
		PrefixExpressionNode prefix = p.as!PrefixExpressionNode;

		final switch (prefix.prefix)
		{
			case Prefix.Increment:
			case Prefix.Decrement: raw.type = ValueType.NotKnownAtCompileTime; break;
			case Prefix.Void: raw.type = ValueType.Undefined; raw.setValue("undefined"); break;
			case Prefix.Positive:
				return raw.toNumber();
			case Prefix.Negative:
				final switch (raw.type)
				{
					case ValueType.Undefined:
					case ValueType.NaN: raw.type = ValueType.NaN; raw.setValue("NaN"); break;
					case ValueType.Infinity: raw.setValue("-Infinity"); break;
					case ValueType.Null: raw.type = ValueType.Numeric; raw.setValue("-0"); break;
					case ValueType.Bool: raw.type = ValueType.Numeric; raw.setValue(raw.value == "true" ? "-1" : "-0"); break;
					case ValueType.String:
						raw.type = ValueType.Numeric;
						if (raw.value.length == 0)
						{
							raw.setValue("-0");
						} else
						{
							try
							{
								int t = raw.to!int;
								if (raw.value[0] == '-')
									raw.setValue((-t).to!string);
								else
									raw.setValue("-"~t.to!string);
							} catch (Exception e)
							{
								raw.type = ValueType.NaN;
								raw.setValue("NaN");
							}
						}
						break;
					case ValueType.Numeric: raw.value = (cast(const(ubyte)[])"-")~raw.value; break;
					case ValueType.NotKnownAtCompileTime: break;
					case ValueType.Object: raw.setValue("NaN"); raw.type = ValueType.NaN; break;
				}
				break;
			case Prefix.Typeof:
				final switch (raw.type)
				{
					case ValueType.Undefined: raw.type = ValueType.String; raw.setValue(`undefined`); break;
					case ValueType.NaN: raw.type = ValueType.String; raw.setValue(`number`); break;
					case ValueType.Infinity: raw.type = ValueType.String; raw.setValue(`number`); break;
					case ValueType.Null: raw.type = ValueType.String; raw.setValue(`object`); break;
					case ValueType.Bool: raw.type = ValueType.String; raw.setValue(`boolean`); break;
					case ValueType.String: raw.type = ValueType.String; raw.setValue(`string`); break;
					case ValueType.Numeric: raw.type = ValueType.String; raw.setValue(`number`); break;
					case ValueType.NotKnownAtCompileTime: break;
					case ValueType.Object: raw.setValue(`object`); raw.type = ValueType.String; break;
				}
				break;
			case Prefix.Delete:
				final switch (raw.type)
				{
					case ValueType.Undefined:
					case ValueType.NaN:
					case ValueType.Infinity: raw.type = ValueType.Bool; raw.setValue("false"); break;
					case ValueType.Object:
					case ValueType.Null:
					case ValueType.Bool:
					case ValueType.String:
					case ValueType.Numeric: raw.type = ValueType.Bool; raw.setValue("true"); break;
					case ValueType.NotKnownAtCompileTime: break;
				}
				break;
			case Prefix.Tilde:
				final switch (raw.type)
				{
					case ValueType.Object:
					case ValueType.Undefined:
					case ValueType.NaN:
					case ValueType.Infinity:
					case ValueType.Null: raw.type = ValueType.Numeric; raw.setValue("-1"); break;
					case ValueType.Bool: raw.type = ValueType.Numeric; raw.setValue(raw.value == "true" ? "-2" : "-1"); break;
					case ValueType.String:
						raw.type = ValueType.Numeric;
						try
						{
							auto t = raw.to!int + 1;
							t = -t;
							raw.setValue(t.to!string);
						} catch (Exception e)
						{
							raw.setValue("-1");
						}
						break;
					case ValueType.Numeric: raw.type = ValueType.Numeric; raw.setValue((-(raw.to!int + 1)).to!string); break;
					case ValueType.NotKnownAtCompileTime: break;
				}
				break;
			case Prefix.Negation:
				final switch (raw.type)
				{
					case ValueType.Object:
					case ValueType.Infinity: raw.type = ValueType.Bool; raw.setValue("false"); break;
					case ValueType.Undefined:
					case ValueType.NaN:
					case ValueType.Null: raw.type = ValueType.Bool; raw.setValue("true"); break;
					case ValueType.Bool: raw.type = ValueType.Bool; raw.setValue(raw.value == "true" ? "false" : "true"); break;
					case ValueType.String: raw.type = ValueType.Bool; raw.setValue(raw.value.length == 0 ? "true" : "false"); break;
					case ValueType.Numeric: raw.type = ValueType.Bool; raw.setValue((raw.value == "0" || raw.value == "-0") ? "true" : "false"); break;
					case ValueType.NotKnownAtCompileTime: break;
				}
				break;
		}
	}
	return raw;
}

RawValue getRawValue(Node node)
{
	switch(node.type)
	{
		case NodeType.UnaryExpressionNode:
			auto unary = node.as!UnaryExpressionNode;
			return getRawValue(node.children[0]).processPrefixExpressions(unary.prefixs);
		case NodeType.IdentifierReferenceNode:
			switch (cast(const(char)[])node.as!(IdentifierReferenceNode).identifier)
			{
				case "undefined": return RawValue.from("undefined",ValueType.Undefined);
				case "NaN": return RawValue.from("NaN",ValueType.NaN);
				case "Infinity": return RawValue.from("Infinity",ValueType.Infinity);
				case "null": return RawValue.from("null",ValueType.Null);
				default: break;
			}
			break;
		case NodeType.KeywordNode:
			if (node.as!(KeywordNode).keyword == Keyword.Null)
				return RawValue.from("null",ValueType.Null);
			break;
		case NodeType.BooleanNode: return RawValue.from(node.as!(BooleanNode).value ? "true" : "false",ValueType.Bool);
		case NodeType.StringLiteralNode: return RawValue(node.as!(StringLiteralNode).value,ValueType.String);
		case NodeType.DecimalLiteralNode: return RawValue(node.as!(DecimalLiteralNode).value,ValueType.Numeric);
		case NodeType.ObjectLiteralNode: return RawValue.from("",ValueType.Object);
		case NodeType.BinaryExpressionNode:
			return resolveBinaryExpression(node.as!BinaryExpressionNode);
		default: break;
	}
	return RawValue.from("",ValueType.NotKnownAtCompileTime);
}

RawValue toNumber(RawValue raw)
{
	import std.conv : to;
	final switch (raw.type)
	{
		case ValueType.NotKnownAtCompileTime:
		case ValueType.Numeric:
		case ValueType.NaN:
		case ValueType.Infinity: break;
		case ValueType.Undefined: raw.type = ValueType.NaN; raw.setValue("NaN"); break;
		case ValueType.Null: raw.type = ValueType.Numeric; raw.setValue("0"); break;
		case ValueType.Bool: raw.type = ValueType.Numeric; raw.setValue(raw.value == "true" ? "1" : "0"); break;
		case ValueType.String:
			raw.type = ValueType.Numeric;
			if (raw.value.length == 0)
				raw.setValue("0");
			else
			{
				try
				{
					int t = raw.to!int;
					raw.setValue(t.to!string);
				} catch (Exception e)
				{
					raw.type = ValueType.NaN;
					raw.setValue("NaN");
				}
			}
			break;
		case ValueType.Object: return raw.toPrimitive.toNumber;
	}
	return raw;
}
auto toPrimitive(RawValue raw)
{
	if (raw.type != ValueType.Object)
		return raw;
	//  if valueOf returns a primitive, return it. Otherwise if toString returns a primitive return it. Otherwise throw an error
	version(unittest){throw new UnitTestException(["cannot coerceAsBoolean"]);}assert(0);
}
@("processPrefixExpressions")
unittest
{
	void assertProcessPrefixExpressions(string input, ValueType expectedType, string expectedValue, in string file = __FILE__, in size_t line = __LINE__)
	{
		auto parser = parser(input);
		parser.scanToken();
		auto node = parser.parseUnaryExpression();
		auto raw = node.getRawValue();
		raw.type.shouldEqual(expectedType,file,line);
		raw.value.shouldEqual(expectedValue,file,line);
	}
	assertProcessPrefixExpressions(`true`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`false`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`0`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`""`,ValueType.String,``);
	assertProcessPrefixExpressions(`null`,ValueType.Null,`null`);
	assertProcessPrefixExpressions(`NaN`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`undefined`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`Infinity`,ValueType.Infinity,`Infinity`);

	assertProcessPrefixExpressions(`!true`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!false`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!0`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!-1`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!1`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!""`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!"0"`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!"1"`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!"-1"`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!"asdf"`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!null`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!NaN`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!undefined`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`!Infinity`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`!a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`!{}`,ValueType.Bool,"false");

	assertProcessPrefixExpressions(`~true`,ValueType.Numeric,`-2`);
	assertProcessPrefixExpressions(`~false`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~0`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~-1`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`~1`,ValueType.Numeric,`-2`);
	assertProcessPrefixExpressions(`~""`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~"0"`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~"1"`,ValueType.Numeric,`-2`);
	assertProcessPrefixExpressions(`~"-1"`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`~"asdf"`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~null`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~NaN`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~undefined`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~Infinity`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`~a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`~{}`,ValueType.Numeric,"-1");

	assertProcessPrefixExpressions(`-true`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`-false`,ValueType.Numeric,`-0`);
	assertProcessPrefixExpressions(`-0`,ValueType.Numeric,`-0`);
	assertProcessPrefixExpressions(`-1`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`-""`,ValueType.Numeric,`-0`);
	assertProcessPrefixExpressions(`-"0"`,ValueType.Numeric,`-0`);
	assertProcessPrefixExpressions(`-"1"`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`-"-1"`,ValueType.Numeric,`1`);
	assertProcessPrefixExpressions(`-"--1"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`-"+1"`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`-"++1"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`-"asdf"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`-null`,ValueType.Numeric,`-0`);
	assertProcessPrefixExpressions(`-NaN`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`-undefined`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`-Infinity`,ValueType.Infinity,`-Infinity`);
	assertProcessPrefixExpressions(`-a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`-{}`,ValueType.NaN,"NaN");

	assertProcessPrefixExpressions(`+true`,ValueType.Numeric,`1`);
	assertProcessPrefixExpressions(`+false`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`+0`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`+-1`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`+1`,ValueType.Numeric,`1`);
	assertProcessPrefixExpressions(`+""`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`+"0"`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`+"1"`,ValueType.Numeric,`1`);
	assertProcessPrefixExpressions(`+"-1"`,ValueType.Numeric,`-1`);
	assertProcessPrefixExpressions(`+"--1"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`+"+1"`,ValueType.Numeric,`1`);
	assertProcessPrefixExpressions(`+"++1"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`+"asdf"`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`+null`,ValueType.Numeric,`0`);
	assertProcessPrefixExpressions(`+NaN`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`+undefined`,ValueType.NaN,`NaN`);
	assertProcessPrefixExpressions(`+Infinity`,ValueType.Infinity,`Infinity`);
	assertProcessPrefixExpressions(`+a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`+{}`,ValueType.NaN,"NaN").shouldThrow();

	assertProcessPrefixExpressions(`typeof true`,ValueType.String,`boolean`);
	assertProcessPrefixExpressions(`typeof false`,ValueType.String,`boolean`);
	assertProcessPrefixExpressions(`typeof 0`,ValueType.String,`number`);
	assertProcessPrefixExpressions(`typeof -1`,ValueType.String,`number`);
	assertProcessPrefixExpressions(`typeof 1`,ValueType.String,`number`);
	assertProcessPrefixExpressions(`typeof ""`,ValueType.String,`string`);
	assertProcessPrefixExpressions(`typeof "0"`,ValueType.String,`string`);
	assertProcessPrefixExpressions(`typeof "1"`,ValueType.String,`string`);
	assertProcessPrefixExpressions(`typeof "-1"`,ValueType.String,`string`);
	assertProcessPrefixExpressions(`typeof "asdf"`,ValueType.String,`string`);
	assertProcessPrefixExpressions(`typeof null`,ValueType.String,`object`);
	assertProcessPrefixExpressions(`typeof NaN`,ValueType.String,`number`);
	assertProcessPrefixExpressions(`typeof undefined`,ValueType.String,`undefined`);
	assertProcessPrefixExpressions(`typeof Infinity`,ValueType.String,`number`);
	assertProcessPrefixExpressions(`typeof a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`typeof {}`,ValueType.String,`object`);

	assertProcessPrefixExpressions(`void true`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void false`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void 0`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void 1`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void ""`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void "0"`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void "1"`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void "-1"`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void "asdf"`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void null`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void NaN`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void undefined`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void Infinity`,ValueType.Undefined,`undefined`);
	assertProcessPrefixExpressions(`void a`,ValueType.Undefined,"undefined");
	assertProcessPrefixExpressions(`void {}`,ValueType.Undefined,`undefined`);

	assertProcessPrefixExpressions(`delete true`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete false`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete 0`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete 1`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete -1`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete ""`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete "0"`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete "1"`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete "-1"`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete "asdf"`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete null`,ValueType.Bool,`true`);
	assertProcessPrefixExpressions(`delete NaN`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`delete undefined`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`delete Infinity`,ValueType.Bool,`false`);
	assertProcessPrefixExpressions(`delete a`,ValueType.NotKnownAtCompileTime,"");
	assertProcessPrefixExpressions(`delete {}`,ValueType.Bool,`true`);
}

bool coerceAsBoolean(RawValue raw)
{
	final switch (raw.type)
	{
		case ValueType.Undefined: return false;
		case ValueType.NaN: return false;
		case ValueType.Infinity: return true;
		case ValueType.Null: return false;
		case ValueType.Bool: return raw.value == "true" ? true : false;
		case ValueType.String: return raw.value.length == 0 ? false : true;
		case ValueType.Numeric: return (raw.value == "0" || raw.value == "-0") ? false : true;
		case ValueType.Object:
		case ValueType.NotKnownAtCompileTime: version(unittest){throw new UnitTestException(["cannot coerceAsBoolean"]);}assert(0);
	}
}
@("coerceAsBoolean")
unittest
{
	import unit_threaded;
	RawValue.from("",ValueType.Object).coerceAsBoolean.shouldThrow();
}
Ternary coerceToTernary(Node node)
{
	switch (node.type)
	{
		case NodeType.BooleanNode:
		case NodeType.StringLiteralNode:
		case NodeType.DecimalLiteralNode:
		case NodeType.ObjectLiteralNode:
		case NodeType.UnaryExpressionNode:
		case NodeType.BinaryExpressionNode:
			auto raw = node.getRawValue();
			if (raw.type == ValueType.NotKnownAtCompileTime || raw.type == ValueType.Object)
				return Ternary.None;
			return raw.coerceAsBoolean ? Ternary.True : Ternary.False;
		case NodeType.ExpressionNode:
			return node.children[$-1].coerceToTernary();
		case NodeType.ParenthesisNode:
			return node.children[$-1].coerceToTernary();
		default: return Ternary.None;
	}
}
@("toPrimitive identity feature")
unittest
{
	RawValue.from("Test",ValueType.Undefined).toPrimitive.shouldEqual(RawValue.from("Test",ValueType.Undefined));
}
bool doStrictEquality(alias type = false)(RawValue a, RawValue b, bool invert)
{
	bool r = doStrictEquality!type(a,b);
	if (invert)
		return !r;
	return r;
}
bool doStrictEquality(alias type = false)(RawValue a, RawValue b)
{
	// NOTE: can be replaced with `import std.traits : isExpressions` when gdc/ldc support it
	template isExpressions(T ...)
	{
		static if (T.length >= 2)
			enum bool isExpressions =
				isExpressions!(T[0 .. $/2]) &&
				isExpressions!(T[$/2 .. $]);
		else static if (T.length == 1)
			enum bool isExpressions =
				!is(T[0]) && __traits(compiles, { auto ex = T[0]; });
		else
			enum bool isExpressions = true; // default
	}
	import std.traits : isBoolean;
	if (a.type == ValueType.Undefined || a.type == ValueType.Null)
		return (b.type == ValueType.Undefined || b.type == ValueType.Null);
	if (a.type != b.type || a.type == ValueType.NaN || b.type == ValueType.NaN)
		return false;
	static if (isExpressions!(type) && isBoolean!(typeof(type)) && type == false)
	{
		switch (a.type)
		{
			case ValueType.Numeric:
			case ValueType.String:
			case ValueType.Bool:
				return a.value == b.value;
			case ValueType.Object:
			default: version(unittest){throw new UnitTestException(["Not supported"]);} assert(0,"Not supported"); // Note: the equality is true when a and b reference the same object, but we don't support it now
		}
	} else
	{
		assert(a.type == b.type);
		assert(b.type == b.type);
		static if (type == ValueType.Numeric || type == ValueType.String || type == ValueType.Bool)
		{
			return a.value == b.value;
		} else static if (type == ValueType.Object)
		{
			static assert(false, "Not supported"); // Note: the equality is true when a and b reference the same object, but we don't support it now
		} else
			return false;
	}
}
@("doStrictEquality")
unittest
{
	doStrictEquality(RawValue.from("",ValueType.Object),RawValue.from("",ValueType.Object)).shouldThrow;
}

bool doWeakEquality(RawValue a, RawValue b, bool invert)
{
	bool r = doWeakEquality(a,b);
	if (invert)
		return !r;
	return r;
}
bool doWeakEquality(RawValue a, RawValue b)
{
	if (a.type == b.type)
		return doStrictEquality(a,b);
	if (a.type == ValueType.Null || a.type == ValueType.Undefined)
		return (b.type == ValueType.Undefined || b.type == ValueType.Null);
	if (b.type == ValueType.Bool)
		return doStrictEquality!(ValueType.Numeric)(a,b.toNumber);
	if (a.type == ValueType.Numeric)
	{
		if (b.type == ValueType.String)
			return doStrictEquality!(ValueType.Numeric)(a,b.toNumber);
		else if (b.type == ValueType.Object)
			return doStrictEquality!(ValueType.Numeric)(a,b.toPrimitive);
	} else if (a.type == ValueType.String)
	{
		if (b.type == ValueType.Numeric)
			return doStrictEquality!(ValueType.Numeric)(a.toNumber,b);
		else if (b.type == ValueType.Object)
			return doStrictEquality!(ValueType.String)(a,b.toPrimitive);
	} else if (a.type == ValueType.Bool)
		return doWeakEquality(a.toNumber,b);
	else if (a.type == ValueType.Object)
	{
		if (b.type == ValueType.String)
			return doStrictEquality!(ValueType.String)(b,a.toPrimitive);
		else if (b.type == ValueType.Numeric)
			return doStrictEquality!(ValueType.Numeric)(b,a.toPrimitive);
	}
	return false;
}
@("Test WeakEquality with objects")
unittest
{
	doWeakEquality(RawValue.from("{}",ValueType.Object),RawValue.from(`"str"`,ValueType.String)).shouldThrow;
	doWeakEquality(RawValue.from(`"str"`,ValueType.String),RawValue.from("{}",ValueType.Object)).shouldThrow;
	doWeakEquality(RawValue.from("{}",ValueType.Object),RawValue.from(`6`,ValueType.Numeric)).shouldThrow;
	doWeakEquality(RawValue.from(`6`,ValueType.Numeric),RawValue.from("{}",ValueType.Object)).shouldThrow;
	doWeakEquality(RawValue.from(`6`,ValueType.Numeric),RawValue.from("{}",ValueType.NotKnownAtCompileTime)).shouldEqual(false);
	doWeakEquality(RawValue.from("{}",ValueType.NotKnownAtCompileTime),RawValue.from(`6`,ValueType.Numeric)).shouldEqual(false);
	doWeakEquality(RawValue.from("-1",ValueType.Numeric),RawValue.from("-1",ValueType.String)).shouldEqual(true);
}
auto toRawValue(bool v)
{
	return v ? RawValue.from("true",ValueType.Bool) : RawValue.from("false",ValueType.Bool);
}
auto doOperator(RawValue a, RawValue b, ExpressionOperator operator)
{
	import std.conv : to;
	import std.algorithm : startsWith;
	if (a.type == ValueType.NotKnownAtCompileTime)
		return a;
	if (b.type == ValueType.NotKnownAtCompileTime)
		return b;
	final switch (operator)
	{
		case ExpressionOperator.InstanceOf:
		case ExpressionOperator.In:
			return RawValue.from("",ValueType.NotKnownAtCompileTime);
		case ExpressionOperator.LogicalAnd:
			if (a.coerceAsBoolean())
				return b;
			return a;
		case ExpressionOperator.LogicalOr:
			if (a.coerceAsBoolean())
				return a;
			return b;
		case ExpressionOperator.BitwiseAnd:
		case ExpressionOperator.BitwiseOr:
		case ExpressionOperator.BitwiseXor:
			a = a.toNumber;
			b = b.toNumber;
			if (a.type == ValueType.NaN)
				return a;
			if (b.type == ValueType.NaN)
				return b;
			try
			{
				if (operator == ExpressionOperator.BitwiseOr)
					return RawValue.from((a.to!int | b.to!int).to!string,ValueType.Numeric);
				else if (operator == ExpressionOperator.BitwiseAnd)
					return RawValue.from((a.to!int & b.to!int).to!string,ValueType.Numeric);
				else
					return RawValue.from((a.to!int ^ b.to!int).to!string,ValueType.Numeric);
			} catch (Exception e)
			{
				return RawValue.from("NaN",ValueType.NaN);
			}
		case ExpressionOperator.StrictEqual:
		case ExpressionOperator.Equal:
		case ExpressionOperator.StrictNotEqual:
		case ExpressionOperator.NotEqual:
			bool invert = false;
			if (operator == ExpressionOperator.StrictNotEqual || operator == ExpressionOperator.NotEqual)
				invert = true;
			if (operator == ExpressionOperator.StrictNotEqual || operator == ExpressionOperator.StrictEqual)
				return doStrictEquality(a,b,invert).toRawValue;
			return doWeakEquality(a,b,invert).toRawValue;
		case ExpressionOperator.LessOrEqual:
		case ExpressionOperator.LessThan:
		case ExpressionOperator.GreaterOrEqual:
		case ExpressionOperator.GreaterThan:
			a = a.toNumber;
			b = b.toNumber;
			if (a.type == ValueType.NaN || b.type == ValueType.NaN)
				return RawValue.from("false",ValueType.Bool);
			try
			{
				if (operator == ExpressionOperator.LessThan)
					return RawValue.from((a.to!int < b.to!int).to!string,ValueType.Bool);
				if (operator == ExpressionOperator.LessOrEqual)
					return RawValue.from((a.to!int <= b.to!int).to!string,ValueType.Bool);
				if (operator == ExpressionOperator.GreaterThan)
					return RawValue.from((a.to!int > b.to!int).to!string,ValueType.Bool);
				return RawValue.from((a.to!int >= b.to!int).to!string,ValueType.Bool);
			} catch (Exception e)
			{
				return RawValue.from("NaN",ValueType.NaN);
			}
		case ExpressionOperator.LeftShift:
		case ExpressionOperator.TripleRightSift:
		case ExpressionOperator.RightShift:
			a = a.toNumber;
			b = b.toNumber;
			if (a.type == ValueType.NaN)
				return RawValue.from("0",ValueType.Numeric);
			if (b.type == ValueType.NaN)
				return a;
			try
			{
				if (operator == ExpressionOperator.LeftShift)
				{
					return RawValue.from((a.to!int << b.to!int).to!string,ValueType.Numeric);
				} else
					return RawValue.from((a.to!int >> b.to!int).to!string,ValueType.Numeric);
			} catch (Exception e)
			{
				return RawValue.from("NaN",ValueType.NaN);
			}
		case ExpressionOperator.Add:
			if (a.type == ValueType.String)
			{
				if (b.type == ValueType.String)
				{
					return RawValue(a.value~b.value,ValueType.String);
				}
				else
					return RawValue(a.value~b.value,ValueType.String);
			}
			if (b.type == ValueType.String)
			{
				return RawValue(a.value~b.value,ValueType.String);
			}
			goto case ExpressionOperator.Minus;
		case ExpressionOperator.Minus:
			a = a.toNumber;
			b = b.toNumber;
			if (a.type == ValueType.NaN)
				return a;
			if (b.type == ValueType.NaN)
				return b;
			try
			{
				if (operator == ExpressionOperator.Add)
					return RawValue.from((a.to!int + b.to!int).to!string,ValueType.Numeric);
				else
					return RawValue.from((a.to!int - b.to!int).to!string,ValueType.Numeric);
			} catch (Exception e)
			{
				return RawValue.from("NaN",ValueType.NaN);
			}
		case ExpressionOperator.Multiply:
		case ExpressionOperator.Division:
		case ExpressionOperator.Mod:
			a = a.toNumber;
			b = b.toNumber;
			if (a.type == ValueType.NaN)
				return a;
			if (b.type == ValueType.NaN)
				return b;
			try
			{
				if (operator == ExpressionOperator.Multiply)
					return RawValue.from((a.to!int * b.to!int).to!string,ValueType.Numeric);
				else if (operator == ExpressionOperator.Mod)
					return RawValue.from((a.to!int % b.to!int).to!string,ValueType.Numeric);
				else
					return RawValue.from((a.to!double / b.to!double).to!string,ValueType.Numeric);
			} catch (Exception e)
			{
				return RawValue.from("NaN",ValueType.NaN);
			}
	}
}
@("doOperator")
unittest
{
	doOperator(
		RawValue.from("A",ValueType.NotKnownAtCompileTime),RawValue.from("B",ValueType.NotKnownAtCompileTime),
		ExpressionOperator.NotEqual
	).shouldEqual(RawValue.from("A",ValueType.NotKnownAtCompileTime));
	doOperator(
		RawValue.from("A",ValueType.Object),RawValue.from("B",ValueType.NotKnownAtCompileTime),
		ExpressionOperator.NotEqual
	).shouldEqual(RawValue.from("B",ValueType.NotKnownAtCompileTime));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.BitwiseOr
	).shouldEqual(RawValue.from("NaN",ValueType.NaN));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.LeftShift
	).shouldEqual(RawValue.from("NaN",ValueType.NaN));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.InstanceOf
	).shouldEqual(RawValue.from("",ValueType.NotKnownAtCompileTime));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.LessOrEqual
	).shouldEqual(RawValue.from("NaN",ValueType.NaN));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.Add
	).shouldEqual(RawValue.from("NaN",ValueType.NaN));

	doOperator(
		RawValue.from("A",ValueType.Numeric),RawValue.from("B",ValueType.Numeric),
		ExpressionOperator.Multiply
	).shouldEqual(RawValue.from("NaN",ValueType.NaN));

	doOperator(
		RawValue.from("-1",ValueType.Numeric),RawValue.from(`-1`,ValueType.String),
		ExpressionOperator.Equal
	).shouldEqual(RawValue.from("true",ValueType.Bool));

}
ptrdiff_t getExprOperatorPrecedence(ExpressionOperator operator)
{
	// Note: the Precedence are taken from https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Operators/Operator_Precedence
	final switch (operator)
	{
		case ExpressionOperator.LogicalOr:			return 5;
		case ExpressionOperator.LogicalAnd:			return 6;
		case ExpressionOperator.BitwiseOr:			return 7;
		case ExpressionOperator.BitwiseXor:			return 8;
		case ExpressionOperator.BitwiseAnd:			return 9;
		case ExpressionOperator.StrictEqual:		return 10;
		case ExpressionOperator.Equal:				return 10;
		case ExpressionOperator.StrictNotEqual:		return 10;
		case ExpressionOperator.NotEqual:			return 10;
		case ExpressionOperator.InstanceOf:			return 11;
		case ExpressionOperator.In:					return 11;
		case ExpressionOperator.LessOrEqual:		return 11;
		case ExpressionOperator.LessThan:			return 11;
		case ExpressionOperator.GreaterOrEqual:		return 11;
		case ExpressionOperator.GreaterThan:		return 11;
		case ExpressionOperator.LeftShift:			return 12;
		case ExpressionOperator.TripleRightSift:	return 12;
		case ExpressionOperator.RightShift:			return 12;
		case ExpressionOperator.Add:				return 13;
		case ExpressionOperator.Minus:				return 13;
		case ExpressionOperator.Multiply:			return 14;
		case ExpressionOperator.Division:			return 14;
		case ExpressionOperator.Mod:				return 14;
	}
}
ExpressionOperator getLowestOperator(BinaryExpressionNode binExpr)
{
	import std.algorithm : map, min, reduce;
	import std.range : stride;
	return binExpr.children[1..$].stride(2).map!(op=>op.as!(ExpressionOperatorNode).operator).reduce!(
		(a,b)
		{
			return a.getExprOperatorPrecedence < b.getExprOperatorPrecedence ? a : b;
		});
}
bool isNonCommutative(ExpressionOperator operator)
{
	switch(operator)
	{
		case ExpressionOperator.Minus:
		case ExpressionOperator.Division:
		case ExpressionOperator.In:
			return true;
		default:
			return false;
	}
}
@("isNonCommutative")
unittest
{
	isNonCommutative(ExpressionOperator.Minus).shouldBeTrue;
	isNonCommutative(ExpressionOperator.Division).shouldBeTrue;
	isNonCommutative(ExpressionOperator.Add).shouldBeFalse;
	isNonCommutative(ExpressionOperator.Multiply).shouldBeFalse;

}
/// < 0 means opB has precedence. > 0 means opA. 0 means indifference
ptrdiff_t compareExprOperatorPrecedence(ExpressionOperator opA, ExpressionOperator opB)
{
	return getExprOperatorPrecedence(opA) - getExprOperatorPrecedence(opB);
}
auto resolveUnaryExpression(UnaryExpressionNode expr)
{
	return getRawValue(expr);
}
auto resolveBinaryExpression(BinaryExpressionNode expr)
{
	import std.algorithm : remove, map;
	import std.range : stride;
	import std.array : array;
	auto children = expr.children;
	assert(children.length % 2 == 1); //make sure length is odd
	size_t opCount = (children.length-1)/2;
	
	RawValue[] values;
	foreach (idx; 0..(opCount+1))
	{
		auto raw = getRawValue(children[idx*2]);
		if (raw.type == ValueType.NotKnownAtCompileTime)
			return raw;
		values ~= raw;
	}
	ExpressionOperator[] operators = children[1..$].stride(2).map!(op=>op.as!(ExpressionOperatorNode).operator).array();

	while (true)
	{
		if (opCount == 1)
		{
			// if there is only one operator, there is no precedence
			return doOperator(values[0],values[1],operators[0]);
		} else if (opCount == 2)
		{
			// when there are 2, check if second operator has higher precedence, else do operator 1 first
			if (compareExprOperatorPrecedence(operators[0],operators[1]) < 0)
				return doOperator(values[0],doOperator(values[1],values[2],operators[1]),operators[0]);
			return doOperator(doOperator(values[0],values[1],operators[0]),values[2],operators[1]);
		} else
		{
			// when there are more
			if (compareExprOperatorPrecedence(operators[0],operators[1]) >= 0)
			{
				// and the first operator has higher precedence than the second one, perform and remove operator and value
				values[1] = doOperator(values[0],values[1],operators[0]);
				operators.remove(0);
				values.remove(0);
				operators.length = operators.length-1;
				values.length = values.length-1;
				--opCount;
			} else
			{
				bool didSomething = false;
				// else we search for the operator that has higher or equal precedence than the operator on its left and its right
				foreach (idx; 1..(opCount-1))
				{
					if (compareExprOperatorPrecedence(operators[idx-1],operators[idx]) > 0)
						continue;
					if (compareExprOperatorPrecedence(operators[idx],operators[idx+1]) < 0)
						continue;
					// perform and remove operator and values
					values[idx+1] = doOperator(values[idx],values[idx+1],operators[idx]);
					operators.remove(idx);
					values.remove(idx);
					operators.length = operators.length-1;
					values.length = values.length-1;
					--opCount;
					didSomething = true;
					break;
				}
				// if we didn't do something and the last operator has higher precedence than the second to last one, perform and remove operator and value
				if (!didSomething && compareExprOperatorPrecedence(operators[$-2],operators[$-1]) <= 0)
				{
					values[$-2] = doOperator(values[$-2],values[$-1],operators[$-1]);
					operators.remove(operators.length-1);
					values.remove(values.length-1);
					operators.length = operators.length-1;
					values.length = values.length-1;
					--opCount;
				}
			}
		}
	}
}
@("resolveBinaryExpression")
unittest
{
	void assertBinaryExpression(string input, bool value, in string file = __FILE__, in size_t line = __LINE__)
	{
		if (value)
			assertEvalBinaryExpression(input,ValueType.Bool,"true",file,line);
		else
			assertEvalBinaryExpression(input,ValueType.Bool,"false",file,line);
	}
	assertBinaryExpression(`1 == 1`, true);
	assertBinaryExpression(`null == null`, true);
	assertBinaryExpression(`undefined == null`, true);
	assertBinaryExpression(`null == undefined`, true);
	assertBinaryExpression(`1 == "1"`, true);
	assertBinaryExpression(`1 == "0"`, false);
	assertBinaryExpression(`1 == "asdf"`, false);
	assertBinaryExpression(`"1" == 1`, true);
	assertBinaryExpression(`"0" == 1`, false);
	assertBinaryExpression(`"asdf" == 1`, false);
	assertBinaryExpression(`-1 == "-1"`, true);
	assertBinaryExpression(`"-1" == -1`, true);
	assertBinaryExpression(`true == 1`, true);
	assertBinaryExpression(`1 == true`, true);
	assertBinaryExpression(`false == 0`, true);
	assertBinaryExpression(`0 == false`, true);
	assertBinaryExpression(`true == 2`, false);
	assertBinaryExpression(`2 == true`, false);
	assertBinaryExpression(`false == 2`, false);
	assertBinaryExpression(`2 == false`, false);
	assertBinaryExpression(`1 != 1`, false);
	assertBinaryExpression(`null != null`, false);
	assertBinaryExpression(`undefined != null`, false);
	assertBinaryExpression(`null != undefined`, false);
	assertBinaryExpression(`1 != "1"`, false);
	assertBinaryExpression(`1 != "0"`, true);
	assertBinaryExpression(`1 != "asdf"`, true);
	assertBinaryExpression(`"1" != 1`, false);
	assertBinaryExpression(`"0" != 1`, true);
	assertBinaryExpression(`"asdf" != 1`, true);
	assertBinaryExpression(`-1 != "-1"`, false);
	assertBinaryExpression(`"-1" != -1`, false);
	assertBinaryExpression(`true != 1`, false);
	assertBinaryExpression(`1 != true`, false);
	assertBinaryExpression(`false != 0`, false);
	assertBinaryExpression(`0 != false`, false);
	assertBinaryExpression(`true != 2`, true);
	assertBinaryExpression(`2 != true`, true);
	assertBinaryExpression(`false != 2`, true);
	assertBinaryExpression(`2 != false`, true);

	assertBinaryExpression(`2 === false`, false);
	assertBinaryExpression(`"2" === false`, false);
	assertBinaryExpression(`true === 2`, false);
	assertBinaryExpression(`true === "2"`, false);
	assertBinaryExpression(`null === undefined`, true);
	assertBinaryExpression(`undefined === null`, true);
	assertBinaryExpression(`2 === 5`, false);
	assertBinaryExpression(`5 === 2`, false);
	assertBinaryExpression(`2 === 2`, true);
	assertBinaryExpression(`-2 === -2`, true);
	assertBinaryExpression(`"abc" === "def"`, false);
	assertBinaryExpression(`"def" === "abc"`, false);
	assertBinaryExpression(`"abc" === "abc"`, true);
	assertBinaryExpression(`true === false`, false);
	assertBinaryExpression(`false === true`, false);
	assertBinaryExpression(`true === true`, true);
	assertBinaryExpression(`false === false`, true);
	assertBinaryExpression(`2 !== false`, true);
	assertBinaryExpression(`"2" !== false`, true);
	assertBinaryExpression(`true !== 2`, true);
	assertBinaryExpression(`true !== "2"`, true);
	assertBinaryExpression(`null !== undefined`, false);
	assertBinaryExpression(`undefined !== null`, false);
	assertBinaryExpression(`2 !== 5`, true);
	assertBinaryExpression(`5 !== 2`, true);
	assertBinaryExpression(`2 !== 2`, false);
	assertBinaryExpression(`-2 !== -2`, false);
	assertBinaryExpression(`"abc" !== "def"`, true);
	assertBinaryExpression(`"def" !== "abc"`, true);
	assertBinaryExpression(`"abc" !== "abc"`, false);
	assertBinaryExpression(`true !== false`, true);
	assertBinaryExpression(`false !== true`, true);
	assertBinaryExpression(`true !== true`, false);
	assertBinaryExpression(`false !== false`, false);

	assertEvalBinaryExpression(`false || true`, ValueType.Bool, "true");
	assertEvalBinaryExpression(`true || false`, ValueType.Bool, "true");
	assertEvalBinaryExpression(`false && true`, ValueType.Bool, "false");
	assertEvalBinaryExpression(`true && false`, ValueType.Bool, "false");
	assertEvalBinaryExpression(`false || NaN`, ValueType.NaN, "NaN");
	assertEvalBinaryExpression(`NaN || false`, ValueType.Bool, "false");
	assertEvalBinaryExpression(`false || Infinity`, ValueType.Infinity, "Infinity");
	assertEvalBinaryExpression(`Infinity || false`, ValueType.Infinity, "Infinity");

	assertEvalBinaryExpression(`false || "str"`, ValueType.String, `str`);
	assertEvalBinaryExpression(`true || "str"`, ValueType.Bool, "true");
	assertEvalBinaryExpression(`false && "str"`, ValueType.Bool, "false");
	assertEvalBinaryExpression(`true && "str"`, ValueType.String, `str`);

	assertEvalBinaryExpression(`NaN | 4`, ValueType.NaN, "NaN");
	assertEvalBinaryExpression(`1 | NaN`, ValueType.NaN, "NaN");
	assertEvalBinaryExpression(`1 | 4`, ValueType.Numeric, "5");
	assertEvalBinaryExpression(`2 & 10`, ValueType.Numeric, "2");
	assertEvalBinaryExpression(`17 ^ 3`, ValueType.Numeric, "18");

	assertEvalBinaryExpression(`NaN >> 3`, ValueType.Numeric, "0");
	assertEvalBinaryExpression(`3 >> NaN`, ValueType.Numeric, "3");
	assertEvalBinaryExpression(`NaN * 3`, ValueType.NaN, "NaN");
	assertEvalBinaryExpression(`3 * NaN`, ValueType.NaN, "NaN");
}
@("Test BinaryExpression evaluations")
unittest
{
	assertEvalBinaryExpression("0 + 1 + 1 - 6 + 5 - 55 + 99 - 12",ValueType.Numeric,"33");
	assertEvalBinaryExpression("6 * 1 / 1 + 6 % 5 - 55 * 99",ValueType.Numeric,"-5438");
	assertEvalBinaryExpression("1 & true != 4 & 15",ValueType.Numeric,"1");
	assertEvalBinaryExpression("1 < 5 && 6 > 3",ValueType.Bool,"true");
	assertEvalBinaryExpression(`6 * "1" / 1 + "6" % 5 - "55" * 99`,ValueType.Numeric,"-5438");
	assertEvalBinaryExpression("1 <= 1 && 7 >= 7",ValueType.Bool,"true");
	assertEvalBinaryExpression("1 << 4",ValueType.Numeric,"16");
	assertEvalBinaryExpression("4 >> 1",ValueType.Numeric,"2");
	assertEvalBinaryExpression("0 || 1 & 1 == 6 < 5 + 10 * 9 + 99 != 1 * 4 & 15",ValueType.Numeric,"1");
	assertEvalBinaryExpression("0 || 1 & 1 == a < 5 + 10 * 9 + b() != 1 * 4 & 15",ValueType.NotKnownAtCompileTime);
	//assertEvalBinaryExpression("0xff + 255 - 0xa0",ValueType.Numeric,"350");

	assertEvalBinaryExpression(`NaN > 55`,ValueType.Bool,"false");
	assertEvalBinaryExpression(`55 < NaN`,ValueType.Bool,"false");
}
@("String coercing rules")
unittest
{
	assertEvalBinaryExpression(`"asdf" + "jkl;"`,ValueType.String,`asdfjkl;`);
	assertEvalBinaryExpression(`5 + "jkl;"`,ValueType.String,`5jkl;`);
	assertEvalBinaryExpression(`"asdf" + 5`,ValueType.String,`asdf5`);
	assertEvalBinaryExpression(`"asdf" + true`,ValueType.String,`asdftrue`);
	assertEvalBinaryExpression(`true + "jkl;"`,ValueType.String,`truejkl;`);
	assertEvalBinaryExpression(`"asdf" + false`,ValueType.String,`asdffalse`);
	assertEvalBinaryExpression(`false + "jkl;"`,ValueType.String,`falsejkl;`);
	assertEvalBinaryExpression(`"asdf" + null`,ValueType.String,`asdfnull`);
	assertEvalBinaryExpression(`null + "jkl;"`,ValueType.String,`nulljkl;`);
	assertEvalBinaryExpression(`"asdf" + undefined`,ValueType.String,`asdfundefined`);
	assertEvalBinaryExpression(`undefined + "jkl;"`,ValueType.String,`undefinedjkl;`);
	assertEvalBinaryExpression(`"asdf" + NaN`,ValueType.String,`asdfNaN`);
	assertEvalBinaryExpression(`NaN + "jkl;"`,ValueType.String,`NaNjkl;`);
	assertEvalBinaryExpression(`"asdf" + Infinity`,ValueType.String,`asdfInfinity`);
	assertEvalBinaryExpression(`Infinity + "jkl;"`,ValueType.String,`Infinityjkl;`);
	assertEvalBinaryExpression(`"asdf" + -Infinity`,ValueType.String,`asdf-Infinity`);
	assertEvalBinaryExpression(`-Infinity + "jkl;"`,ValueType.String,`-Infinityjkl;`);
	assertEvalBinaryExpression(`6 + 5 + "jkl;"`,ValueType.String,`11jkl;`);
	assertEvalBinaryExpression(`false + true + "jkl;"`,ValueType.String,`1jkl;`);
	assertEvalBinaryExpression(`false + false + "jkl;"`,ValueType.String,`0jkl;`);
	assertEvalBinaryExpression(`4 + null + "jkl;"`,ValueType.String,`4jkl;`);
	assertEvalBinaryExpression(`true + undefined + "jkl;"`,ValueType.String,`NaNjkl;`);
	assertEvalBinaryExpression(`undefined + NaN + "jkl;"`,ValueType.String,`NaNjkl;`);
	assertEvalBinaryExpression(`NaN + Infinity + "jkl;"`,ValueType.String,`NaNjkl;`);
	assertEvalBinaryExpression(`0 < 0 + 1 * 6`,ValueType.Bool,`true`);
	assertEvalBinaryExpression(`0 << 0 | 1 ^ 6`,ValueType.Numeric,`7`);
}
@("reduce expression to ternary bool")
unittest
{
	assertExpressionToTernary("true",Ternary.True);
	assertExpressionToTernary("false",Ternary.False);
	assertExpressionToTernary(`""`,Ternary.False);
	assertExpressionToTernary(`"non-empty-string"`,Ternary.True);
	assertExpressionToTernary(`null`,Ternary.False);
	assertExpressionToTernary(`0`,Ternary.False);
	assertExpressionToTernary(`!-1`,Ternary.False);
	assertExpressionToTernary(`void delete !-1`,Ternary.False);
	assertExpressionToTernary(`true == true`,Ternary.True);
	assertExpressionToTernary(`!null == !undefined`,Ternary.True);
	assertExpressionToTernary(`"abcd" == "abcd"`,Ternary.True);
	assertExpressionToTernary(`"abcd" == "abcde"`,Ternary.False);
	assertExpressionToTernary(`"abcd" == fun()`,Ternary.None);
	assertExpressionToTernary(`abcd.bla('df') == -1`,Ternary.None);
	assertExpressionToTernary(`abcd.bla('df') > -1`,Ternary.None);
	//assertExpressionToTernary(`{} == {}`,Ternary.True).shouldThrow();
	assertExpressionToTernary(`!{}`,Ternary.False);
}
auto toUnaryExpression(RawValue value)
{
	final switch (value.type)
	{
		case ValueType.Undefined:
		case ValueType.NaN:
		case ValueType.Infinity:
			return cast(Node)new IdentifierReferenceNode(value.value);
		case ValueType.Null:
			return cast(Node)new KeywordNode(Keyword.Null);
		case ValueType.Bool:
			return cast(Node)new BooleanNode(value.value == "true");
		case ValueType.String:
			return cast(Node)new StringLiteralNode(value.value);
		case ValueType.Numeric:
			return cast(Node)new DecimalLiteralNode(value.value);
		case ValueType.Object: case ValueType.NotKnownAtCompileTime: version (unittest) { throw new UnitTestException(["Not supported"]); } assert(0);
	}
}
@("toUnaryExpression")
unittest
{
	toUnaryExpression(RawValue.from("undefined",ValueType.Undefined)).as!(IdentifierReferenceNode).identifier.shouldEqual("undefined");
	toUnaryExpression(RawValue.from("NaN",ValueType.NaN)).as!(IdentifierReferenceNode).identifier.shouldEqual("NaN");
	toUnaryExpression(RawValue.from("Infinity",ValueType.Infinity)).as!(IdentifierReferenceNode).identifier.shouldEqual("Infinity");
	toUnaryExpression(RawValue.from("Null",ValueType.Null)).as!(KeywordNode).keyword.shouldEqual(Keyword.Null);
	toUnaryExpression(RawValue.from("true",ValueType.Bool)).as!(BooleanNode).value.shouldEqual(true);
	toUnaryExpression(RawValue.from("str",ValueType.String)).as!(StringLiteralNode).value.shouldEqual("str");
	toUnaryExpression(RawValue.from("42",ValueType.Numeric)).as!(DecimalLiteralNode).value.shouldEqual("42");
	toUnaryExpression(RawValue.from("",ValueType.Object)).shouldThrow();
	toUnaryExpression(RawValue.from("",ValueType.NotKnownAtCompileTime)).shouldThrow();
}
