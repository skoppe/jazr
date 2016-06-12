module es6.testhelpers;

version(unittest)
{
	import unit_threaded;
	import es6.nodes;
	Type shouldBeOfType(Type)(Node n, in string file = __FILE__, in size_t line = __LINE__)
	{
		import std.format : format;
		auto d = cast(Type)n;
		if (d is null)
			throw new UnitTestException([format("Expected Node to be of type %s, instead got %s",Type.stringof,n)],file,line);
		return d;
	}
	auto shouldBeObject(T)(T a, T b, in string file = __FILE__, in size_t line = __LINE__)
		if (is(T == class))
	{
		if (a !is b)
			throw new UnitTestException(["Expected two objects to be the same"],file,line);
	}
	auto shouldNotBeObject(T)(T a, T b, in string file = __FILE__, in size_t line = __LINE__)
		if (is(T == class))
	{
		if (a is b)
			throw new UnitTestException(["Expected two objects to not be the same"],file,line);
	}
}