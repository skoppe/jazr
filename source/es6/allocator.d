module es6.allocator;

import core.memory;

version (unittest)
{
    import std.stdio;
}

@safe:

version = customallocator;

version (customallocator):
//version = debug_rollback_allocator;

extern(C) __gshared string[] rt_options = [ "gcopt=disable:1" ];

struct PointerBumpAllocator
{
public:

    @disable this(this);

    ~this()
    {
        //while (first !is null)
            //deallocateNode();
    }

    /**
     * Allocates `size` bytes of memory.
     */
    void[] allocate(const size_t size)
    out (arr)
    {
        assert(arr.length == size);
    }
    body
    {
        if (first is null)
            allocateNode(size);

        // Move size up to the next multiple of 8 for memory alignment purposes
        immutable size_t s = size & ~7UL;
        immutable size_t s2 = s == size ? size : s + 8;

        size_t fu = first.used;
        size_t end = fu + s2;
        assert(end >= fu + size);
        assert(end % 8 == 0);
        if (end > first.mem.length)
        {
            allocateNode(size);
            fu = first.used;
            end = fu + s2;
        }
        assert((cast(size_t) first.mem.ptr) % 8 == 0);
        assert(((cast(size_t) first.mem.ptr) + end) % 8 == 0);
        void[] m = first.mem[fu .. fu + size];
        first.used = end;
        return m;
    }

    /**
     * Allocates a T and returns a pointer to it
     */
    auto make(T, Args...)(auto ref Args args) @trusted
    {
        import std.algorithm.comparison : max;
        import std.experimental.allocator : stateSize;
        import std.conv : emplace;

        void[] mem = allocate(max(stateSize!T, 1));
        if (mem.ptr is null)
            return null;
        static if (is(T == class))
            return emplace!T(mem, args);
        else
            return emplace(cast(T*) mem.ptr, args);
    }

private:

    // Used for debugging
    bool contains(size_t point) const
    {
        for (const(Node)* n = first; n !is null; n = n.next)
            if (n.contains(point))
                return true;
        return false;
    }

    static struct Node
    {
        Node* next;
        size_t used;
        ubyte[] mem;

        bool contains(size_t p) const pure nothrow @nogc @safe
        {
            return p >= cast(size_t) mem.ptr && p <= cast(size_t) mem.ptr + mem.length;
        }
    }

    void allocateNode(size_t size) @trusted
    {
        import std.algorithm : max;
        import std.experimental.allocator.mallocator : Mallocator;
        import std.conv : emplace;

        enum ALLOC_SIZE = 1024 * 16;

        ubyte[] m = cast(ubyte[]) Mallocator.instance.allocate(max(size + Node.sizeof, ALLOC_SIZE));
        version (debug_rollback_allocator)
            m[] = 0;
        Node* n = emplace!Node(cast(Node*) m.ptr, first, 0, m[Node.sizeof .. $]);
        GC.addRange(n.mem.ptr,n.mem.length);
        assert((cast(size_t) n.mem.ptr) % 8 == 0, "The memoriez!");
        first = n;
    }

    void deallocateNode() @trusted
    {
        assert(first !is null);
        import std.experimental.allocator.mallocator : Mallocator;

        Node* next = first.next;
        ubyte[] mem = (cast(ubyte*) first)[0 .. Node.sizeof + first.mem.length];
        version (debug_rollback_allocator)
            mem[] = 0;
        Mallocator.instance.deallocate(mem);
        first = next;
    }

    Node* first;
}

import std.traits;

struct ArrayBuilder(T, size_t Len = 8)
{
    void put(T t) @trusted
    {
        static if (is(T == class) || isPointer!T)
            if (t is null)
                return;

        if (arr !is null)
        {
            assert(arr.length > 0);
            if (_length < arr.length)
                arr[_length] = t;
            else
                arr ~= t;
            _length++;
            return;
        }

        if (_length + 1 >= stackSpace.length)
        {
            arr = new T[stackSpace.length << 1];
            arr[0 .. stackSpace.length] = stackSpace[];
            arr[_length++] = t;

            return;
        }

        stackSpace[_length++] = t;
    }

    size_t length()
    {
        return this._length;
    }
    T[] data()
    {
        if (arr is null)
        {
            arr = new T[_length];
            arr[0 .. _length] = stackSpace[0.._length];
        }
        return arr[0.._length];
    }

private:

    T[Len] stackSpace;
    T[] arr;
    size_t _length;
}

@("ArrayBuilder")
unittest
{
    auto arr = ArrayBuilder!int();
    arr.put(1);
    arr.put(2);
    arr.put(3);
    assert(arr.data == [1,2,3]);

    arr = ArrayBuilder!int();
    foreach(i; 0..24)
        arr.put(i);
    assert(arr.data == [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]);

    arr = ArrayBuilder!int();
    foreach(i; 0..48)
        arr.put(i);
    assert(arr.data == [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47]);
}
