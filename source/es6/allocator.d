module es6.allocator;

import core.memory;

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