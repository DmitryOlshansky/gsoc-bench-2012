// Written in the D programming language.

/++
    Functions which operate on Unicode characters.

    For functions which operate on ASCII characters and ignore Unicode
    characters, see $(LINK2 std_ascii.html, std.ascii).

    (Short introduction to come)

    Synopsis:
    ---
    unittest
    {
        import std.uni;
        //intialize codepoint sets using regex notation
        //$(D set) contains codepoints from both scripts.
        auto set = CodepointSet("[\p{Cyrilic}||\p{Armenian}]");
        auto ascii = CodepointSet("[\p{ASCII}]");
        auto currency = CodepointSet("[\p{Currency_Symbol}]");

        //easy set ops
        auto a = set & ascii;
        assert(a.empty); //as it has no intersection with ascii
        a = set | ascii;
        auto b = currency - a; //subtract all ASCII, cyrilic and armenian

        //some properties of codepoint sets
        assert(b.length == 46); //only 46 left per unicode 6.1
        assert(!b['$']);    //testing is not really fast but works

        //building lookup tables
        auto oneTrie = a.buildTrie!1; //1-level Trie lookup table
        assert(oneTrie['£']);
        //pick best trie level, and bind it as a functor
        auto cyrilicOrArmenian = set.buildLookup;
        import std.algorithm;
        auto balance = find!(cyrilicOrArmenian)("Hello ընկեր!");
        assert(balance == "ընկեր!");

        //Normalization
        string s = "Plain ascii (and not only), is always normalized!";
        assert(s is normalize(s));//same string
        string nonS = "eﬃcient?"); //ffi ligature
        auto nS = normalize(nonS);
        assert(nS == "efficient?");
        assert(nS != n);
        //to NFKD, if available
        asert(normalize!NFKD("2¹⁰") == "210");
    }
    ---

    References:
        $(WEB www.digitalmars.com/d/ascii-table.html, ASCII Table),
        $(WEB en.wikipedia.org/wiki/Unicode, Wikipedia),
        $(WEB www.unicode.org, The Unicode Consortium)

    Trademarks:
        Unicode(tm) is a trademark of Unicode, Inc.

    Macros:
        WIKI=Phobos/StdUni

    Copyright: Copyright 2000 -
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(WEB digitalmars.com, Walter Bright), Jonathan M Davis, and Kenji Hara
    Source:    $(PHOBOSSRC std/_uni.d)
  +/
module uni;

static import std.ascii;
import std.traits, std.range, std.algorithm, std.typecons,
    std.conv, std.typetuple;
import std.array; //@@BUG UFCS doesn't work with 'local' imports

enum dchar lineSep = '\u2028'; /// UTF line separator
enum dchar paraSep = '\u2029'; /// UTF paragraph separator

//debug = std_uni;

//debug(std_uni) import std.stdio;
import std.stdio;

private:

enum lastDchar = 0x10FFFF;

auto force(T, F)(F from)
	if(isIntegral!T && !is(T == F))
{
	assert(from <= T.max && from >= T.min);
	return cast(T)from;
}

auto force(T, F)(F from)
    if(is(T == F))
{
    return from;
}

//cheap algorithm grease ;)
auto adaptIntRange(T, F)(F[] src)
{
	static struct ConvertIntegers//@@@BUG when in the 9 hells will map be copyable again?!
	{
		private F[] data;

		@property T front()
		{
			return force!T(data.front);
		}

		void popFront(){ data.popFront(); }

		@property bool empty()const { return data.empty; }

		@property size_t length()const { return data.length; }

		auto opSlice(size_t s, size_t e)
		{
		    return ConvertIntegers(data[s..e]);
        }

        //doesn't work with slices @@@BUG 7097
        @property size_t opDollar(){   return data.length; }
	}
	return ConvertIntegers(src);
}

//repeat bit X times pattern in val assuming it's length is 'bits'
size_t replicateBits(size_t times, size_t bits)(size_t val)
{
    static if(times == 1)
        return val;
    else static if(times % 2)
        return (replicateBits!(times-1, bits)(val)<<bits) | val;
    else
        return replicateBits!(times/2, bits*2)(val<<bits | val);
}

unittest
{//for replicate
    size_t m = 0b111;
    foreach(i; TypeTuple!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    {
        assert(replicateBits!(i, 3)(m)+1 == (1<<(3*i)));
        //writefln("%2d:%32b", i, replicateBits!(i, 3)(m));
    }
}

//multiple arrays squashed to one memory block
struct MultiArray(Types...)
{
    this(size_t[] sizes...)
    {
        size_t full_size;
        foreach(i, v; Types)
        {
            full_size += spaceFor!(bitSizeOf!v)(sizes[i]);
            sz[i] = sizes[i];
            static if(i >= 1)
                offsets[i] = offsets[i-1] +
                    spaceFor!(bitSizeOf!(Types[i-1]))(sizes[i-1]);
        }

        storage = new size_t[full_size];
    }

    @property auto slice(size_t n)()inout
    {
        return packedArrayView!(Unpack!(Types[n]), bitSizeOf!(Types[n]))(raw_ptr!n[0..length!n()]);
    }

    @property size_t length(size_t n)()const{ return sz[n]; }

    @property void length(size_t n)(size_t new_size)
    {
        if(new_size > sz[n])
        {//extend
            size_t delta = (new_size - sz[n]);
            sz[n] += delta;
			delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
            storage.length +=  delta;//extend space at end
            //raw_slice!x must follow resize as it could be moved!
            //next stmts move all data past this array, last-one-goes-first
            static if(n != dim-1)
            {
                auto start = raw_ptr!(n+1);
				//len includes delta
                size_t len = (storage.ptr+storage.length-start);

                copy(retro(start[0..len-delta])
                    , retro(start[delta..len]));

                start[0..delta] = 0;
                //offsets are used for raw_slice, ptr etc.
                foreach(i; n+1..dim)
                    offsets[i] += delta;
            }
        }
        else if(new_size < sz[n])
        {//shrink
            size_t delta = (sz[n] - new_size);
			sz[n] -= delta;
            delta = spaceFor!(bitSizeOf!(Types[n]))(delta);            
            //move all data past this array, forward direction
            static if(n != dim-1)
            {
                auto start = raw_ptr!(n+1);
                size_t len = storage.length;
                copy(start[delta..len]
                 , start[0..len-delta]);
                
                //adjust offsets last, they affect raw_slice
                foreach(i; n+1..dim)
                    offsets[i] -= delta;
            }
			storage.length -= delta;
        }
        //else - NOP
    }

	@property size_t bytes(size_t n=size_t.max)() const
	{
		static if(n == size_t.max)
			return storage.length*size_t.sizeof;
		else static if(n != Types.length-1)
			return (raw_ptr!(n+1)-raw_ptr!n)*size_t.sizeof;
		else
			return (storage.ptr+storage.length - raw_ptr!n)*size_t.sizeof;
	}

private:
    @property auto raw_ptr(size_t n)()inout
    {
        static if(n == 0)
            return storage.ptr;
        else
        {
            return storage.ptr+offsets[n];
        }
    }
    size_t[Types.length] offsets;//offset for level x
    size_t[Types.length] sz;//size of level x
    enum dim = Types.length;
    static bool needNotifyGc()
    {
        bool yes = false;
        foreach(v; staticMap!(hasIndirections, Types))
            yes = yes || v;
        return yes;
    }
    template Unpack(T)
    {
         //TODO: hackish! do proper pattern matching with BitPacked!(sz, T)
        static if(is(typeof(T.bitSize)) && is(T.entity) )
        {
            alias T.entity Unpack;
        }
        else
            alias T Unpack;
    }
    alias staticMap!(bitSizeOf, Types) bitWidth;
    enum indirections = needNotifyGc();
    size_t[] storage;
}

unittest
{
    // sizes are:
    //lvl0: 3, lvl1 : 2, lvl2: 1
    auto m = MultiArray!(int, ubyte, int)(3,2,1);

    static void check(size_t k, T)(ref T m, int n)
    {
        foreach(i; 0..n)
            assert(m.slice!(k)[i] == i+1, text("level:",i," : ",m.slice!(k)[0..n]));
    }

    static void checkB(size_t k, T)(ref T m, int n)
    {
        foreach(i; 0..n)
            assert(m.slice!(k)[i] == n-i, text("level:",i," : ",m.slice!(k)[0..n]));
    }

    static void fill(size_t k, T)(ref T m, int n)
    {
        foreach(i; 0..n)
            m.slice!(k)[i] = force!ubyte(i+1);
    }

    static void fillB(size_t k, T)(ref T m, int n)
    {
        foreach(i; 0..n)
            m.slice!(k)[i] = force!ubyte(n-i);
    }

    m.length!1 = 100;
    fill!1(m, 100);
    check!1(m, 100);

    m.length!0 = 220;
    fill!0(m, 220);
    check!1(m, 100);
    check!0(m, 220);

    m.length!2 = 17;
    fillB!2(m, 17);
    checkB!2(m, 17);
    check!0(m, 220);
    check!1(m, 100);

    m.length!2 = 33;
    checkB!2(m, 17);
    fillB!2(m, 33);
    checkB!2(m, 33);
    check!0(m, 220);
    check!1(m, 100);

    m.length!1 = 195;
    fillB!1(m, 195);
    checkB!1(m, 195);
    checkB!2(m, 33);
    check!0(m, 220);

    auto marr = MultiArray!(BitPacked!(4, uint), BitPacked!(6, uint))(20, 10);
    marr.length!0 = 15;
    marr.length!1 = 30;
    fill!1(marr, 30);
    fill!0(marr, 15);
    check!1(marr, 30);
    check!0(marr, 15);
}

unittest
{//more bitpacking tests
    alias MultiArray!(BitPacked!(3, size_t)
                , BitPacked!(4, size_t)
                , BitPacked!(3, size_t)
                , BitPacked!(6, size_t)
                , bool) Bitty;
    alias sliceBits!(13, 16).entity fn1;
    alias sliceBits!( 9, 13).entity fn2;
    alias sliceBits!( 6,  9).entity fn3;
    alias sliceBits!( 0,  6).entity fn4;
    static void check(size_t lvl, MA)(ref MA arr){
        for(size_t i = 0; i< arr.length!lvl; i++)
            assert(arr.slice!(lvl)[i] == i, text("Mismatch on lvl ", lvl, " idx ", i, " value: ", arr.slice!(lvl)[i]));
    }

    static void fillIdx(size_t lvl, MA)(ref MA arr){
        for(size_t i = 0; i< arr.length!lvl; i++)
            arr.slice!(lvl)[i] = i;
    }
    Bitty m1;
    
    m1.length!4 = 10;
    m1.length!3 = 2^^6;
    m1.length!2 = 2^^3;
    m1.length!1 = 2^^4;
    m1.length!0 = 2^^3;

    m1.length!4 = 2^^16;

    for(size_t i = 0; i< m1.length!4; i++)
        m1.slice!(4)[i] = i % 2;

    fillIdx!1(m1);
    check!1(m1);
    fillIdx!2(m1);
    check!2(m1);
    fillIdx!3(m1);
    check!3(m1);
    fillIdx!0(m1);
    check!0(m1);
    check!3(m1);
    check!2(m1);
    check!1(m1);
    for(size_t i=0; i < 2^^16; i++)
    {
        m1.slice!(4)[i] = i % 2;
        m1.slice!(0)[fn1(i)] = fn1(i);
        m1.slice!(1)[fn2(i)] = fn2(i);
        m1.slice!(2)[fn3(i)] = fn3(i);
        m1.slice!(3)[fn4(i)] = fn4(i);
    }
    for(size_t i=0; i < 2^^16; i++)
    {
        assert(m1.slice!(4)[i] == i % 2);
        assert(m1.slice!(0)[fn1(i)] == fn1(i));
        assert(m1.slice!(1)[fn2(i)] == fn2(i));
        assert(m1.slice!(2)[fn3(i)] == fn3(i));
        assert(m1.slice!(3)[fn4(i)] == fn4(i));
    }
}

//test bit packing with MultiArrays

size_t spaceFor(size_t bits)(size_t new_len)
{
    static if(bits > 8*size_t.sizeof)
    {
        static assert(bits % (size_t.sizeof*8) == 0);
        return new_len * bits/(8*size_t.sizeof);
    }
    else
    {
        enum factor = size_t.sizeof*8/bits;
        return (new_len)/factor+1;
    }
}

//only per word packing, speed is more important
//doesn't own memory, only provides access
struct PackedArrayView(T, size_t bits)
{
    this(inout(size_t)[] arr)inout
    {
        original = arr;
    }

    static if(bits % 8)
    {
        T opIndex(size_t idx)inout
        {        
                return cast(T)
                ((original[idx/factor] >> bits*(idx%factor))
                     & mask);       
        }

        void opIndexAssign(T val, size_t idx)
        in
        {
            static if(isIntegral!T)
                assert(val <= mask, text("mask: ",mask, " bits: ", bits, "value:", val, " > ", mask));
        }
        body
        {
                size_t tgt_shift = bits*(idx%(factor));
                original[idx/factor] &= ~(mask<<tgt_shift);
                original[idx/factor] |= cast(size_t)val << tgt_shift;
        }
    }
    else
    {//by byte granular type itself
        ref inout(T) opIndex(size_t idx)inout
        {
            return (cast(inout(T)*)original.ptr)[idx];
        }
    }

    void opSliceAssign(T val, size_t start, size_t end)
    {
        //rounded to factor granuarity
        /*size_t pad_start = (start+factor/2)/factor*factor;//rounded up
        size_t pad_end = end/factor*factor; //rounded down
        size_t i;
        for(i=start; i<pad_start; i++)
            this[i] = val;
        writeln("!!~!~!!");
        //all in between is x*factor elements
        if(pad_start != pad_end)
        {
            size_t repval = replicateBits!(factor, bits)(val);
            for(size_t j=i/factor; i<pad_end; i+=factor, j++)
                original[j] = repval;//so speed it up by factor
        }
        for(; i<end; i++)
            this[i] = val;*/
        for(size_t i=start; i<end; i++)
            this[i] = val;
    }

    auto opSlice(size_t from, size_t to)
    {
        return SliceOverIndexed!PackedArrayView(from, to, &this);
    }

    auto opSlice(){ return opSlice(0, length); }

    bool opEquals(T)(const ref T arr) const
    {
        if(length != arr.length)
           return false;
        for(size_t i=0;i<length; i++)
            if(this[i] != arr[i])
                return false;
        return true;
    }

    @property size_t length()const{ return original.length*factor; }

private:

    //factor - number of elements in one machine word
    enum factor = size_t.sizeof*8/bits, mask = 2^^bits-1;
    size_t[] original;
}


private struct SliceOverIndexed(T)
{
    auto opIndex(size_t idx)const
    //in
    //{
    //    assert(idx < to - from);
    //}
    //body
    {
        return arr.opIndex(from+idx);
    }

    void opIndexAssign(Item val, size_t idx)
    in
    {
        assert(idx < to - from);
    }
    body
    {
       (*arr)[from+idx] = val;
    }

    auto opSlice(size_t a, size_t b)
    {
        return SliceOverIndexed(from+a, from+b, arr);
    }

    void opSliceAssign(T)(T val, size_t start, size_t end)
    {
        return arr.opSliceAssign(val, start+from, end+from);
    }

    auto opSlice()
    {
        return opSlice(from, to);
    }

    @property size_t length()const{ return to-from;}

    @property bool empty()const { return from == to; }

    @property auto front()const { return (*arr)[from]; }

    @property void front(Item val) { (*arr)[from] = val; }

    @property auto back()const { return (*arr)[to-1]; }

    @property void back(Item val) { (*arr)[to-1] = val; }

    @property auto save() { return this; }

    void popFront() {   from++; }

    void popBack() {   to--; }

    bool opEquals(T)(const ref T arr) const
    {
        if(arr.length != length)
            return false;
        for(size_t i=0; i <length; i++)
            if(this[i] != arr[i])
                return false;
        return true;
    }
private:
    alias typeof(T.init[0]) Item;
     size_t from, to;
     T* arr;
}

private auto packedArrayView(T, size_t bits)(inout(size_t)[] arr)inout
{
    return inout(PackedArrayView!(T, bits))(arr);
}

/*
unittest
{
    size_t[] sample = new size_t[328];
    auto parr = packedArrayView!(uint, 7)(sample);
    foreach(i; 0..parr.length)
        parr[i] = i % 128;
    writefln("%(%x%)", sample);

    foreach(i; 0..parr.length)
        assert(parr[i] == i % 128, text(i, " vs ", parr[i]));

    auto parr2 = packedArrayView!(uint, 14)(sample);
    //re-viewing it as doubly sized is supported cleanly
    for(int i=0; i<parr2.length; i++)
        assert(parr2[i] == ((((2*i+1) % 128)<<7) | (2*i % 128)), text(i, " vs ", parr2[i]));
    equal(parr2[0..2],  [128, 384+2]);
}

*/
@safe:
//hope to see simillar stuff in public interface... once Allocators are out
//@@@BUG moveFront and friends? dunno, for now it's POD-only

@trusted size_t genericReplace(Policy=void, T, Range)
    (ref T dest, size_t from, size_t to, Range stuff)
{
    size_t delta = to - from;
    size_t stuff_end = from+stuff.length;
    if(stuff.length > delta)
    {//replace increases length
        delta = stuff.length - delta;//now, new is > old  by delta
        static if(is(Policy == void))
            dest.length = dest.length+delta;//@@@BUG lame @property
        else
            dest = Policy.realloc(dest, dest.length+delta);
        auto rem = copy(retro(dest[to..dest.length-delta])
             , retro(dest[to+delta..dest.length]));
        assert(rem.empty);
        copy(stuff, dest[from..stuff_end]);
    }
    else if(stuff.length == delta)
    {
        copy(stuff, dest[from..to]);
    }
    else
    {//replace decreases length by delta
        delta = delta - stuff.length;
        copy(stuff, dest[from..stuff_end]);
        auto rem =  copy(dest[to..dest.length]
             , dest[stuff_end..dest.length-delta]);
        static if(is(Policy == void))
            dest.length = dest.length - delta;//@@@BUG lame @property
        else
            dest = Policy.realloc(dest, dest.length-delta);
        assert(rem.empty);
    }
    return stuff_end;
}

//Simple storage manipulation policy
//TODO: stop working around the bugs rorts them!
@trusted public struct GcPolicy
{
    static T[] dup(T)(const T[] arr)
    {
        return arr.dup;
    }

    static T[] realloc(T)(T[] arr, size_t sz)
    {
        arr.length = sz;
        return arr;
    }

    static void replaceImpl(T, Range)(ref T[] dest, size_t from, size_t to, Range stuff)
    {
        replaceInPlace(dest, from, to, stuff);
    }

    static void append(T, V)(ref T[] arr, V value)
        if(!isInputRange!V)
    {
        arr ~= force!T(value);
    }

    static void append(T, V)(ref T[] arr, V value)
        if(isInputRange!V)
    {
        insertInPlace(arr, arr.length, value);
    }

    static void destroy(T)(ref T arr)
        if(isDynamicArray!T && is(Unqual!T == T))
    {
        arr[] = cast(typeof(T.init[0]))(0xdead_beef); 
    }

    static void destroy(T)(ref T arr)
        if(isDynamicArray!T && !is(Unqual!T == T))
    { /*NOP*/ }
}

//ditto
@trusted struct ReallocPolicy
{
    import std.exception, core.stdc.stdlib;
    static T[] dup(T)(const T[] arr)
    {
        auto result = alloc!T(arr.length);
        result[] = arr[];
        return result;
    }

    static T[] alloc(T)(size_t size)
    {
        auto ptr = cast(T*)enforce(malloc(T.sizeof*size), "out of memory on C heap");
        return ptr[0..size];
    }

    static T[] realloc(T)(T[] arr, size_t size)
    {
        if(!size)
        {
            destroy(arr);
            return null;
        }
        auto ptr = cast(T*)enforce(core.stdc.stdlib.realloc(
                             arr.ptr, T.sizeof*size), "out of memory on C heap");
        return ptr[0..size];
    }

    static void replaceImpl(T, Range)(ref T[] dest, size_t from, size_t to, Range stuff)
    {
        genericReplace!(ReallocPolicy)(dest, from, to, stuff);
    }

    static void append(T, V)(ref T[] arr, V value)
        if(!isInputRange!V)
    {
        arr = realloc(arr, arr.length+1);
        arr[$-1] = force!T(value);
    }

    static void append(T, V)(ref T[] arr, V value)
        if(isInputRange!V && hasLength!V)
    {
        arr = realloc(arr, arr.length+value.length);
        copy(value, arr[$-value.length..$]);
    }

    static void destroy(T)(ref T[] arr)
    {
        free(arr.ptr);
        arr = null;
    }
}

unittest
{
    with(ReallocPolicy)
    {
        bool test(T, U, V)(T orig, size_t from, size_t to, U toReplace, V result,
                   string file = __FILE__, size_t line = __LINE__)
        {
            {
                replaceImpl(orig, from, to, toReplace);
                scope(exit) destroy(orig);
                if(!std.algorithm.equal(orig, result))
                    return false;
            }
            return true;
        }
        static T[] arr(T)(T[] args... )
        {
            return dup(args);
        }

        assert(test(arr([1, 2, 3, 4]), 0, 0, [5, 6, 7], [5, 6, 7, 1, 2, 3, 4]));
        assert(test(arr([1, 2, 3, 4]), 0, 2, cast(int[])[], [3, 4]));
        assert(test(arr([1, 2, 3, 4]), 0, 4, [5, 6, 7], [5, 6, 7]));
        assert(test(arr([1, 2, 3, 4]), 0, 2, [5, 6, 7], [5, 6, 7, 3, 4]));
        assert(test(arr([1, 2, 3, 4]), 2, 3, [5, 6, 7], [1, 2, 5, 6, 7, 4]));
    }
}

//bootstrap full set operations from 3 primitives:
//addInterval, skipUpTo, dropUpTo & byInterval iteration
mixin template BasicSetOps()
{
@trusted:
    alias typeof(this) This;
    /**
        $(P Sets support natural syntax for set algebra, namely:)
        $(BOOKTABLE
            $(TR $(TH Operator) $(TH Math notation) $(TH Description) )
            $(TR $(TD &) $(TD a ∩ b) $(TD intersection) )
            $(TR $(TD |) $(TD a ∪ b) $(TD union) )
            $(TR $(TD -) $(TD a ∖ b) $(TD subtraction) )
            $(TR $(TD ~) $(TD a ~ b) $(TD symmetric set difference i.e. (a ∪ b) \ (a ∩ b) ))
        )
    */
    const This opBinary(string op, U)(U rhs) 
        if(is(typeof(U.init.isSet)) || is(U:dchar))
    {
        static if(op == "&" || op == "|" || op == "~")
        {//symmetric ops thus can swap arguments to reuse r-value
            static if(is(U:dchar))
            {
                auto copy = this.dup;
                mixin("copy "~op~"= rhs; ");
                return copy;
            }
            else
            {
                static if(is(Unqual!U == U))
                {
                    //try hard to reuse r-value         
                    mixin("rhs "~op~"= this;");
                    return rhs;
                }
                else
                {
                    auto tmp = this.dup;
                    mixin("tmp "~op~"= rhs;");
                    return tmp;
                }
            }
        }
        else static if(op == "-")
        {
            auto copy = this.dup;
            copy -= rhs;
            return copy;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    bool opBinaryRight(string op, U)(U ch)
        if(op == "in" && is(U : dchar))
    {
        return this[ch];
    }

    ///The 'op=' versions of the above overloaded operators.
    ref This opOpAssign(string op, U)(in U rhs)
        if(is(typeof(U.init.isSet)) || is(U:dchar))
    {
        static if(op == "|")    //union
        {
            static if(is(U:dchar))
			{
                this.addInterval(rhs, rhs+1);
				return this;
			}
            else
                return this.add(rhs);
        }
        else static if(op == "&")   //intersection
                return this.intersect(rhs);//overloaded
        else static if(op == "-")   //set difference
                return this.sub(rhs);//overloaded
        else static if(op == "~")   //symmetric set difference
        {
            auto copy = this & rhs;
            this |= rhs;
            this -= copy;
            return this;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    ///Range that spans each codepoint in this set.
    @property auto byChar() const
    {
        static struct CharRange
        {
            this(in This set)
            {
                this.r = set.byInterval;
                cur = r.front.a;
            }

            @property dchar front() const
            {
                return cast(dchar)cur;
            }

            @property bool empty() const
            {
                return r.empty;
            }

            void popFront()
            {
                cur++;
                while(cur >= r.front.b)
                {
                    r.popFront();
                    if(r.empty)
                        break;
                    cur = r.front.a;
                }
            }
        private:
            uint cur;
            typeof(This.init.byInterval) r;
        }

        return CharRange(this);
    }

    /**
        $(P Obtain textual representation of this set in from of [a..b) intervals
        and feed it to $(D sink). )
        $(P Used by various standard formatting facilities such as
         $(XREF std._format, formattedWrite), $(D write), $(D writef) and others.
        )
    */
    void toString(scope void delegate (const(char)[]) sink)
    {
        import std.format;
        foreach(i; byInterval)
                formattedWrite(sink, "[%d..%d) ", i.a, i.b);
    }

	ref add(uint a, uint b)
    {
        addInterval(a, b);
        return this;
    }
	enum isSet = true;
private:

    ref intersect(in This rhs)
    {
        Marker mark;
        foreach( i; rhs.byInterval())
        {
            mark = this.dropUpTo(i.a, mark);
            mark = this.skipUpTo(i.b, mark);
        }
        this.dropUpTo(uint.max, mark);
        return this;
    }

    ref intersect(dchar ch)
    {
        foreach(i; byInterval)
            if(i.a >= ch && ch < i.b)
                return this = This.init.add(ch, ch+1);
        this = This.init;
        return this;
    }

    ref sub(dchar ch)
    {
        //workaround a BUG, somehow overload below doesn't survive if base class has sub(dchar)
        return subChar(ch);
    }

    //same as the above except that skip & drop parts are swapped
    ref sub(in This rhs)
    {
        uint top;
        Marker mark;
        foreach(i; rhs.byInterval)
        {
            mark = this.skipUpTo(i.a, mark);
            mark = this.dropUpTo(i.b, mark);
        }
        return this;
    }

    ref add(in This rhs)
    {
        Marker start;
        foreach(i; rhs.byInterval())
        {
            start = addInterval(i.a, i.b, start);
        }
        return this;
    }

};

///RleBitSet is ...
@trusted public struct RleBitSet(T, SP=GcPolicy)
    if(isUnsigned!T)
{

public:
	this(Set)(in Set set)
		if(is(typeof(Set.init.isSet)))
	{
		size_t top=0;
		foreach(iv; set.byInterval)
		{
				appendPad(data, iv.a - top);
				appendPad(data, iv.b - iv.a);
				top = iv.b;
		}
	}

    this()(in uint[] intervals...) //@@@BUG text is not safe yet?!
    in
    {
        assert(intervals.length % 2 == 0, "Odd number of interval bounds [a, b)!");
        for(uint i=1; i<intervals.length; i++)
            assert(intervals[i-1] < intervals[i]
                   , text(intervals[i-1], ">", intervals[i], " in set c-tor"));
    }
    body
    {
        size_t top=0;
        for(size_t i = 0;  i < intervals.length; i+=2){
            appendPad(data, intervals[i] - top);
            appendPad(data, intervals[i+1] - intervals[i]);
            top = intervals[i+1];
        }
    }

    this(this) 
    {//TODO: COW
        data = SP.dup(data);
    }

    static if(is(SP == ReallocPolicy))
        const ~this() 
        {
            SP.destroy(data);
        }

    ///Make a mutable copy of this set.
    @property auto dup()const
    {
        RleBitSet s;
        s.data = SP.dup(data);
        return s;
    }

    @property auto byInterval() const
    {
        import std.typecons;
        static struct IntervalRange
        {
            this(in RleBitSet set_)
            {
                data = set_.data;
                popFront();
            }

            uint step(ref uint idx, uint value)
            {
                static if(T.sizeof == 4)
                {
                    value += data[idx];
                    idx++;
                }
                else
                {
                    value += data[idx];
                    while(idx+1 < data.length && data[idx+1] == 0)
                    {
                        assert(idx+2 < data.length);
                        value += data[idx+2];
                        idx += 2;
                    }
                    idx++;
                }
                return value;
            }

            @property auto front() const
            {
                return Tuple!(uint,"a", uint,"b")(a, b);
            }

            @property bool empty() const
            {
                return data == null;
            }

            void popFront()
            {
                if(idx == data.length)
                {
                    data = null;
                    return;
                }
                a = step(idx, b);
                b = step(idx, a);
            }

            uint a, b, idx;
            const(T)[] data;
        }

        return IntervalRange(this);
    }

    bool opEquals(U,SP)(ref const RleBitSet!(U,SP) rhs) const
        if(isUnsigned!U)
    {
        static if(T.sizeof == 4 && U.sizeof == 4)//short-circuit full versions
            return repr == rhs.repr;
        else
        {
            uint top=0, rtop=0, idx=0, ridx=0;
            while(idx < data.length && ridx < rhs.data.length)
            {
                top += data[idx];
                rtop += rhs.data[ridx];
                while(rtop != top)
                {
                    //check for ... x 0 y "prolong" sequence
                    if(ridx + 1 < rhs.data.length && rhs.data[ridx+1] == 0)
                    {
                        //OK rhs has extra segment
                        assert(ridx+2 < rhs.data.length); // 0 at the end is an error
                        rtop += rhs.data[ridx+2];
                        ridx += 2;
                    }
                    else if(idx + 1 < data.length && data[idx+1] == 0)
                    {
                        assert(idx+2 < data.length); // ditto at end
                        top += data[idx+2];
                        idx += 2;
                    }
                    else
                        return false;
                }
                idx++;
                ridx++;
            }
            if(idx == data.length)
            {
                if(ridx == rhs.data.length)
                    return true;
                //check overlong sequence
                rtop += rhs.data[ridx];
                while(rtop != top)
                    if(ridx + 1 < rhs.data.length && rhs.data[ridx+1] == 0)
                    {
                        //rhs has extra segment
                        assert(ridx+2 < rhs.data.length); // 0 at the end is an error
                        rtop += rhs.data[ridx+2];
                        ridx += 2;
                    }
                    else
                        return false;
            }
            else
            {
                if(idx == data.length)
                    return true;
                //check overlong sequence
                top += data[idx];
                while(rtop != top)
                    if(idx + 1 < data.length && data[idx+1] == 0)
                    {
                        //rhs has extra segment
                        assert(idx+2 < data.length); // 0 at the end is an error
                        top += data[idx+2];
                        idx += 2;
                    }
                    else
                        return false;
            }
            return idx == data.length && ridx == rhs.data.length;
        }
    }

    bool opIndex(uint val)const
    {
        foreach(i; byInterval)
            if(val < i.b)
                return val >= i.a;
        return false;
    }

	@property size_t size()const
	{
		size_t sum = 0 ;
		for(size_t i=0; i<data.length; i+=2)
			sum += data[i+1];//sum up positive intervals
		return sum;
	}

	ref invert()
	{
		//TODO: implement inversion
		return this;
	}

	@property bool empty()const
	{
		return data.length == 0;
	}

    void store(OutputRange)(OutputRange sink) const
        if(isOutputRange!(OutputRange, T))
    {
        foreach(v; data)
            put(sink, v);
    }

    @safe @property size_t bytes() pure const nothrow 
    {
        return data.length*T.sizeof;
    }

    mixin BasicSetOps;
private:
    static if(is(SP == GcPolicy))
        static RleBitSet fromRawArray(T[] input) @trusted pure nothrow
        {//assumes it's a GC-ed slice
            RleBitSet set=void;    
            set.data = input;
            return set;        
        }
    
    struct Marker//denotes position in RleBitSet
    {
        uint idx;
        uint top_before_idx;
    };

    //Think of it as of RLE compressed bit-array
    //data holds _lengths_ of intervals
    //first value is a length of negative portion, second interval is positive,
    //3rd is negative etc. (length can be zero e.g. if interval contains 0 like [\x00-\x7f])
    T[] data;

    static void appendPad(ref T[] dest, size_t val)
    {
        while(val > T.max)
        {//should be somewhat rare(!)
            val -= T.max;
            SP.append(dest, adaptIntRange!T([T.max, 0]));
        }
        SP.append(dest, val);
    }

    static size_t replacePad(ref T[] dest, size_t from, size_t to, uint[] to_insert)
    {
        static if(T.sizeof == 4)//short-circuit to optimal version
        {
            SP.replaceImpl(dest, from, to, to_insert);
            return from+to_insert.length-1;
        }
        else
        {
            T[] scratch_space;//TODO mem leak
            size_t s=0;
            foreach(i, v; to_insert)
                if(v > T.max)
                {
                    SP.append(scratch_space, adaptIntRange!T(to_insert[s..i]));
                    appendPad(scratch_space, v);
                    s = i+1;
                }

            if(s == 0)
            {
                SP.replaceImpl(dest, from, to, adaptIntRange!T(to_insert)); // short-circuit #2
                return from+to_insert.length-1;
            }
            else
            {// some of (T.max, 0) ended up there
                SP.append(scratch_space, adaptIntRange!T(to_insert[s..$]));
                SP.replaceImpl(dest, from, to, scratch_space);
                return from+scratch_space.length-1;
            }
        }
    }

    @property const(T)[] repr() const{ return data; }

    //special case for RLE set
    ref subChar(dchar ch)
    {
        Marker mark;
        mark = skipUpTo(ch, mark);
        if(mark.top_before_idx == ch && mark.idx+1 != data.length)
        {
            data[mark.idx+1] -= 1;
            data[mark.idx] += 1;
            assert(data[mark.idx] == 1);
        }
        return this;
    }

    //returns last point of insertion (idx,  top_value right before idx),
    // so that top += data[idx] on first iteration  gives top of idx
    Marker addInterval(uint a, uint b, Marker mark=Marker.init)
    in
    {
        assert(a <= b);
    }
    body
    {
        uint hint = mark.idx, hint_top_before=mark.top_before_idx;
        static if(T.sizeof != 4)
            if(a == b)//empty interval, happens often with ushort/ubyte lists
                return Marker(hint, hint_top_before);
        uint top=hint_top_before, idx, a_start, a_idx;
        debug(std_uni)
        {
            scope(exit){
                writefln("after adding (%d, %d):", a, b);
                toString((x){ write(x); });
            }
        }
        uint pos, pre_top;//marker that indicates place of insertion
        assert(a >= top, text(a, "<=", top));
        for(idx=hint; idx < data.length; idx++)
        {
            top += data[idx];
            if(a <= top)
            {
                assert(top >=  data[idx]);
                a_start = top - data[idx];
                assert(a_start <= a);
                a_idx = idx;
                break;
            }
        }

        if(idx == data.length)
        {
            //  [---+++----++++----++++++]
            //  [                         a  b]
            static if(T.sizeof < 4)
            {
               appendPad(data, a - top);
               appendPad(data, b - a);
            }
            else
                SP.append(data, adaptIntRange!T([a - top, b - a]));

            return Marker(cast(uint)data.length-1, b - data[$-1]);
        }

        top -= data[idx];
        for(; idx<data.length;idx++)
        {
            top += data[idx];
            if(b <= top)
                break;
        }

        debug(std_uni)
        {
            writefln("a_start=%d; a_idx=%d; idx=%d;", a_start, a_idx, idx);
            writefln("a=%s; b=%s; top=%s; a_start=%s;", a, b, top, a_start);
        }

        uint[] to_insert;
        if(idx == data.length)
        {
            //  [-------++++++++----++++++-]
            //  [      s     a                 b]
            if(a_idx & 1)//a in positive
            {
                to_insert = [ b - a_start ];
            }
            else// a in negative
            {
                to_insert = [ a - a_start, b - a];
            }
            pos = cast(uint)replacePad(data, a_idx, idx, to_insert);
            pre_top = b - data[pos];
            return Marker(cast(uint)pos, pre_top) ; //bail out early
        }

        if(a_idx & 1)
        {//a in positive
            if(idx & 1)//b in positive
            {
                //  [-------++++++++----++++++-]
                //  [       s    a        b    ]
                to_insert = [top - a_start];
            }
            else //b in negative
            {
                //  [-------++++++++----++++++-]
                //  [       s    a   b         ]
                if(top == b)
                {
                    assert(idx+1 < data.length);
                    pre_top = b + data[idx+1];
                    pos = cast(uint)replacePad(data, a_idx, idx+2, [b + data[idx+1] - a_start]);
                    pre_top -= data[pos];
                    return Marker(cast(uint)pos, pre_top);
                }
                to_insert = [b - a_start, top - b];
            }
        }
        else
        { // a in negative
            if(idx & 1) //b in positive
            {
                //  [----------+++++----++++++-]
                //  [     a     b              ]
                to_insert = [a - a_start, top - a];
            }
            else// b in negative
            {
                //  [----------+++++----++++++-]
                //  [  a       s      b        ]
                if(top == b)
                {
                    assert(idx+1 < data.length);
                    pre_top = top + data[idx+1];
                    pos = cast(uint)replacePad(data, a_idx, idx+2, [a - a_start, top + data[idx+1] - a ]);
                    pre_top -= data[pos];
                    return Marker(cast(uint)pos, pre_top);
                }
                assert(a >= a_start, text(a, "<= ", a_start));
                to_insert = [a - a_start, b - a, top - b];
            }
        }
        pos = cast(uint)replacePad(data, a_idx, idx+1, to_insert);
        pre_top = top - data[pos];
        debug(std_uni)
        {
            writefln("marker idx: %d; length=%d", pos, pre_top, data.length);
            writeln("inserting ", to_insert);
        }
        return Marker(cast(uint)pos, pre_top);
    }

    //remove intervals up to [..a) staring at Marker(idx, top_before)
    Marker dropUpTo(uint a, Marker mark=Marker.init)
    {
        uint start_idx = mark.idx, top_before = mark.top_before_idx;
        uint top=top_before, idx=start_idx;
        uint pos, pre_top;//marker that indicates place of insertion
        assert(idx % 2 == 0); //can't start in positive interval,
        //though negative interval can be of length zero
        for(; idx < data.length; idx++)
        {
            top += data[idx];
            if(a <= top)
                break;
        }
        if(idx >= data.length)
        {
            //nothing left
            SP.replaceImpl(data, start_idx, data.length, cast(T[])[]);
            return Marker(cast(uint)data.length, top);
        }

        if(idx & 1)
        {   //a in positive
            //[--+++----++++++----+++++++------...]
            //      |<---si       s  a  t
            uint start = top - data[idx];
            if(top == a)//glue two negative intervals
            {
                // for negative stuff, idx can be equal data.length-1
                if(idx + 1 == data.length)
                {
                    replacePad(data, start_idx, data.length, []);
                    return Marker(cast(uint)data.length, top);
                }
                replacePad(data, start_idx, idx+2, [top + data[idx+1] - top_before]);
                return Marker(start_idx, top_before);
            }
            replacePad(data, start_idx, idx+1, [a - top_before, top - a]);
        }
        else
        {   //a in negative
            //[--+++----++++++----+++++++-------+++...]
            //      |<---si              s  a  t
            replacePad(data, start_idx, idx+1, [top - top_before]);
        }
        return Marker(start_idx, top_before);
    }

    //skip intervals up to ..a)
    Marker skipUpTo(uint a, Marker mark=Marker.init)
    out(result)
    {
        assert(result.idx % 2 == 0);//always negative intervals
        //(may be  0-width after-split)
    }
    body
    {
        uint start_idx = mark.idx, top_before = mark.top_before_idx;
        uint top=top_before, idx=start_idx;
        assert(data.length % 2 == 0);
        for(; idx < data.length; idx++)
        {
            top += data[idx];
            if(a <= top)
                break;
        }
        if(idx >= data.length) //could have Marker point to recently removed stuff
            return Marker(cast(uint)data.length, top);

        if(idx & 1)//landed in positive, check for split
        {
            if(top == a)//no need to split, it's end
                return Marker(idx+1, top);
            uint start = top - data[idx];
            //split it up
            uint val = cast(uint)replacePad(data, idx, idx+1, [a - start, 0, top - a]);

            return Marker(val-1, top - (data[val]+data[val-1]));        //avoid odd index
        }
        return Marker(idx, top - data[idx]);
    }
};

/**
    $(D CodepointSet) is a packed data structure for sets of codepoints.
    Memory usage is 6 bytes per each contigous interval in a set.
*/
@trusted public struct InversionList(SP=GcPolicy)
{
	this(Set)(in Set set)
		if(is(typeof(Set.init.isSet)))
	{
		uint[] arr;
		foreach(v; set.byInterval)
		{
			arr ~= v.a;
			arr ~= v.b;
		}
		data = Uint24Array!(SP)(arr);
	}

    this()(uint[] intervals...)
    in
    {
        assert(intervals.length % 2 == 0, "Odd number of interval bounds [a, b)!");
        for(uint i=1; i<intervals.length; i++)
            assert(intervals[i-1] < intervals[i]);
    }
    body
    {
        data = Uint24Array!(SP)(intervals);
    }

    this(this)
    {//TODO: COW
        data = data.dup;
    }

    ///Make a mutable copy of this set.
    @property auto dup()const
    {
        InversionList s;
        s.data = data.dup;
        return s;
    }

    @property auto byInterval()const 
    {
        static struct Intervals
        {
            @property auto front()const
            {
                uint a = *cast(uint*)slice.ptr;
                uint b= *cast(uint*)(slice.ptr+1);
                //optimize a bit, since we go by even steps
                return Tuple!(uint, "a", uint, "b")(a & 0xFF_FFFF, b >> 8);
            }

            @property auto back()const
            {
                uint a = *cast(uint*)slice.ptr[len-2];
                uint b = *cast(uint*)slice.ptr[len-1];
                //optimize a bit, since we go by even steps
                return Tuple!(uint, "a", uint, "b")(a & 0xFF_FFFF, b >> 8);
            }

            void popFront()
            {
               len -= 2;
               slice = slice[3..$];//3*2 16bit == 2*24 bits
            }

            void popBack()
            {
                len -= 2;
                slice = slice[0..$-3];
            }

            @property bool empty()const { return len == 0; }

            @property auto save(){ return this; }
        private:
            const(ushort)[] slice;
            size_t len;
        }
        return Intervals(data.data, data.length);
    }

    bool opIndex(uint val)
    {
        return assumeSorted(data[]).lowerBound!(SearchPolicy.gallop)(val).length & 1;
    }

	///Number of characters in this set
	@property size_t size()
	{
		size_t sum = 0;
		foreach(iv; byInterval)
		{
			sum += iv.b - iv.a;
		}
		return sum;
	}

	///Do an in-place inversion of set.  See also '!' unary operator.
	ref invert()
	{
		if(data.length == 0)
		{
			addInterval(0, lastDchar+1);
			return this;
		}
		if(data[0] != 0)
			genericReplace(data, 0, 0, [0]);
		if(data[data.length-1] != lastDchar+1)
			genericReplace(data, data.length, data.length, [lastDchar+1]);

		return this;
	}

	@property bool empty() const
	{
		return data.length == 0;
	}

    mixin BasicSetOps;
private:
    alias typeof(this) This;
    alias size_t Marker;

    //special case for normal InversionList
    ref subChar(dchar ch)
    {
        auto mark = skipUpTo(ch);
        if(mark != data.length
            && data[mark] == ch && data[mark-1] == ch)
        {
            //it has split, meaning that ch happens to be in one of intervals
            data[mark] = data[mark]+1;
        }
        return this;
    }

    //
    Marker addInterval(int a, int b, Marker hint=Marker.init)
    in
    {
        assert(a <= b);
    }
    body
    {
        auto range = assumeSorted(data[]);
        size_t pos;
        size_t a_idx = range.lowerBound(a).length;
        if(a_idx == range.length)
        {
            //  [---+++----++++----++++++]
            //  [                         a  b]
            data.append([a, b]);
            return data.length-1;
        }
        size_t b_idx = range[a_idx..range.length].lowerBound(b).length+a_idx;
        uint[] to_insert;
        debug(std_uni)
        {
            writefln("a_idx=%d; b_idx=%d;", a_idx, b_idx);
        }
        if(b_idx == range.length)
        {
            //  [-------++++++++----++++++-]
            //  [      s     a                 b]
            if(a_idx & 1)//a in positive
            {
                to_insert = [ b ];
            }
            else// a in negative
            {
                to_insert = [a, b];
            }
            genericReplace(data, a_idx, b_idx, to_insert);
            return a_idx+to_insert.length-1;
        }

        uint top = data[b_idx];

        debug(std_uni)
        {
            writefln("a_idx=%d; b_idx=%d;", a_idx, b_idx);
            writefln("a=%s; b=%s; top=%s;", a, b, top);
        }
        if(a_idx & 1)
        {//a in positive
            if(b_idx & 1)//b in positive
            {
                //  [-------++++++++----++++++-]
                //  [       s    a        b    ]
                to_insert = [top];
            }
            else //b in negative
            {
                //  [-------++++++++----++++++-]
                //  [       s    a   b         ]
                if(top == b)
                {
                    assert(b_idx+1 < data.length);
                    pos = genericReplace(data, a_idx, b_idx+2, [data[b_idx+1]]);
                    return pos;
                }
                to_insert = [b, top ];
            }
        }
        else
        { // a in negative
            if(b_idx & 1) //b in positive
            {
                //  [----------+++++----++++++-]
                //  [     a     b              ]
                to_insert = [a, top];
            }
            else// b in negative
            {
                //  [----------+++++----++++++-]
                //  [  a       s      b        ]
                if(top == b)
                {
                    assert(b_idx+1 < data.length);
                    pos = genericReplace(data, a_idx, b_idx+2, [a, data[b_idx+1] ]);
                    return pos;
                }
                to_insert = [a, b, top];
            }
        }
        pos = genericReplace(data, a_idx, b_idx+1, to_insert);
        debug(std_uni)
        {
            writefln("marker idx: %d; length=%d", pos, data[pos], data.length);
            writeln("inserting ", to_insert);
        }
        return pos;
    }

    //
    Marker dropUpTo(uint a, Marker pos=Marker.init)
    in
    {
        assert(pos % 2 == 0); //at start of interval
    }
    body
    {
        auto range = assumeSorted!"a<=b"(data[pos..data.length]);
        if(range.empty)
            return pos;
        size_t idx = pos;
        idx += range.lowerBound(a).length;

        debug(std_uni)
        {
            writeln("dropUpTo full length=", data.length);
            writeln(pos,"~~~", idx);
        }
        if(idx == data.length)
            return genericReplace(data, pos, idx, cast(uint[])[]);
        if(idx & 1)
        {   //a in positive
            //[--+++----++++++----+++++++------...]
            //      |<---si       s  a  t
            genericReplace(data, pos, idx, [a]);
        }
        else
        {   //a in negative
            //[--+++----++++++----+++++++-------+++...]
            //      |<---si              s  a  t
            genericReplace(data, pos, idx, cast(uint[])[]);
        }
        return pos;
    }

    //
    Marker skipUpTo(uint a, Marker pos=Marker.init)
    out(result)
    {
        assert(result % 2 == 0);//always start of interval
        //(may be  0-width after-split)
    }
    body
    {
        assert(data.length % 2 == 0);
        auto range = assumeSorted!"a<=b"(data[pos..data.length]);
        size_t idx = pos+range.lowerBound(a).length;

        if(idx >= data.length) //could have Marker point to recently removed stuff
            return data.length;

        if(idx & 1)//inside of interval, check for split
        {

            uint top = data[idx];
            if(top == a)//no need to split, it's end
                return idx+1;
            uint start = data[idx-1];
            if(a == start)
                return idx-1;
            //split it up
            genericReplace(data, idx, idx+1, [a, a, top]);
            return idx+1;        //avoid odd index
        }
        return idx;
    }

    Uint24Array!SP data;
};

///Packed array of 24-bit integers.
@trusted struct Uint24Array(SP=GcPolicy)
{
    this(Range)(Range range)
        if(isInputRange!Range && hasLength!Range)
    {
        length = range.length;
        copy(range, this[]);
    }

	this(Range)(Range range)
        if(isInputRange!Range &&  !hasLength!Range)
	{
		auto a = array(range); //TODO: use better things like appending to Uint24Array
		this(a);
	}

    this(this)
    {
        data = SP.dup(data);
    }

    ~this()
    {
        SP.destroy(data);
    }

    @property size_t length()const { return roundDiv(data.length*2, 3); }

    @property void length(size_t len)
    {
        data = SP.realloc(data, roundDiv(len*3,2));
    }

    ///Read 24-bit packed integer
    uint opIndex(size_t idx)const
    {
        uint* ptr = cast(uint*)(data.ptr+3*idx/2);
        version(LittleEndian)
            return idx & 1 ? *ptr >>8 : *ptr & 0xFF_FFFF;
        else version(BigEndian)
            return idx & 1 ? *ptr & 0xFF_FFFF : *ptr >>8;
    }

    ///Write 24-bit packed integer
    void opIndexAssign(uint val, size_t idx)
    in
    {
        assert(val <= 0xFF_FFFF);
    }
    body
    {
        uint* ptr = cast(uint*)(data.ptr+3*idx/2);
        version(LittleEndian)
        {
            *ptr = idx & 1 ? (val<<8) | (*ptr&0xFF)
                : val | (*ptr & 0xFF00_0000);
        }
        else version(BigEndian)
        {
            *ptr = idx & 1 ? val | (*ptr & 0xFF00_0000)
                : (val<<8) | (*ptr&0xFF);
        }
    }

    //
    auto opSlice(size_t from, size_t to)
    {
        return SliceOverIndexed!Uint24Array(from, to, &this);
    }
    //
    auto opSlice()
    {
        return opSlice(0, length);
    }

    @property auto dup() const
    {
        Uint24Array r;
        r.data = SP.dup(data);
        return r;
    }

    void append(Range)(Range range)
        if(isInputRange!Range && hasLength!Range)
    {
        size_t nl = length + range.length;
        length = nl;
        copy(range, this[nl-range.length..nl]);
    }

    bool opEquals(const ref Uint24Array rhs)const
    {
        return data[0..roundDiv(data.length*2,3)]
            == rhs.data[0..roundDiv(rhs.data.length*2,3)];
    }
private:
    static uint roundDiv(size_t src, uint div)
    {
        return cast(uint)(src + div/2)/div;
    }
    ushort[] data;
}

@trusted unittest//Uint24 tests //@@@BUG@@ iota is system ?!
{
    InversionList!GcPolicy val;
    foreach(Policy; TypeTuple!(GcPolicy, ReallocPolicy))
    {
        alias typeof(Uint24Array!Policy.init[]) Range;
        alias Uint24Array!Policy U24A;
        static assert(isForwardRange!Range);
        static assert(isBidirectionalRange!Range);
        static assert(isOutputRange!(Range, uint));
        static assert(isRandomAccessRange!(Range));

        auto arr = U24A([42u, 36, 100]);
        assert(arr[0] == 42);
        assert(arr[1] == 36);
        arr[0] = 72;
        arr[1] = 0xFE_FEFE;
        assert(arr[0] == 72);
        assert(arr[1] == 0xFE_FEFE);
        assert(arr[2] == 100);


        auto r2 = U24A(iota(0, 100));
        assert(equal(r2[], iota(0, 100)), text(r2[]));
        copy(iota(10, 170, 2), r2[10..90]);
        assert(equal(r2[], chain(iota(0, 10), iota(10, 170, 2), iota(90, 100)))
               , text(r2[]));
    }
}

version(unittest)
{

private alias TypeTuple!(InversionList!GcPolicy, InversionList!ReallocPolicy) AbsTypes;
private alias staticMap!(RleBitSet, TypeTuple!(ubyte, ushort,uint)) RleTypes;
private alias TypeTuple!(AbsTypes, RleTypes) AllSets;

}

@trusted unittest//core set primitives test
{
    foreach(CodeList; AllSets)
    {
        CodeList a;
        //"plug a hole" test
        a.add(10, 20).add(25, 30).add(15, 27);
        assert(a == CodeList(10, 30), text(a));

        auto x = CodeList.init;
        x.add(10, 20).add(30, 40).add(50, 60);

        a = x;
        a.add(20, 49);//[10, 49) [50, 60)
        assert(a == CodeList(10, 49, 50 ,60));

        a = x;
        a.add(20, 50);
        assert(a == CodeList(10, 60), text(a.byInterval));

        //simple unions, mostly edge effects
        x = CodeList.init;
        x.add(10, 20).add(40, 60);

        a = x;
        a.add(10, 25); //[10, 25) [40, 60)
        assert(a == CodeList(10, 25, 40, 60));

        a = x;
        a.add(5, 15); //[5, 20) [40, 60)
        assert(a == CodeList(5, 20, 40, 60));

        a = x;
        a.add(0, 10); // [0, 20) [40, 60)
        assert(a == CodeList(0, 20, 40, 60));

        a = x;
        a.add(0, 5); //prepand
        assert(a == CodeList(0, 5, 10, 20, 40, 60));

        a = x;
        a.add(5, 20);
        assert(a == CodeList(5, 20, 40, 60));

        a = x;
        a.add(3, 37);
        assert(a == CodeList(3, 37, 40, 60));

        a = x;
        a.add(37, 65);
        assert(a == CodeList(10, 20, 37, 65), text(a.byInterval));

        //some tests on helpers for set intersection
        x = CodeList.init.add(10, 20).add(40, 60).add(100, 120);
        a = x;

        auto m = a.skipUpTo(60);
        a.dropUpTo(110, m);
        assert(a == CodeList(10, 20, 40, 60, 110, 120), text(a.data[]));

        a = x;
        a.dropUpTo(100);
        assert(a == CodeList(100, 120), text(a.data[]));

        a = x;
        m = a.skipUpTo(50);
        a.dropUpTo(140, m);
        assert(a == CodeList(10, 20, 40, 50), text(a.data[]));
        a = x;
        a.dropUpTo(60);
        assert(a == CodeList(100, 120), text(a.data[]));
    }
}

unittest//constructors
{
    alias RleBitSet!ushort CodeList;
    auto a = CodeList(10, 25, 30, 45);
    assert(a.repr == [10, 15, 5, 15]);
}

@trusted unittest
{   //full set operations
    foreach(CodeList; AllSets)
    {
        CodeList a, b, c, d;

        //"plug a hole"
        a.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b.add(40, 60).add(80, 100).add(140, 150);
        c = a | b;
        d = b | a;
        assert(c == CodeList(20, 200), text(c));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(25, 45).add(65, 85).add(95,110).add(150, 210);
        c = a | b; //[20,45) [60, 85) [95, 140) [150, 210)
        d = b | a;
        assert(c == CodeList(20, 45, 60, 85, 95, 140, 150, 210), text(c));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(10, 20).add(30,100).add(145,200);
        c = a | b;//[10, 140) [145, 200)
        d = b | a;
        assert(c == CodeList(10, 140, 145, 200));
        assert(c == d, text(c," vs ", d));

        b = CodeList.init.add(0, 10).add(15, 100).add(10, 20).add(200, 220);
        c = a | b;//[0, 140) [150, 220)
        d = b | a;
        assert(c == CodeList(0, 140, 150, 220));
        assert(c == d, text(c," vs ", d));


        a = CodeList.init.add(20, 40).add(60, 80);
        b = CodeList.init.add(25, 35).add(65, 75);
        c = a & b;
        d = b & a;
        assert(c == CodeList(25, 35, 65, 75), text(c));
        assert(c == d, text(c," vs ", d));

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(25, 35).add(65, 75).add(110, 130).add(160, 180);
        c = a & b;
        d = b & a;
        assert(c == CodeList(25, 35, 65, 75, 110, 130, 160, 180), text(c));
        assert(c == d, text(c," vs ", d));

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(10, 30).add(60, 120).add(135, 160);
        c = a & b;//[20, 30)[60, 80) [100, 120) [135, 140) [150, 160)
        d = b & a;

        assert(c == CodeList(20, 30, 60, 80, 100, 120, 135, 140, 150, 160),text(c));
        assert(c == d, text(c, " vs ",d));
        assert((c & a) == c);
        assert((d & b) == d);
        assert((c & d) == d);

        b = CodeList.init.add(40, 60).add(80, 100).add(140, 200);
        c = a & b;
        d = b & a;
        assert(c == CodeList(150, 200), text(c));
        assert(c == d, text(c, " vs ",d));
        assert((c & a) == c);
        assert((d & b) == d);
        assert((c & d) == d);

        assert((a & a) == a);
        assert((b & b) == b);

        a = CodeList.init.add(20, 40).add(60, 80).add(100, 140).add(150, 200);
        b = CodeList.init.add(30, 60).add(75, 120).add(190, 300);
        c = a - b;// [30, 40) [60, 75) [120, 140) [150, 190)
        d = b - a;// [40, 60) [80, 100) [200, 300)
        assert(c == CodeList(20, 30, 60, 75, 120, 140, 150, 190), text(c));
        assert(d == CodeList(40, 60, 80, 100, 200, 300), text(d));
        assert(c - d == c, text(c-d, " vs ", c));
        assert(d - c == d, text(d-c, " vs ", d));
        assert(c - c == CodeList.init);
        assert(d - d == CodeList.init);

        a = CodeList.init.add(20, 40).add( 60, 80).add(100, 140).add(150,            200);
        b = CodeList.init.add(10,  50).add(60,                           160).add(190, 300);
        c = a - b;// [160, 190)
        d = b - a;// [10, 20) [40, 50) [80, 100) [140, 150) [200, 300)
        assert(c == CodeList(160, 190), text(c));
        assert(d == CodeList(10, 20, 40, 50, 80, 100, 140, 150, 200, 300), text(d));
        assert(c - d == c, text(c-d, " vs ", c));
        assert(d - c == d, text(d-c, " vs ", d));
        assert(c - c == CodeList.init);
        assert(d - d == CodeList.init);

        a = CodeList.init.add(20,    40).add(60, 80).add(100,      140).add(150,  200);
        b = CodeList.init.add(10, 30).add(45,         100).add(130,             190);
        c = a ~ b; // [10, 20) [30, 40) [45, 60) [80, 130) [140, 150) [190, 200)
        d = b ~ a;
        assert(c == CodeList(10, 20, 30, 40, 45, 60, 80, 130, 140, 150, 190, 200),
               text(c));
        assert(c == d, text(c, " vs ", d));
    }
}

private alias RleBitSet!ubyte uList;
private alias RleBitSet!ushort mList;
private alias RleBitSet!uint cList;

@system unittest// set operations and integer overflow ;)
{
    uList a, b, c, d;
    a = uList(20, 40, 100,      300, 400,     1200);
    b = uList(0,           260, 300,      600);
    assert(a.repr == [20, 20, 60, 200, 100, 255, 0, 255, 0, 255, 0, 35]);
    assert(b.repr == [0, 255, 0, 5, 40, 255, 0, 45]);
    c = a & b; //[20,40) [100, 260) [400, 600)
    d = b & a;
    auto e = uList(20, 40, 100, 260, 400, 600);
    assert(c == e, text(c, " vs ", e));
    assert(c == d, text(c, " vs ", d));
}

@system unittest// ditto
{
    foreach(i, List; TypeTuple!(mList, cList))
    {
        List a, b, c, d;
        a = List(    150,       450,    550,    750,    1000,  75_000);
        b = List(80,    220,       460,      700,   900,             150_000);
        c = a & b;
        d = a | b;
        assert(c == uList(150, 220, 550, 700, 1000, 75_000), text(c));
        assert(d == uList(80, 450,  460, 750, 900, 150_000), text(d));

        c = a - b;
        d = b - a;
        assert(c == mList(220, 450, 700, 750), text(c));
        assert(d == mList(80, 150,   460, 550, 900, 1000, 75_000, 150_000), text(d));
    }
}

//@@@BUG Error: safe function '__unittest13' cannot call system function 'opAssign' WTF?
@system unittest//even more set operations with BIG intervals
{
    foreach(List; TypeTuple!(mList, cList))
    {
        List a, b, c, d, e, f;
        a = List(10_000,         100_000,
                  1_000_000,                                           10_000_000);
        b = List(       50_000            ,150_000, 250_000 ,350_000,
                  900_000       ,2_300_000,  4_600_000 ,6_400_000, 8_000_000 ,12_000_000);
        c = a | b;
        d = a & b;
        assert(c == mList(10_000, 150_000, 250_000, 350_000, 900_000, 12_000_000));
        assert(d == cList(50_000, 100_000, 1_000_000, 2_300_000, 4_600_000, 6_400_000, 8_000_000, 10_000_000));

        c = a ~ b;
        d = b ~ a;
        assert(c == d);
        assert(c == uList(10_000, 50_000, 100_000, 150_000, 250_000, 350_000, 900_000, 1_000_000,
                       2_300_000, 4_600_000, 6_400_000, 8_000_000, 10_000_000, 12_000_000));

        c = a - b;
        d = b - a;

        assert(c == uList(10_000, 50_000, 2_300_000, 4_600_000, 6_400_000, 8_000_000));
        assert(d == mList(100_000, 150_000, 250_000, 350_000, 900_000, 1_000_000,
                       10_000_000, 12_000_000));
    }
}
@system:
unittest// vs single dchar
{
    mList a = mList(10, 100, 120, 200);
    assert(a - 'A' == uList(10, 65, 66, 100, 120, 200), text(a - 'A'));
    assert((a & 'B') == uList(66, 67));
}

unittest//iteration
{
    import std.typecons;
    auto arr = "ABCDEFGHIJKLMabcdefghijklm"d;
    auto a = mList('A','N','a', 'n');
    assert(equal(a.byInterval, [ tuple(cast(uint)'A', cast(uint)'N'), tuple(cast(uint)'a', cast(uint)'n')]), text(a.byInterval));

    assert(equal(a.byChar, arr), text(a.byChar));

    auto x = uList(100, 500, 600, 900, 1200, 1500);
    assert(equal(x.byInterval, [ tuple(100, 500), tuple(600, 900), tuple(1200, 1500)]), text(x.byInterval));
}


@trusted struct Trie(Value, Key, Prefix...)
    if(Prefix.length >= 1)
{

    static if(is(Value dummy : SetAsSlot!(U), U))
    {
        alias U V;
        enum type = TrieType.Set;
        static putValue(ref V cont, Key val)
        {
            cont.insert(val);
        }
    }
    else static if(is(Value dummy: MapAsSlot!(C, U, X), C, U, X))
    {//TODO: built-in AA are somehow sluggish and need GC addRoot (still not implemented)
        alias C V;
        alias U Item;
        static assert(is(X == Key));
        enum type = TrieType.Map;
        static putValue(Pair)(ref V cont, Pair val)
        {
            cont.insert(val);
        }
    }
    else
    {
        alias Value V;
        enum type = TrieType.Value;
        static putValue(ref V x, V val)
        {
            x = val;
        }
    }

    ///Construct Trie from array of keys
    ///fills all possible keys with zeros in index
    this(Keys)(Keys keys)
        if(!is(typeof(Keys.init.isSet)))
    {
        enum last = Prefix.length-1;
        enum pageBits=Prefix[$-1].bitSize, pageSize = 1<<pageBits;
        size_t maxIdx = 1;
        //maximum index is sizes of each level multiplied
        foreach(v; Prefix)
            maxIdx *= 2^^v.bitSize;

        size_t[Prefix.length] idxs;
        table = table(idxs);
        //one page per level is bootstrap minimum
        foreach(i; Sequence!(0, Prefix.length))
            table.length!i = (1<<Prefix[i].bitSize);

        {//don't pollute the ctor scope
            auto ptr = table.slice!(last);
            size_t j = 0;
            size_t prevKeyIdx = size_t.max;
            static if(isDynamicArray!Keys)
            {
                alias GetComparators!(Prefix.length, cmpK) Comps;
				static if(type == TrieType.Set || (type == TrieType.Value && is(V == bool)))
				{
	                multiSort!(Comps, SwapStrategy.unstable)
		                (keys);
				}
				else static if(is(Unqual!Keys  == V[]))
				{
					/* NOP */
					//we consider indexes to be presorted as need as index in array is treated as key
					//and value of elements is value
				}
				else
					static assert(0, "Unsupported type of array "~Keys.stringof~" for Trie of "~V.stringof);
                auto r = keys;
            }
            else static if(type == TrieType.Map)
            {
                static assert(isAssociativeArray!Keys
                              , "MapAsSlot Tries can only be constructed out of AAs");
                alias GetComparators!(Prefix.length, cmpK0) Comps;
                auto r = array(zip(keys.byValue, keys.byKey));
                multiSort!Comps(r);
            }
            else
                static assert(false, "unsupported constructor for "~Keys.stringof);

            for(size_t i=0;i<r.length; i++)
            {
                static if(type == TrieType.Map)
                    size_t keyIdx = getIndex(r[i][1]);
				else static if(type == TrieType.Value && !is(V == bool) && is(Key : size_t))
				//value and not bool  and key is implictly convertible to size_t  == simple map,
				//key is index i
				{
					size_t keyIdx = i;
				}
                else
                    size_t keyIdx = getIndex(r[i]);
                if(keyIdx != prevKeyIdx)
                {
                    static if(type == TrieType.Value && is(V == bool))
                    {
                        addValue!last(idxs, false, keyIdx - j);
                        addValue!last(idxs, true);
                    }
					else
					{
                        addValue!last(idxs, r.front.init, keyIdx - j);
                        addValue!last(idxs, r[i]);
                    }
                    prevKeyIdx = keyIdx;
                    j = keyIdx+1;
                }
                else
                {//Set or map version can have "duplicate" slot keys
                     static if(type == TrieType.Set
                               || type == TrieType.Map)
                     {
                        idxs[last]--;
                        addValue!last(idxs, r[i]);
                     }
                }

            }
            static if(type == TrieType.Set)
                addValue!last(idxs, Key.init, maxIdx-j);
            else static if(type == TrieType.Map)
                addValue!last(idxs, r.front.init, maxIdx-j);
            else
                addValue!last(idxs, false, maxIdx-j);

			table.length!last = table.length!last - pageSize;
        }
    }

    ///Construct boolean Trie from set.
    this(Set)(in Set set, Key maxKey=Key.max)
        if(is(typeof(Set.init.isSet)))
    {
        enum last = Prefix.length-1;
        enum pageBits=Prefix[$-1].bitSize, pageSize = 1<<pageBits;
        maxKey =((maxKey + pageSize/2)>>pageBits)<<pageBits;

        auto ivals = set.byInterval;
        size_t[Prefix.length] idxs;


        table = table(idxs);
        //one page per level is bootstrap minimum
        foreach(i; Sequence!(0, Prefix.length))
            table.length!i = (1<<Prefix[i].bitSize);

        {//don't pollute the ctor scope
            auto ptr = table.slice!(last);
            size_t i = 0;
            for(;;)
            {
                if(ivals.empty)
                    break;
                uint a = ivals.front.a, b = ivals.front.b;

                addValue!last(idxs, false, a - i);
                i = a;

                assert(i < maxKey, "set has keys > maxKey in Trie c-tor");
                addValue!last(idxs, true, b - i);
                i = b;

                ivals.popFront();
            }
            addValue!last(idxs, false, maxKey - i);
        }

		table.length!last = table.length!last - pageSize;
    }

    inout(V) opIndex(Key key) inout
    {
        size_t idx;
        alias Prefix p;
        idx = p[0].entity(key);
        foreach(i, v; p[0..$-1])
            idx = (table.slice!i[idx]<<p[i+1].bitSize) + p[i+1].entity(key);
        return table.slice!(p.length-1)[idx];
    }

    @property size_t bytes(size_t n=size_t.max)() const
    {
        return table.bytes!n;
    }

    @property size_t pages(size_t n)() const
    {
        return (bytes!n+2^^(Prefix[n].bitSize-1))
                /2^^Prefix[n].bitSize;
    }

    //needed for multisort to work
    static bool cmpK(size_t i)(Key a, Key b)
    {
        return Prefix[i].entity(a) < Prefix[i].entity(b);
    }

    //ditto
    static if(type == TrieType.Map)
    static bool cmpK0(size_t i)
        (const ref Tuple!(Item,Key) a, const ref Tuple!(Item, Key) b)
    {
        return Prefix[i].entity(a[1]) < Prefix[i].entity(b[1]);
    }
private:
    enum TrieType{ Value, Set, Map };
    //for multi-sort
    template GetComparators(size_t n, alias cmpFn)
    {
        static if(n > 0)
            alias TypeTuple!(GetComparators!(n-1, cmpFn), cmpFn!(n-1)) GetComparators;
        else
            alias TypeTuple!() GetComparators;
    }

    static size_t getIndex(Key key)//get "mapped" virtual integer index
    {
        alias Prefix p;
        size_t idx;
        foreach(i, v; p[0..$-1])
        {
            idx |= p[i].entity(key);
            idx <<= p[i+1].bitSize;
        }
        idx |= p[$-1].entity(key);
        return idx;
    }

    static arrayRepr(T)(T x)
    {
        if(x.length > 32)
        {
            return text(x[0..16],"~...~", x[x.length-16..x.length]);
        }
        else
            return text(x);
    }

    //true if page was allocated, false is it was mapped or not an end of page yet
    void addValue(size_t level, T)(size_t[] indices, T val, size_t numVals=1)
    body
    {
        enum pageSize = 1<<Prefix[level].bitSize;
        if(numVals == 0)
            return;
        do
        {
            //need to take pointer again, memory block  may move
            auto ptr = table.slice!(level);
            if(numVals == 1)
            {
                static if(level == Prefix.length-1 && type != TrieType.Value)
                    putValue(ptr[indices[level]], val);
                else// can incurr narrowing conversion
                    ptr[indices[level]] = force!(typeof(ptr[indices[level]]))(val);
                indices[level]++;
                numVals = 0;
            }
            else
            {
                //where is the next page boundary
                size_t nextPB = (indices[level]+pageSize)/pageSize*pageSize;
                size_t j = indices[level];
                size_t n =  nextPB-j;//can fill right in this page
                if(numVals > n)
                    numVals -= n;
                else
                {
                    n = numVals;
                    numVals = 0;
                }
				static if(level < Prefix.length-1)
					assert(indices[level] <= 2^^Prefix[level+1].bitSize);

                static if(level == Prefix.length-1 && type != TrieType.Value)
                {
                    for(int i=0;i<n; i++)
                        putValue(ptr[j++], val);
                }
                else
                {
                    ptr[j..j+n]  = val;
                    j += n;
                }
                indices[level] = j;

            }
			//last level (i.e. topmost) has 1 "page" 
			//thus it need not to add a new page on upper level
            static if(level != 0)
            {
                alias typeof(table.slice!(level-1)[0]) NextIdx;
                NextIdx next_lvl_index;
                if(indices[level] % pageSize == 0)
                {
                    auto last = indices[level]-pageSize;
                    auto slice = ptr[indices[level] - pageSize..indices[level]];
                    size_t j;
                    for(j=0; j<last; j+=pageSize)
                    {
                        if(equal(ptr[j..j+pageSize], slice[0..pageSize]))
                        {
                            //get index to it, reuse ptr space for the next block
                            next_lvl_index = cast(NextIdx)(j/pageSize);
                            version(none)
                            {
                            writefln("LEVEL(%s) page maped idx: %s: 0..%s  ---> [%s..%s]"
                                    ,level
                                    ,indices[level-1], pageSize, j, j+pageSize);
                            writeln("LEVEL(", level
                                    , ") mapped page is: ", slice, ": ", arrayRepr(ptr[j..j+pageSize]));
                            writeln("LEVEL(", level
                                    , ") src page is :", ptr, ": ", arrayRepr(slice[0..pageSize]));
                            }
                            indices[level] -= pageSize; //reuse this page, it is duplicate
                            break;
                        }
                    }

                    if(j == last)
                    {							
                        next_lvl_index = cast(NextIdx)(indices[level]/pageSize - 1);
                        //allocate next page
                        version(none)
                        {
                        writefln("LEVEL(%s) page allocated: %s"
                                 , level, arrayRepr(slice[0..pageSize]));
                        writefln("LEVEL(%s) index: %s ; page at this index %s"
                                 , level
                                 , next_lvl_index
                                 , arrayRepr(
                                     table.slice!(level)
                                      [pageSize*next_lvl_index..(next_lvl_index+1)*pageSize]
                                    ));
                        }
                        table.length!level = table.length!level + pageSize;
                    }
                    addValue!(level-1)(indices, next_lvl_index);
                }
            }
        }
        while(numVals);
    }

    //last index is not stored in table, it is used as offset to values in a block.
    static if(is(V  == bool))//always pack bool
        MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), BitPacked!(1, V)) table;
    else
        MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), V) table;
}

/**
    Wrapping T by SetAsSlot indicates that T should be considered
    as a set of values.
    When SetAsSlot!T is used as $(D Value) type, Trie will internally
    translate assignments/tests to insert & 'in' operator repspectively.
*/
public struct SetAsSlot(T){}

///Ditto for map of Key -> Value.
public struct MapAsSlot(T, Value, Key){}

/**
    Wrapper, provided to simplify definition
    of custom Trie data structures. Use it on a lambda to indicate that
    returned value always fits within $(D bits) of bits.
*/
public template assumeSize(size_t bits, alias Fn)
{
    enum bitSize = bits;
    alias Fn entity;
}

///indicates MultiArray to apply bit packing to this field
struct BitPacked(size_t sz, T) if(isIntegral!T || is(T:dchar))
{
    enum bitSize = sz;
    alias T entity;
}

template sliceBitsImpl(size_t from, size_t to)
{
    T sliceBitsImpl(T)(T x)
    out(result)
    {
        assert(result < (1<<to-from));
    }
    body
    {
        static assert(from < to);
        return (x >> from) & ((1<<(to-from))-1);
    }
}

///todo
public template sliceBits(size_t from, size_t to)
{
    alias assumeSize!(to-from, sliceBitsImpl!(from, to)) sliceBits;
}

uint low_8(uint x) { return x&0xFF; }
uint midlow_8(uint x){ return (x&0xFF00)>>8; }
alias assumeSize!(8, low_8) lo8;
alias assumeSize!(8, midlow_8) mlo8;

template Sequence(size_t start, size_t end)
{
    static if(start < end)
        alias TypeTuple!(start, Sequence!(start+1, end)) Sequence;
    else
        alias TypeTuple!() Sequence;
}

//---- TRIE TESTS ----
version(unittest)
private enum TokenKind : ubyte { //from DCT by Roman Boiko (Boost v1.0 licence)
        // token kind has not been initialized to a valid value
        Invalid = 0,

        // protection
        Package, Private, Protected, Public, // note: extern also specifies protection level

        // storage classes
        Extern, Abstract, Auto, Const, Deprecated, Enum, Final, Immutable, InOut, NoThrow, Override, Pure, Scope, Shared, Static, Synchronized, _GShared,

        // basic type names
        Bool, Char, UByte, Byte, WChar, UShort, Short, DChar, UInt, Int, ULong, Long, Float, Double, Real, CFloat, CDouble, CReal, IFloat, IDouble, IReal, Void,

        // other keywords
        Alias, Align, Asm, Assert, Body, Break, Case, Cast, Catch, Cent, Class, Continue, Debug, Default, Delegate, Delete, Do, Else, Export, False, Finally, ForEach_Reverse, ForEach, For, Function,
        GoTo, If, Import, Interface, Invariant, In, Is, Lazy, Macro, Mixin, Module, New, Null, Out, Pragma, Ref, Return, Struct, Super, Switch,
        Template, This, Throw, True, Try, TypeDef, TypeId, TypeOf, UCent, Union, UnitTest, Version, Volatile, While, With, _FILE_, _LINE_, _Thread, _Traits,

        // any identifier which is not a keyword
        Identifier,

        // literals
        StringLiteral, CharacterLiteral, IntegerLiteral, FloatLiteral,

        // punctuation

        // brackets
        LeftParen,          // (
        RightParen,         // )
        LeftBracket,        // [
        RightBracket,       // ]
        LeftCurly,          // {
        RightCurly,         // }

        // assignment operators
        Assign,             // =
        AmpersandAssign,    // &=
        TildeAssign,        // ~=
        SlashAssign,        // /=
        LeftShiftAssign,    // <<=
        MinusAssign,        // -=
        PercentAssign,      // %=
        StarAssign,         // *=
        OrAssign,           // |=
        PlusAssign,         // +=
        PowerAssign,        // ^^=
        RightShiftAssign,   // >>=
        URightShiftAssign,  // >>>=
        XorAssign,          // ^=

        // relational operators
        Eq,                 // ==
        NotEq,              // !=
        GreaterThan,        // >
        GreaterOrEq,        // >=
        LessThan,           // <
        LessEqOrGreater,    // <>=
        LessOrGreater,      // <>
        LessOrEq,           // <=
        UnordCompare,       // !<>=
        UnordGreaterOrEq,   // !<
        UnordLessOrEq,      // !>
        UnordOrEq,          // !<>
        UnordOrGreater,     // !<=
        UnordOrLess,        // !>=

        // shift operators
        LeftShift,          // <<
        RightShift,         // >>
        URightShift,        // >>>

        // other binary operators
        Power,              // ^^
        BoolAnd,            // &&
        BoolOr,             // ||
        BitOr,              // |
        BitXor,             // ^
        Percent,            // %
        Slash,              // /

        // operators which can be either unary or binary
        Star,               // * (multiply; pointer)
        Minus,              // -
        Plus,               // +
        Ampersand,          // & (address of; bitwise and)
        Tilde,              // ~ (concat; complement)

        // unary operators
        Bang,               // ! (not; actual compile time parameter)
        Decrement,          // --
        Increment,          // ++

        // other punctuation
        Dot,                // .
        Slice,              // ..
        Ellipsis,           // ...
        Lambda,             // =>
        Question,           // ?
        Comma,              // ,
        Semicolon,          // ;
        Colon,              // :
        Dollar,             // $
        Hash,               // #
        At,                 // @

        // other tokens

        SpecialToken, EndOfLine,
        // note: it is important that the following tokens are last, because column calculation depends on whether tab appears in token spelling
        WhiteSpace, ScriptLine, Comment, SpecialTokenSequence,
        // end of file is always inserted (at the end)
        // it corresponds to either of \0 or \1A, but is also inserted immediately after __EOF__ special token
        // spelling includes everything starting from frontIndex and till the physical end of file, and it may be ""
        // __EOF__ inside a comment, character or string literal is treated as string (unlike DMD, which treats it as EoF inside token strings and character literals)
        _EOF_
};

unittest
{
    static trieStats(TRIE)(TRIE t)
    {
        writeln("---TRIE FOOTPRINT STATS---");
        foreach(i; Sequence!(0, t.table.dim) )
        {
            writefln("lvl%s = %s bytes;  %s pages"
                     , i, t.bytes!i, t.pages!i);
        }
        writefln("TOTAL: %s bytes", t.bytes);
        debug(std_uni)
		{
			writeln("INDEX (excluding value level):");
			foreach(i; Sequence!(0, t.table.dim-1) )
				writeln(t.table.slice!(i)[0..t.table.length!i]);
		}
        writeln("---------------------------");
    }
    //@@@BUG link failure, lambdas not found by linker somehow (in case of trie2)
    //alias assumeSize!(8, function (uint x) { return x&0xFF; }) lo8;
    //alias assumeSize!(7, function (uint x) { return (x&0x7F00)>>8; }) next8;
    alias RleBitSet!ubyte Set;
    auto set = Set('A','Z','a','z');
    auto trie = Trie!(bool, uint, lo8)(set, 256);//simple bool array
    for(int a='a'; a<'z';a++)
        assert(trie[a]);
    for(int a='A'; a<'Z';a++)
        assert(trie[a]);
    for(int a=0; a<'A'; a++)
        assert(!trie[a]);
    for(int a ='Z'; a<'a'; a++)
        assert(!trie[a]);

    auto redundant2 = Set(1, 18, 256+2, 256+111, 512+1, 512+18,
                          768+2, 768+111);
    auto trie2 = Trie!(bool, uint, mlo8, lo8)(redundant2, 1024);
    trieStats(trie2);
    foreach(e; redundant2.byChar)
        assert(trie2[e], text(cast(uint)e, " - ", trie2[e]));
    foreach(i; 0..1024)
    {
        assert(trie2[i] == (i in redundant2));
    }
    trieStats(trie2);

    auto redundant3 = Set(
          2,    4,    6,    8,    16,
       2+16, 4+16, 16+6, 16+8, 16+16,
       2+32, 4+32, 32+6, 32+8,
      );

    enum max3 = 256;
    //sliceBits
    auto trie3 = Trie!(bool, uint
                       , sliceBits!(6,8)
                       , sliceBits!(4,6)
                       , sliceBits!(0,4)
                       )(redundant3, max3);
    trieStats(trie3);
    foreach(i; 0..max3)
        assert(trie3[i] == (i in redundant3), text(cast(uint)i));

    auto redundant4 = Set(
            10, 64, 64+10, 128, 128+10, 256, 256+10, 512,
            1000, 2000, 3000, 4000, 5000, 6000
        );
    enum max4 = 2^^16;
    auto trie4 = Trie!(bool, size_t
                       , sliceBits!(13, 16)
                       , sliceBits!(9, 13)
                       , sliceBits!(6, 9) 
                       , sliceBits!(0, 6)
                       )(redundant4, max4);
    foreach(i; 0..max4){		
        if(i in redundant4)
            assert(trie4[i], text(cast(uint)i));
	}
    trieStats(trie4);

    string[] redundantS = ["tea", "tackle", "teenage", "start", "stray"];
    auto strie = Trie!(bool, string, useItemAt!(0, char))(redundantS);
    //using first char only
    assert(strie["test"], text(strie["test"]));
    assert(!strie["aea"]);
    assert(strie["s"]);

    //A realistic example: keyword detector
    enum keywords = [
            "abstract",
            "alias",
            "align",
            "asm",
            "assert",
            "auto",
            "body",
            "bool",
            "break",
            "byte",
            "case",
            "cast",
            "catch",
            "cdouble",
            "cent",
            "cfloat",
            "char",
            "class",
            "const",
            "continue",
            "creal",
            "dchar",
            "debug",
            "default",
            "delegate",
            "delete",
            "deprecated",
            "do",
            "double",
            "else",
            "enum",
            "export",
            "extern",
            "false",
            "final",
            "finally",
            "float",
            "for",
            "foreach",
            "foreach_reverse",
            "function",
            "goto",
            "idouble",
            "if",
            "ifloat",
            "immutable",
            "import",
            "in",
            "inout",
            "int",
            "interface",
            "invariant",
            "ireal",
            "is",
            "lazy",
            "long",
            "macro",
            "mixin",
            "module",
            "new",
            "nothrow",
            "null",
            "out",
            "override",
            "package",
            "pragma",
            "private",
            "protected",
            "public",
            "pure",
            "real",
            "ref",
            "return",
            "scope",
            "shared",
            "short",
            "static",
            "struct",
            "super",
            "switch",
            "synchronized",
            "template",
            "this",
            "throw",
            "true",
            "try",
            "typedef",
            "typeid",
            "typeof",
            "ubyte",
            "ucent",
            "uint",
            "ulong",
            "union",
            "unittest",
            "ushort",
            "version",
            "void",
            "volatile",
            "wchar",
            "while",
            "with",
            "__FILE__",
            "__gshared",
            "__LINE__",
            "__thread",
            "__traits"
    ];

    //assumes T.init == empty, NG if T.init is a legal key
    struct SmallSet(size_t N, T)
    {
        T[N] items;
        void insert(T val)
        {
            int i;
            if(val == T.init)
                return;
            for(i=0;i<N; i++)
                if(items[i] == T.init)
                {
                    items[i] = val;
                    return;
                }
            throw new Exception(text("out of slots in ", this," on key=", val));
        }

        bool opBinaryRight(string op, T)(T key)
            if(op == "in")
        {
            return  items[].countUntil(key) != -1;
        }
    }

    struct SmallMap(size_t N, V, K)
    {
        void insert(Tuple!(V, K) t){ _set.insert(t); }

        V opBinaryRight(string op, T)(T key)
            if(op == "in")
        {
            auto idx = map!"a[1]"(_set.items[]).countUntil(key);
            return idx < 0 ? V.init : _set.items[idx][0];
        }
        private:
            SmallSet!(N, Tuple!(V, K)) _set;
    }

    static size_t useLength(T)(T[] arr)
    {
        return arr.length > 63 ? 0 : arr.length; //need max length, 64 - 6bits
    }

    enum k = bitSizeOf!(SmallSet!(2, string));

    auto keyTrie = Trie!(SetAsSlot!(SmallSet!(2,string))
                         , string
                         , assumeSize!(6, useLength)
                         , useItemAt!(0, char)
                         , useLastItem!(char))(keywords);
    foreach(key; keywords)
        assert( key in keyTrie[key], text(key, (cast (size_t[])keyTrie[key].items)));
    trieStats(keyTrie);
    auto keywordsMap = [
            "abstract" : TokenKind.Abstract,
            "alias" : TokenKind.Alias,
            "align" : TokenKind.Align,
            "asm" : TokenKind.Asm,
            "assert" : TokenKind.Assert,
            "auto" : TokenKind.Auto,
            "body" : TokenKind.Body,
            "bool" : TokenKind.Bool,
            "break" : TokenKind.Break,
            "byte" : TokenKind.Byte,
            "case" : TokenKind.Case,
            "cast" : TokenKind.Cast,
            "catch" : TokenKind.Catch,
            "cdouble" : TokenKind.CDouble,
            "cent" : TokenKind.Cent,
            "cfloat" : TokenKind.CFloat,
            "char" : TokenKind.Char,
            "class" : TokenKind.Class,
            "const" : TokenKind.Const,
            "continue" : TokenKind.Continue,
            "creal" : TokenKind.CReal,
            "dchar" : TokenKind.DChar,
            "debug" : TokenKind.Debug,
            "default" : TokenKind.Default,
            "delegate" : TokenKind.Delegate,
            "delete" : TokenKind.Delete,
            "deprecated" : TokenKind.Deprecated,
            "do" : TokenKind.Do,
            "double" : TokenKind.Double,
            "else" : TokenKind.Else,
            "enum" : TokenKind.Enum,
            "export" : TokenKind.Export,
            "extern" : TokenKind.Extern,
            "false" : TokenKind.False,
            "final" : TokenKind.Final,
            "finally" : TokenKind.Finally,
            "float" : TokenKind.Float,
            "for" : TokenKind.For,
            "foreach" : TokenKind.ForEach,
            "foreach_reverse" : TokenKind.ForEach_Reverse,
            "function" : TokenKind.Function,
            "goto" : TokenKind.GoTo,
            "idouble" : TokenKind.IDouble,
            "if" : TokenKind.If,
            "ifloat" : TokenKind.IFloat,
            "immutable" : TokenKind.Immutable,
            "import" : TokenKind.Import,
            "in" : TokenKind.In,
            "inout" : TokenKind.InOut,
            "int" : TokenKind.Int,
            "interface" : TokenKind.Interface,
            "invariant" : TokenKind.Invariant,
            "invariant" : TokenKind.Invariant,
            "ireal" : TokenKind.IReal,
            "is" : TokenKind.Is,
            "lazy" : TokenKind.Lazy,
            "long" : TokenKind.Long,
            "macro" : TokenKind.Macro,
            "mixin" : TokenKind.Mixin,
            "module" : TokenKind.Module,
            "new" : TokenKind.New,
            "nothrow" : TokenKind.NoThrow,
            "null" : TokenKind.Null,
            "out" : TokenKind.Out,
            "override" : TokenKind.Override,
            "package" : TokenKind.Package,
            "pragma" : TokenKind.Pragma,
            "private" : TokenKind.Private,
            "protected" : TokenKind.Protected,
            "public" : TokenKind.Public,
            "pure" : TokenKind.Pure,
            "real" : TokenKind.Real,
            "ref" : TokenKind.Ref,
            "return" : TokenKind.Return,
            "scope" : TokenKind.Scope,
            "shared" : TokenKind.Shared,
            "short" : TokenKind.Short,
            "static" : TokenKind.Static,
            "struct" : TokenKind.Struct,
            "super" : TokenKind.Super,
            "switch" : TokenKind.Switch,
            "synchronized" : TokenKind.Synchronized,
            "template" : TokenKind.Template,
            "this" : TokenKind.This,
            "throw" : TokenKind.Throw,
            "true" : TokenKind.True,
            "try" : TokenKind.Try,
            "typedef" : TokenKind.TypeDef,
            "typeid" : TokenKind.TypeId,
            "typeof" : TokenKind.TypeOf,
            "ubyte" : TokenKind.UByte,
            "ucent" : TokenKind.UCent,
            "uint" : TokenKind.UInt,
            "ulong" : TokenKind.ULong,
            "union" : TokenKind.Union,
            "unittest" : TokenKind.UnitTest,
            "ushort" : TokenKind.UShort,
            "version" : TokenKind.Version,
            "void" : TokenKind.Void,
            "volatile" : TokenKind.Volatile,
            "wchar" : TokenKind.WChar,
            "while" : TokenKind.While,
            "with" : TokenKind.With,
            "__FILE__" : TokenKind._FILE_,
            "__gshared" : TokenKind._GShared,
            "__LINE__" : TokenKind._LINE_,
            "__thread" : TokenKind._Thread,
            "__traits" : TokenKind._Traits,
    ];
    auto keyTrie2 = Trie!(MapAsSlot!(SmallMap!(2, TokenKind, string), TokenKind, string)
                         , string
                         , assumeSize!(6, useLength)
                         , useItemAt!(0, char)
                         , useLastItem!(char))(keywordsMap);
    foreach(k,v; keywordsMap)
        assert((k in keyTrie2[k]) == v);
    trieStats(keyTrie2);

	//a bit size test
	auto a = array(map!(x => to!ubyte(x))(iota(0, 256)));
	auto bt = Trie!(bool, ubyte, sliceBits!(7, 8), sliceBits!(5, 7), sliceBits!(0, 5))(a);
	trieStats(bt);
	foreach(i; 0..256)
		assert(bt[cast(ubyte)i]);
}


template useItemAt(size_t idx, T)
    if(isIntegral!T || is(T: dchar))
{
    size_t entity(in T[] arr){ return arr[idx]; }
    enum bitSize = 8*T.sizeof;
}

template useLastItem(T)
{
    size_t entity(in T[] arr){ return arr[$-1]; }
    enum bitSize = 8*T.sizeof;
}

template fullBitSize(Prefix...)
{
	static if(Prefix.length > 0)
		enum fullBitSize = Prefix[0].bitSize+fullBitSize!(Prefix[1..$]);
	else
		enum fullBitSize = 0;
}

template idxTypes(Key, size_t fullBits, Prefix...)
{
    static if(Prefix.length == 1)
    {//the last level is value level, so no index once reduced to 1-level
        alias TypeTuple!() idxTypes;
    }
    else
    {
        //Important note on bit packing
        //Each level has to hold enough of bits to address the next one    
		//The bottom level is known to hold full bit width
		//thus it's size in pages is fill_bit_width - size_of_last_prefix
		//Recourse on this notion
        alias TypeTuple!(
			idxTypes!(Key, fullBits - Prefix[$-1].bitSize, Prefix[0..$-1]),
			BitPacked!(fullBits - Prefix[$-1].bitSize, typeof(Prefix[$-2].entity(Key.init)))
		) idxTypes;
    }
}

template bitSizeOf(T)
{
    static if(is(typeof(T.bitSize)))
        enum bitSizeOf = T.bitSize;
    else
        enum bitSizeOf = T.sizeof*8;
}

@safe:
public: //Public API continues
/++
    Whether or not $(D c) is a Unicode whitespace character.
    (general Unicode category: Part of C0(tab, vertical tab, form feed,
    carriage return, and linefeed characters), Zs, Zl, Zp, and NEL(U+0085))
  +/
bool isWhite(dchar c) @safe 
{
	return unicodeWhite_Space[c];
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isLower) instead.)

    Return whether $(D c) is a Unicode lowercase character.
  +/
deprecated bool isUniLower(dchar c) @safe pure nothrow
{
    return isLower(c);
}

/++
    Return whether $(D c) is a Unicode lowercase character.
  +/
bool isLower(dchar c) @safe pure nothrow
{
    if(std.ascii.isASCII(c))
        return std.ascii.isLower(c);

    return isAlpha(c) && c == toLower(c);
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isUpper) instead.)

    Return whether $(D c) is a Unicode uppercase character.
  +/
deprecated bool isUniUpper(dchar c) @safe pure nothrow
{
    return isUpper(c);
}

/++
    Return whether $(D c) is a Unicode uppercase character.
  +/
bool isUpper(dchar c) @safe pure nothrow
{
    if(std.ascii.isASCII(c))
        return std.ascii.isUpper(c);

    return isAlpha(c) && c == toUpper(c);
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D toLower) instead.)

    If $(D c) is a Unicode uppercase character, then its lowercase equivalent
    is returned. Otherwise $(D c) is returned.
  +/
deprecated dchar toUniLower(dchar c) @safe pure nothrow
{
    return toLower(c);
}

/++
    If $(D c) is a Unicode uppercase character, then its lowercase equivalent
    is returned. Otherwise $(D c) is returned.
  +/
dchar toLower(dchar c) @safe pure nothrow
{
    if(std.ascii.isUpper(c))
        c += 32;
    else if(c >= 0x00C0)
    {
        if((c >= 0x00C0 && c <= 0x00D6) ||
           (c >= 0x00D8 && c<=0x00DE))
        {
            c += 32;
        }
        else if((c >= 0x0100 && c < 0x0138) ||
                (c > 0x0149 && c < 0x0178))
        {
            if(c == 0x0130)
                c = 0x0069;
            else if((c & 1) == 0)
                ++c;
        }
        else if(c == 0x0178)
            c = 0x00FF;
        else if((c >= 0x0139 && c < 0x0149) ||
                (c > 0x0178 && c < 0x017F))
        {
            if(c & 1)
                ++c;
        }
        else if(c >= 0x0200 && c <= 0x0217)
        {
            if((c & 1) == 0)
                ++c;
        }
        else if((c >= 0x0401 && c <= 0x040C) ||
                (c>= 0x040E && c <= 0x040F))
        {
            c += 80;
        }
        else if(c >= 0x0410 && c <= 0x042F)
            c += 32;
        else if(c >= 0x0460 && c <= 0x047F)
        {
            if((c & 1) == 0)
                ++c;
        }
        else if(c >= 0x0531 && c <= 0x0556)
            c += 48;
        else if(c >= 0x10A0 && c <= 0x10C5)
            c += 48;
        else if(c >= 0xFF21 && c <= 0xFF3A)
            c += 32;
    }

    return c;
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D toUpper) instead.)

    If $(D c) is a Unicode lowercase character, then its uppercase equivalent
    is returned. Otherwise $(D c) is returned.
  +/
deprecated dchar toUniUpper(dchar c) @safe pure nothrow
{
    return toUpper(c);
}

/++
    If $(D c) is a Unicode lowercase character, then its uppercase equivalent
    is returned. Otherwise $(D c) is returned.
  +/
dchar toUpper(dchar c) @safe pure nothrow
{
    if(std.ascii.isLower(c))
        c -= 32;
    else if(c >= 0x00E0)
    {
        if((c >= 0x00E0 && c <= 0x00F6) ||
           (c >= 0x00F8 && c <= 0x00FE))
        {
            c -= 32;
        }
        else if(c == 0x00FF)
            c = 0x0178;
        else if((c >= 0x0100 && c < 0x0138) ||
                (c > 0x0149 && c < 0x0178))
        {
            if(c == 0x0131)
                c = 0x0049;
            else if(c & 1)
                --c;
        }
        else if((c >= 0x0139 && c < 0x0149) ||
                (c > 0x0178 && c < 0x017F))
        {
            if((c & 1) == 0)
                --c;
        }
        else if(c == 0x017F)
            c = 0x0053;
        else if(c >= 0x0200 && c <= 0x0217)
        {
            if(c & 1)
                --c;
        }
        else if(c >= 0x0430 && c<= 0x044F)
            c -= 32;
        else if((c >= 0x0451 && c <= 0x045C) ||
                (c >=0x045E && c<= 0x045F))
        {
            c -= 80;
        }
        else if(c >= 0x0460 && c <= 0x047F)
        {
            if(c & 1)
                --c;
        }
        else if(c >= 0x0561 && c < 0x0587)
            c -= 48;
        else if(c >= 0xFF41 && c <= 0xFF5A)
            c -= 32;
    }

    return c;
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isAlpha) instead.)

    Returns whether $(D c) is a Unicode alpha character
    (general Unicode category: Lu, Ll, Lt, Lm, and Lo).

    Standards: Unicode 5.0.0.
  +/
deprecated bool isUniAlpha(dchar c) @safe pure nothrow
{
    return isAlpha(c);
}

/++
    Returns whether $(D c) is a Unicode alpha character
    (general Unicode category: Lu, Ll, Lt, Lm, and Lo).

    Standards: Unicode 5.0.0.
  +/
bool isAlpha(dchar c) @safe pure nothrow
{
    static immutable dchar table[][2] =
    [
    [ 'A', 'Z' ],
    [ 'a', 'z' ],
    [ 0x00AA, 0x00AA ],
    [ 0x00B5, 0x00B5 ],
    [ 0x00BA, 0x00BA ],
    [ 0x00C0, 0x00D6 ],
    [ 0x00D8, 0x00F6 ],
    [ 0x00F8, 0x02C1 ],
    [ 0x02C6, 0x02D1 ],
    [ 0x02E0, 0x02E4 ],
    [ 0x02EE, 0x02EE ],
    [ 0x037A, 0x037D ],
    [ 0x0386, 0x0386 ],
    [ 0x0388, 0x038A ],
    [ 0x038C, 0x038C ],
    [ 0x038E, 0x03A1 ],
    [ 0x03A3, 0x03CE ],
    [ 0x03D0, 0x03F5 ],
    [ 0x03F7, 0x0481 ],
    [ 0x048A, 0x0513 ],
    [ 0x0531, 0x0556 ],
    [ 0x0559, 0x0559 ],
    [ 0x0561, 0x0587 ],
    [ 0x05D0, 0x05EA ],
    [ 0x05F0, 0x05F2 ],
    [ 0x0621, 0x063A ],
    [ 0x0640, 0x064A ],
    [ 0x066E, 0x066F ],
    [ 0x0671, 0x06D3 ],
    [ 0x06D5, 0x06D5 ],
    [ 0x06E5, 0x06E6 ],
    [ 0x06EE, 0x06EF ],
    [ 0x06FA, 0x06FC ],
    [ 0x06FF, 0x06FF ],
    [ 0x0710, 0x0710 ],
    [ 0x0712, 0x072F ],
    [ 0x074D, 0x076D ],
    [ 0x0780, 0x07A5 ],
    [ 0x07B1, 0x07B1 ],
    [ 0x07CA, 0x07EA ],
    [ 0x07F4, 0x07F5 ],
    [ 0x07FA, 0x07FA ],
    [ 0x0904, 0x0939 ],
    [ 0x093D, 0x093D ],
    [ 0x0950, 0x0950 ],
    [ 0x0958, 0x0961 ],
    [ 0x097B, 0x097F ],
    [ 0x0985, 0x098C ],
    [ 0x098F, 0x0990 ],
    [ 0x0993, 0x09A8 ],
    [ 0x09AA, 0x09B0 ],
    [ 0x09B2, 0x09B2 ],
    [ 0x09B6, 0x09B9 ],
    [ 0x09BD, 0x09BD ],
    [ 0x09CE, 0x09CE ],
    [ 0x09DC, 0x09DD ],
    [ 0x09DF, 0x09E1 ],
    [ 0x09F0, 0x09F1 ],
    [ 0x0A05, 0x0A0A ],
    [ 0x0A0F, 0x0A10 ],
    [ 0x0A13, 0x0A28 ],
    [ 0x0A2A, 0x0A30 ],
    [ 0x0A32, 0x0A33 ],
    [ 0x0A35, 0x0A36 ],
    [ 0x0A38, 0x0A39 ],
    [ 0x0A59, 0x0A5C ],
    [ 0x0A5E, 0x0A5E ],
    [ 0x0A72, 0x0A74 ],
    [ 0x0A85, 0x0A8D ],
    [ 0x0A8F, 0x0A91 ],
    [ 0x0A93, 0x0AA8 ],
    [ 0x0AAA, 0x0AB0 ],
    [ 0x0AB2, 0x0AB3 ],
    [ 0x0AB5, 0x0AB9 ],
    [ 0x0ABD, 0x0ABD ],
    [ 0x0AD0, 0x0AD0 ],
    [ 0x0AE0, 0x0AE1 ],
    [ 0x0B05, 0x0B0C ],
    [ 0x0B0F, 0x0B10 ],
    [ 0x0B13, 0x0B28 ],
    [ 0x0B2A, 0x0B30 ],
    [ 0x0B32, 0x0B33 ],
    [ 0x0B35, 0x0B39 ],
    [ 0x0B3D, 0x0B3D ],
    [ 0x0B5C, 0x0B5D ],
    [ 0x0B5F, 0x0B61 ],
    [ 0x0B71, 0x0B71 ],
    [ 0x0B83, 0x0B83 ],
    [ 0x0B85, 0x0B8A ],
    [ 0x0B8E, 0x0B90 ],
    [ 0x0B92, 0x0B95 ],
    [ 0x0B99, 0x0B9A ],
    [ 0x0B9C, 0x0B9C ],
    [ 0x0B9E, 0x0B9F ],
    [ 0x0BA3, 0x0BA4 ],
    [ 0x0BA8, 0x0BAA ],
    [ 0x0BAE, 0x0BB9 ],
    [ 0x0C05, 0x0C0C ],
    [ 0x0C0E, 0x0C10 ],
    [ 0x0C12, 0x0C28 ],
    [ 0x0C2A, 0x0C33 ],
    [ 0x0C35, 0x0C39 ],
    [ 0x0C60, 0x0C61 ],
    [ 0x0C85, 0x0C8C ],
    [ 0x0C8E, 0x0C90 ],
    [ 0x0C92, 0x0CA8 ],
    [ 0x0CAA, 0x0CB3 ],
    [ 0x0CB5, 0x0CB9 ],
    [ 0x0CBD, 0x0CBD ],
    [ 0x0CDE, 0x0CDE ],
    [ 0x0CE0, 0x0CE1 ],
    [ 0x0D05, 0x0D0C ],
    [ 0x0D0E, 0x0D10 ],
    [ 0x0D12, 0x0D28 ],
    [ 0x0D2A, 0x0D39 ],
    [ 0x0D60, 0x0D61 ],
    [ 0x0D85, 0x0D96 ],
    [ 0x0D9A, 0x0DB1 ],
    [ 0x0DB3, 0x0DBB ],
    [ 0x0DBD, 0x0DBD ],
    [ 0x0DC0, 0x0DC6 ],
    [ 0x0E01, 0x0E30 ],
    [ 0x0E32, 0x0E33 ],
    [ 0x0E40, 0x0E46 ],
    [ 0x0E81, 0x0E82 ],
    [ 0x0E84, 0x0E84 ],
    [ 0x0E87, 0x0E88 ],
    [ 0x0E8A, 0x0E8A ],
    [ 0x0E8D, 0x0E8D ],
    [ 0x0E94, 0x0E97 ],
    [ 0x0E99, 0x0E9F ],
    [ 0x0EA1, 0x0EA3 ],
    [ 0x0EA5, 0x0EA5 ],
    [ 0x0EA7, 0x0EA7 ],
    [ 0x0EAA, 0x0EAB ],
    [ 0x0EAD, 0x0EB0 ],
    [ 0x0EB2, 0x0EB3 ],
    [ 0x0EBD, 0x0EBD ],
    [ 0x0EC0, 0x0EC4 ],
    [ 0x0EC6, 0x0EC6 ],
    [ 0x0EDC, 0x0EDD ],
    [ 0x0F00, 0x0F00 ],
    [ 0x0F40, 0x0F47 ],
    [ 0x0F49, 0x0F6A ],
    [ 0x0F88, 0x0F8B ],
    [ 0x1000, 0x1021 ],
    [ 0x1023, 0x1027 ],
    [ 0x1029, 0x102A ],
    [ 0x1050, 0x1055 ],
    [ 0x10A0, 0x10C5 ],
    [ 0x10D0, 0x10FA ],
    [ 0x10FC, 0x10FC ],
    [ 0x1100, 0x1159 ],
    [ 0x115F, 0x11A2 ],
    [ 0x11A8, 0x11F9 ],
    [ 0x1200, 0x1248 ],
    [ 0x124A, 0x124D ],
    [ 0x1250, 0x1256 ],
    [ 0x1258, 0x1258 ],
    [ 0x125A, 0x125D ],
    [ 0x1260, 0x1288 ],
    [ 0x128A, 0x128D ],
    [ 0x1290, 0x12B0 ],
    [ 0x12B2, 0x12B5 ],
    [ 0x12B8, 0x12BE ],
    [ 0x12C0, 0x12C0 ],
    [ 0x12C2, 0x12C5 ],
    [ 0x12C8, 0x12D6 ],
    [ 0x12D8, 0x1310 ],
    [ 0x1312, 0x1315 ],
    [ 0x1318, 0x135A ],
    [ 0x1380, 0x138F ],
    [ 0x13A0, 0x13F4 ],
    [ 0x1401, 0x166C ],
    [ 0x166F, 0x1676 ],
    [ 0x1681, 0x169A ],
    [ 0x16A0, 0x16EA ],
    [ 0x1700, 0x170C ],
    [ 0x170E, 0x1711 ],
    [ 0x1720, 0x1731 ],
    [ 0x1740, 0x1751 ],
    [ 0x1760, 0x176C ],
    [ 0x176E, 0x1770 ],
    [ 0x1780, 0x17B3 ],
    [ 0x17D7, 0x17D7 ],
    [ 0x17DC, 0x17DC ],
    [ 0x1820, 0x1877 ],
    [ 0x1880, 0x18A8 ],
    [ 0x1900, 0x191C ],
    [ 0x1950, 0x196D ],
    [ 0x1970, 0x1974 ],
    [ 0x1980, 0x19A9 ],
    [ 0x19C1, 0x19C7 ],
    [ 0x1A00, 0x1A16 ],
    [ 0x1B05, 0x1B33 ],
    [ 0x1B45, 0x1B4B ],
    [ 0x1D00, 0x1DBF ],
    [ 0x1E00, 0x1E9B ],
    [ 0x1EA0, 0x1EF9 ],
    [ 0x1F00, 0x1F15 ],
    [ 0x1F18, 0x1F1D ],
    [ 0x1F20, 0x1F45 ],
    [ 0x1F48, 0x1F4D ],
    [ 0x1F50, 0x1F57 ],
    [ 0x1F59, 0x1F59 ],
    [ 0x1F5B, 0x1F5B ],
    [ 0x1F5D, 0x1F5D ],
    [ 0x1F5F, 0x1F7D ],
    [ 0x1F80, 0x1FB4 ],
    [ 0x1FB6, 0x1FBC ],
    [ 0x1FBE, 0x1FBE ],
    [ 0x1FC2, 0x1FC4 ],
    [ 0x1FC6, 0x1FCC ],
    [ 0x1FD0, 0x1FD3 ],
    [ 0x1FD6, 0x1FDB ],
    [ 0x1FE0, 0x1FEC ],
    [ 0x1FF2, 0x1FF4 ],
    [ 0x1FF6, 0x1FFC ],
    [ 0x2071, 0x2071 ],
    [ 0x207F, 0x207F ],
    [ 0x2090, 0x2094 ],
    [ 0x2102, 0x2102 ],
    [ 0x2107, 0x2107 ],
    [ 0x210A, 0x2113 ],
    [ 0x2115, 0x2115 ],
    [ 0x2119, 0x211D ],
    [ 0x2124, 0x2124 ],
    [ 0x2126, 0x2126 ],
    [ 0x2128, 0x2128 ],
    [ 0x212A, 0x212D ],
    [ 0x212F, 0x2139 ],
    [ 0x213C, 0x213F ],
    [ 0x2145, 0x2149 ],
    [ 0x214E, 0x214E ],
    [ 0x2183, 0x2184 ],
    [ 0x2C00, 0x2C2E ],
    [ 0x2C30, 0x2C5E ],
    [ 0x2C60, 0x2C6C ],
    [ 0x2C74, 0x2C77 ],
    [ 0x2C80, 0x2CE4 ],
    [ 0x2D00, 0x2D25 ],
    [ 0x2D30, 0x2D65 ],
    [ 0x2D6F, 0x2D6F ],
    [ 0x2D80, 0x2D96 ],
    [ 0x2DA0, 0x2DA6 ],
    [ 0x2DA8, 0x2DAE ],
    [ 0x2DB0, 0x2DB6 ],
    [ 0x2DB8, 0x2DBE ],
    [ 0x2DC0, 0x2DC6 ],
    [ 0x2DC8, 0x2DCE ],
    [ 0x2DD0, 0x2DD6 ],
    [ 0x2DD8, 0x2DDE ],
    [ 0x3005, 0x3006 ],
    [ 0x3031, 0x3035 ],
    [ 0x303B, 0x303C ],
    [ 0x3041, 0x3096 ],
    [ 0x309D, 0x309F ],
    [ 0x30A1, 0x30FA ],
    [ 0x30FC, 0x30FF ],
    [ 0x3105, 0x312C ],
    [ 0x3131, 0x318E ],
    [ 0x31A0, 0x31B7 ],
    [ 0x31F0, 0x31FF ],
    [ 0x3400, 0x4DB5 ],
    [ 0x4E00, 0x9FBB ],
    [ 0xA000, 0xA48C ],
    [ 0xA717, 0xA71A ],
    [ 0xA800, 0xA801 ],
    [ 0xA803, 0xA805 ],
    [ 0xA807, 0xA80A ],
    [ 0xA80C, 0xA822 ],
    [ 0xA840, 0xA873 ],
    [ 0xAC00, 0xD7A3 ],
    [ 0xF900, 0xFA2D ],
    [ 0xFA30, 0xFA6A ],
    [ 0xFA70, 0xFAD9 ],
    [ 0xFB00, 0xFB06 ],
    [ 0xFB13, 0xFB17 ],
    [ 0xFB1D, 0xFB1D ],
    [ 0xFB1F, 0xFB28 ],
    [ 0xFB2A, 0xFB36 ],
    [ 0xFB38, 0xFB3C ],
    [ 0xFB3E, 0xFB3E ],
    [ 0xFB40, 0xFB41 ],
    [ 0xFB43, 0xFB44 ],
    [ 0xFB46, 0xFBB1 ],
    [ 0xFBD3, 0xFD3D ],
    [ 0xFD50, 0xFD8F ],
    [ 0xFD92, 0xFDC7 ],
    [ 0xFDF0, 0xFDFB ],
    [ 0xFE70, 0xFE74 ],
    [ 0xFE76, 0xFEFC ],
    [ 0xFF21, 0xFF3A ],
    [ 0xFF41, 0xFF5A ],
    [ 0xFF66, 0xFFBE ],
    [ 0xFFC2, 0xFFC7 ],
    [ 0xFFCA, 0xFFCF ],
    [ 0xFFD2, 0xFFD7 ],
    [ 0xFFDA, 0xFFDC ],
    [ 0x10000, 0x1000B ],
    [ 0x1000D, 0x10026 ],
    [ 0x10028, 0x1003A ],
    [ 0x1003C, 0x1003D ],
    [ 0x1003F, 0x1004D ],
    [ 0x10050, 0x1005D ],
    [ 0x10080, 0x100FA ],
    [ 0x10300, 0x1031E ],
    [ 0x10330, 0x10340 ],
    [ 0x10342, 0x10349 ],
    [ 0x10380, 0x1039D ],
    [ 0x103A0, 0x103C3 ],
    [ 0x103C8, 0x103CF ],
    [ 0x10400, 0x1049D ],
    [ 0x10800, 0x10805 ],
    [ 0x10808, 0x10808 ],
    [ 0x1080A, 0x10835 ],
    [ 0x10837, 0x10838 ],
    [ 0x1083C, 0x1083C ],
    [ 0x1083F, 0x1083F ],
    [ 0x10900, 0x10915 ],
    [ 0x10A00, 0x10A00 ],
    [ 0x10A10, 0x10A13 ],
    [ 0x10A15, 0x10A17 ],
    [ 0x10A19, 0x10A33 ],
    [ 0x12000, 0x1236E ],
    [ 0x1D400, 0x1D454 ],
    [ 0x1D456, 0x1D49C ],
    [ 0x1D49E, 0x1D49F ],
    [ 0x1D4A2, 0x1D4A2 ],
    [ 0x1D4A5, 0x1D4A6 ],
    [ 0x1D4A9, 0x1D4AC ],
    [ 0x1D4AE, 0x1D4B9 ],
    [ 0x1D4BB, 0x1D4BB ],
    [ 0x1D4BD, 0x1D4C3 ],
    [ 0x1D4C5, 0x1D505 ],
    [ 0x1D507, 0x1D50A ],
    [ 0x1D50D, 0x1D514 ],
    [ 0x1D516, 0x1D51C ],
    [ 0x1D51E, 0x1D539 ],
    [ 0x1D53B, 0x1D53E ],
    [ 0x1D540, 0x1D544 ],
    [ 0x1D546, 0x1D546 ],
    [ 0x1D54A, 0x1D550 ],
    [ 0x1D552, 0x1D6A5 ],
    [ 0x1D6A8, 0x1D6C0 ],
    [ 0x1D6C2, 0x1D6DA ],
    [ 0x1D6DC, 0x1D6FA ],
    [ 0x1D6FC, 0x1D714 ],
    [ 0x1D716, 0x1D734 ],
    [ 0x1D736, 0x1D74E ],
    [ 0x1D750, 0x1D76E ],
    [ 0x1D770, 0x1D788 ],
    [ 0x1D78A, 0x1D7A8 ],
    [ 0x1D7AA, 0x1D7C2 ],
    [ 0x1D7C4, 0x1D7CB ],
    [ 0x20000, 0x2A6D6 ],
    [ 0x2F800, 0x2FA1D ],
    ];

    // optimization
    if(c < 0xAA)
    {
        if(c < 'A')
            return false;
        if(c <= 'Z')
            return true;
        if(c < 'a')
            return false;
        if(c <= 'z')
            return true;
        return false;
    }

    return binarySearch!table(c);
}

unittest
{
    for(dchar c = 0; c < 0x80; ++c)
    {
        if(c >= 'A' && c <= 'Z')
            assert(isAlpha(c));
        else if(c >= 'a' && c <= 'z')
            assert(isAlpha(c));
        else
            assert(!isAlpha(c));
    }
}


/++
    Returns whether $(D c) is a Unicode mark
    (general Unicode category: Mn, Me, Mc).

    Standards: Unicode 6.0.0.
  +/

bool isMark(dchar c) @safe pure nothrow
{
    static immutable dchar tableMn[][2] =
    [
    [ 0x0300, 0x036F ],
    [ 0x0483, 0x0487 ],
    [ 0x0591, 0x05BD ],
    [ 0x05BF, 0x05BF ],
    [ 0x05C1, 0x05C2 ],
    [ 0x05C4, 0x05C5 ],
    [ 0x05C7, 0x05C7 ],
    [ 0x0610, 0x061A ],
    [ 0x064B, 0x065F ],
    [ 0x0670, 0x0670 ],
    [ 0x06D6, 0x06DC ],
    [ 0x06DF, 0x06E4 ],
    [ 0x06E7, 0x06E8 ],
    [ 0x06EA, 0x06ED ],
    [ 0x0711, 0x0711 ],
    [ 0x0730, 0x074A ],
    [ 0x07A6, 0x07B0 ],
    [ 0x07EB, 0x07F3 ],
    [ 0x0816, 0x0819 ],
    [ 0x081B, 0x0823 ],
    [ 0x0825, 0x0827 ],
    [ 0x0829, 0x082D ],
    [ 0x0859, 0x085B ],
    [ 0x0900, 0x0902 ],
    [ 0x093A, 0x093A ],
    [ 0x093C, 0x093C ],
    [ 0x0941, 0x0948 ],
    [ 0x094D, 0x094D ],
    [ 0x0951, 0x0957 ],
    [ 0x0962, 0x0963 ],
    [ 0x0981, 0x0981 ],
    [ 0x09BC, 0x09BC ],
    [ 0x09C1, 0x09C4 ],
    [ 0x09CD, 0x09CD ],
    [ 0x09E2, 0x09E3 ],
    [ 0x0A01, 0x0A02 ],
    [ 0x0A3C, 0x0A3C ],
    [ 0x0A41, 0x0A42 ],
    [ 0x0A47, 0x0A48 ],
    [ 0x0A4B, 0x0A4D ],
    [ 0x0A51, 0x0A51 ],
    [ 0x0A70, 0x0A71 ],
    [ 0x0A75, 0x0A75 ],
    [ 0x0A81, 0x0A82 ],
    [ 0x0ABC, 0x0ABC ],
    [ 0x0AC1, 0x0AC5 ],
    [ 0x0AC7, 0x0AC8 ],
    [ 0x0ACD, 0x0ACD ],
    [ 0x0AE2, 0x0AE3 ],
    [ 0x0B01, 0x0B01 ],
    [ 0x0B3C, 0x0B3C ],
    [ 0x0B3F, 0x0B3F ],
    [ 0x0B41, 0x0B44 ],
    [ 0x0B4D, 0x0B4D ],
    [ 0x0B56, 0x0B56 ],
    [ 0x0B62, 0x0B63 ],
    [ 0x0B82, 0x0B82 ],
    [ 0x0BC0, 0x0BC0 ],
    [ 0x0BCD, 0x0BCD ],
    [ 0x0C3E, 0x0C40 ],
    [ 0x0C46, 0x0C48 ],
    [ 0x0C4A, 0x0C4D ],
    [ 0x0C55, 0x0C56 ],
    [ 0x0C62, 0x0C63 ],
    [ 0x0CBC, 0x0CBC ],
    [ 0x0CBF, 0x0CBF ],
    [ 0x0CC6, 0x0CC6 ],
    [ 0x0CCC, 0x0CCD ],
    [ 0x0CE2, 0x0CE3 ],
    [ 0x0D41, 0x0D44 ],
    [ 0x0D4D, 0x0D4D ],
    [ 0x0D62, 0x0D63 ],
    [ 0x0DCA, 0x0DCA ],
    [ 0x0DD2, 0x0DD4 ],
    [ 0x0DD6, 0x0DD6 ],
    [ 0x0E31, 0x0E31 ],
    [ 0x0E34, 0x0E3A ],
    [ 0x0E47, 0x0E4E ],
    [ 0x0EB1, 0x0EB1 ],
    [ 0x0EB4, 0x0EB9 ],
    [ 0x0EBB, 0x0EBC ],
    [ 0x0EC8, 0x0ECD ],
    [ 0x0F18, 0x0F19 ],
    [ 0x0F35, 0x0F35 ],
    [ 0x0F37, 0x0F37 ],
    [ 0x0F39, 0x0F39 ],
    [ 0x0F71, 0x0F7E ],
    [ 0x0F80, 0x0F84 ],
    [ 0x0F86, 0x0F87 ],
    [ 0x0F8D, 0x0F97 ],
    [ 0x0F99, 0x0FBC ],
    [ 0x0FC6, 0x0FC6 ],
    [ 0x102D, 0x1030 ],
    [ 0x1032, 0x1037 ],
    [ 0x1039, 0x103A ],
    [ 0x103D, 0x103E ],
    [ 0x1058, 0x1059 ],
    [ 0x105E, 0x1060 ],
    [ 0x1071, 0x1074 ],
    [ 0x1082, 0x1082 ],
    [ 0x1085, 0x1086 ],
    [ 0x108D, 0x108D ],
    [ 0x109D, 0x109D ],
    [ 0x135D, 0x135F ],
    [ 0x1712, 0x1714 ],
    [ 0x1732, 0x1734 ],
    [ 0x1752, 0x1753 ],
    [ 0x1772, 0x1773 ],
    [ 0x17B7, 0x17BD ],
    [ 0x17C6, 0x17C6 ],
    [ 0x17C9, 0x17D3 ],
    [ 0x17DD, 0x17DD ],
    [ 0x180B, 0x180D ],
    [ 0x18A9, 0x18A9 ],
    [ 0x1920, 0x1922 ],
    [ 0x1927, 0x1928 ],
    [ 0x1932, 0x1932 ],
    [ 0x1939, 0x193B ],
    [ 0x1A17, 0x1A18 ],
    [ 0x1A56, 0x1A56 ],
    [ 0x1A58, 0x1A5E ],
    [ 0x1A60, 0x1A60 ],
    [ 0x1A62, 0x1A62 ],
    [ 0x1A65, 0x1A6C ],
    [ 0x1A73, 0x1A7C ],
    [ 0x1A7F, 0x1A7F ],
    [ 0x1B00, 0x1B03 ],
    [ 0x1B34, 0x1B34 ],
    [ 0x1B36, 0x1B3A ],
    [ 0x1B3C, 0x1B3C ],
    [ 0x1B42, 0x1B42 ],
    [ 0x1B6B, 0x1B73 ],
    [ 0x1B80, 0x1B81 ],
    [ 0x1BA2, 0x1BA5 ],
    [ 0x1BA8, 0x1BA9 ],
    [ 0x1BE6, 0x1BE6 ],
    [ 0x1BE8, 0x1BE9 ],
    [ 0x1BED, 0x1BED ],
    [ 0x1BEF, 0x1BF1 ],
    [ 0x1C2C, 0x1C33 ],
    [ 0x1C36, 0x1C37 ],
    [ 0x1CD0, 0x1CD2 ],
    [ 0x1CD4, 0x1CE0 ],
    [ 0x1CE2, 0x1CE8 ],
    [ 0x1CED, 0x1CED ],
    [ 0x1DC0, 0x1DE6 ],
    [ 0x1DFC, 0x1DFF ],
    [ 0x20D0, 0x20DC ],
    [ 0x20E1, 0x20E1 ],
    [ 0x20E5, 0x20F0 ],
    [ 0x2CEF, 0x2CF1 ],
    [ 0x2D7F, 0x2D7F ],
    [ 0x2DE0, 0x2DFF ],
    [ 0x302A, 0x302F ],
    [ 0x3099, 0x309A ],
    [ 0xA66F, 0xA66F ],
    [ 0xA67C, 0xA67D ],
    [ 0xA6F0, 0xA6F1 ],
    [ 0xA802, 0xA802 ],
    [ 0xA806, 0xA806 ],
    [ 0xA80B, 0xA80B ],
    [ 0xA825, 0xA826 ],
    [ 0xA8C4, 0xA8C4 ],
    [ 0xA8E0, 0xA8F1 ],
    [ 0xA926, 0xA92D ],
    [ 0xA947, 0xA951 ],
    [ 0xA980, 0xA982 ],
    [ 0xA9B3, 0xA9B3 ],
    [ 0xA9B6, 0xA9B9 ],
    [ 0xA9BC, 0xA9BC ],
    [ 0xAA29, 0xAA2E ],
    [ 0xAA31, 0xAA32 ],
    [ 0xAA35, 0xAA36 ],
    [ 0xAA43, 0xAA43 ],
    [ 0xAA4C, 0xAA4C ],
    [ 0xAAB0, 0xAAB0 ],
    [ 0xAAB2, 0xAAB4 ],
    [ 0xAAB7, 0xAAB8 ],
    [ 0xAABE, 0xAABF ],
    [ 0xAAC1, 0xAAC1 ],
    [ 0xABE5, 0xABE5 ],
    [ 0xABE8, 0xABE8 ],
    [ 0xABED, 0xABED ],
    [ 0xFB1E, 0xFB1E ],
    [ 0xFE00, 0xFE0F ],
    [ 0xFE20, 0xFE26 ],
    [ 0x101FD, 0x101FD ],
    [ 0x10A01, 0x10A03 ],
    [ 0x10A05, 0x10A06 ],
    [ 0x10A0C, 0x10A0F ],
    [ 0x10A38, 0x10A3A ],
    [ 0x10A3F, 0x10A3F ],
    [ 0x11001, 0x11001 ],
    [ 0x11038, 0x11046 ],
    [ 0x11080, 0x11081 ],
    [ 0x110B3, 0x110B6 ],
    [ 0x110B9, 0x110BA ],
    [ 0x1D167, 0x1D169 ],
    [ 0x1D17B, 0x1D182 ],
    [ 0x1D185, 0x1D18B ],
    [ 0x1D1AA, 0x1D1AD ],
    [ 0x1D242, 0x1D244 ],
    [ 0xE0100, 0xE01EF ],
    ];

    static immutable dchar tableMe[][2] =
    [
    [ 0x0488, 0x0489 ],
    [ 0x20DD, 0x20E0 ],
    [ 0x20E2, 0x20E4 ],
    [ 0xA670, 0xA672 ],
    ];

    static immutable dchar tableMc[][2] =
    [
    [ 0x0903, 0x0903 ],
    [ 0x093B, 0x093B ],
    [ 0x093E, 0x0940 ],
    [ 0x0949, 0x094C ],
    [ 0x094E, 0x094F ],
    [ 0x0982, 0x0983 ],
    [ 0x09BE, 0x09C0 ],
    [ 0x09C7, 0x09C8 ],
    [ 0x09CB, 0x09CC ],
    [ 0x09D7, 0x09D7 ],
    [ 0x0A03, 0x0A03 ],
    [ 0x0A3E, 0x0A40 ],
    [ 0x0A83, 0x0A83 ],
    [ 0x0ABE, 0x0AC0 ],
    [ 0x0AC9, 0x0AC9 ],
    [ 0x0ACB, 0x0ACC ],
    [ 0x0B02, 0x0B03 ],
    [ 0x0B3E, 0x0B3E ],
    [ 0x0B40, 0x0B40 ],
    [ 0x0B47, 0x0B48 ],
    [ 0x0B4B, 0x0B4C ],
    [ 0x0B57, 0x0B57 ],
    [ 0x0BBE, 0x0BBF ],
    [ 0x0BC1, 0x0BC2 ],
    [ 0x0BC6, 0x0BC8 ],
    [ 0x0BCA, 0x0BCC ],
    [ 0x0BD7, 0x0BD7 ],
    [ 0x0C01, 0x0C03 ],
    [ 0x0C41, 0x0C44 ],
    [ 0x0C82, 0x0C83 ],
    [ 0x0CBE, 0x0CBE ],
    [ 0x0CC0, 0x0CC4 ],
    [ 0x0CC7, 0x0CC8 ],
    [ 0x0CCA, 0x0CCB ],
    [ 0x0CD5, 0x0CD6 ],
    [ 0x0D02, 0x0D03 ],
    [ 0x0D3E, 0x0D40 ],
    [ 0x0D46, 0x0D48 ],
    [ 0x0D4A, 0x0D4C ],
    [ 0x0D57, 0x0D57 ],
    [ 0x0D82, 0x0D83 ],
    [ 0x0DCF, 0x0DD1 ],
    [ 0x0DD8, 0x0DDF ],
    [ 0x0DF2, 0x0DF3 ],
    [ 0x0F3E, 0x0F3F ],
    [ 0x0F7F, 0x0F7F ],
    [ 0x102B, 0x102C ],
    [ 0x1031, 0x1031 ],
    [ 0x1038, 0x1038 ],
    [ 0x103B, 0x103C ],
    [ 0x1056, 0x1057 ],
    [ 0x1062, 0x1064 ],
    [ 0x1067, 0x106D ],
    [ 0x1083, 0x1084 ],
    [ 0x1087, 0x108C ],
    [ 0x108F, 0x108F ],
    [ 0x109A, 0x109C ],
    [ 0x17B6, 0x17B6 ],
    [ 0x17BE, 0x17C5 ],
    [ 0x17C7, 0x17C8 ],
    [ 0x1923, 0x1926 ],
    [ 0x1929, 0x192B ],
    [ 0x1930, 0x1931 ],
    [ 0x1933, 0x1938 ],
    [ 0x19B0, 0x19C0 ],
    [ 0x19C8, 0x19C9 ],
    [ 0x1A19, 0x1A1B ],
    [ 0x1A55, 0x1A55 ],
    [ 0x1A57, 0x1A57 ],
    [ 0x1A61, 0x1A61 ],
    [ 0x1A63, 0x1A64 ],
    [ 0x1A6D, 0x1A72 ],
    [ 0x1B04, 0x1B04 ],
    [ 0x1B35, 0x1B35 ],
    [ 0x1B3B, 0x1B3B ],
    [ 0x1B3D, 0x1B41 ],
    [ 0x1B43, 0x1B44 ],
    [ 0x1B82, 0x1B82 ],
    [ 0x1BA1, 0x1BA1 ],
    [ 0x1BA6, 0x1BA7 ],
    [ 0x1BAA, 0x1BAA ],
    [ 0x1BE7, 0x1BE7 ],
    [ 0x1BEA, 0x1BEC ],
    [ 0x1BEE, 0x1BEE ],
    [ 0x1BF2, 0x1BF3 ],
    [ 0x1C24, 0x1C2B ],
    [ 0x1C34, 0x1C35 ],
    [ 0x1CE1, 0x1CE1 ],
    [ 0x1CF2, 0x1CF2 ],
    [ 0xA823, 0xA824 ],
    [ 0xA827, 0xA827 ],
    [ 0xA880, 0xA881 ],
    [ 0xA8B4, 0xA8C3 ],
    [ 0xA952, 0xA953 ],
    [ 0xA983, 0xA983 ],
    [ 0xA9B4, 0xA9B5 ],
    [ 0xA9BA, 0xA9BB ],
    [ 0xA9BD, 0xA9C0 ],
    [ 0xAA2F, 0xAA30 ],
    [ 0xAA33, 0xAA34 ],
    [ 0xAA4D, 0xAA4D ],
    [ 0xAA7B, 0xAA7B ],
    [ 0xABE3, 0xABE4 ],
    [ 0xABE6, 0xABE7 ],
    [ 0xABE9, 0xABEA ],
    [ 0xABEC, 0xABEC ],
    [ 0x11000, 0x11000 ],
    [ 0x11002, 0x11002 ],
    [ 0x11082, 0x11082 ],
    [ 0x110B0, 0x110B2 ],
    [ 0x110B7, 0x110B8 ],
    [ 0x1D165, 0x1D166 ],
    [ 0x1D16D, 0x1D172 ],
    ];

    return binarySearch!tableMn(c) || binarySearch!tableMe(c) || binarySearch!tableMc(c);
}

unittest
{
    assert(isMark('\u0300'));
    assert(isMark('\u0488'));
    assert(isMark('\u0903'));
}


/++
    Returns whether $(D c) is a Unicode numerical character
    (general Unicode category: Nd, Nl, No).

    Standards: Unicode 6.0.0.
  +/

bool isNumber(dchar c) @safe pure nothrow
{
    static immutable dchar tableNd[][2] =
    [
    [ 0x0030, 0x0039 ],
    [ 0x0660, 0x0669 ],
    [ 0x06F0, 0x06F9 ],
    [ 0x07C0, 0x07C9 ],
    [ 0x0966, 0x096F ],
    [ 0x09E6, 0x09EF ],
    [ 0x0A66, 0x0A6F ],
    [ 0x0AE6, 0x0AEF ],
    [ 0x0B66, 0x0B6F ],
    [ 0x0BE6, 0x0BEF ],
    [ 0x0C66, 0x0C6F ],
    [ 0x0CE6, 0x0CEF ],
    [ 0x0D66, 0x0D6F ],
    [ 0x0E50, 0x0E59 ],
    [ 0x0ED0, 0x0ED9 ],
    [ 0x0F20, 0x0F29 ],
    [ 0x1040, 0x1049 ],
    [ 0x1090, 0x1099 ],
    [ 0x17E0, 0x17E9 ],
    [ 0x1810, 0x1819 ],
    [ 0x1946, 0x194F ],
    [ 0x19D0, 0x19D9 ],
    [ 0x1A80, 0x1A89 ],
    [ 0x1A90, 0x1A99 ],
    [ 0x1B50, 0x1B59 ],
    [ 0x1BB0, 0x1BB9 ],
    [ 0x1C40, 0x1C49 ],
    [ 0x1C50, 0x1C59 ],
    [ 0xA620, 0xA629 ],
    [ 0xA8D0, 0xA8D9 ],
    [ 0xA900, 0xA909 ],
    [ 0xA9D0, 0xA9D9 ],
    [ 0xAA50, 0xAA59 ],
    [ 0xABF0, 0xABF9 ],
    [ 0xFF10, 0xFF19 ],
    [ 0x104A0, 0x104A9 ],
    [ 0x11066, 0x1106F ],
    [ 0x1D7CE, 0x1D7FF ],
    ];

    static immutable dchar tableNl[][2] =
    [
    [ 0x16EE, 0x16F0 ],
    [ 0x2160, 0x2182 ],
    [ 0x2185, 0x2188 ],
    [ 0x3007, 0x3007 ],
    [ 0x3021, 0x3029 ],
    [ 0x3038, 0x303A ],
    [ 0xA6E6, 0xA6EF ],
    [ 0x10140, 0x10174 ],
    [ 0x10341, 0x10341 ],
    [ 0x1034A, 0x1034A ],
    [ 0x103D1, 0x103D5 ],
    [ 0x12400, 0x12462 ],
    ];

    static immutable dchar tableNo[][2] =
    [
    [ 0x00B2, 0x00B3 ],
    [ 0x00B9, 0x00B9 ],
    [ 0x00BC, 0x00BE ],
    [ 0x09F4, 0x09F9 ],
    [ 0x0B72, 0x0B77 ],
    [ 0x0BF0, 0x0BF2 ],
    [ 0x0C78, 0x0C7E ],
    [ 0x0D70, 0x0D75 ],
    [ 0x0F2A, 0x0F33 ],
    [ 0x1369, 0x137C ],
    [ 0x17F0, 0x17F9 ],
    [ 0x19DA, 0x19DA ],
    [ 0x2070, 0x2070 ],
    [ 0x2074, 0x2079 ],
    [ 0x2080, 0x2089 ],
    [ 0x2150, 0x215F ],
    [ 0x2189, 0x2189 ],
    [ 0x2460, 0x249B ],
    [ 0x24EA, 0x24FF ],
    [ 0x2776, 0x2793 ],
    [ 0x2CFD, 0x2CFD ],
    [ 0x3192, 0x3195 ],
    [ 0x3220, 0x3229 ],
    [ 0x3251, 0x325F ],
    [ 0x3280, 0x3289 ],
    [ 0x32B1, 0x32BF ],
    [ 0xA830, 0xA835 ],
    [ 0x10107, 0x10133 ],
    [ 0x10175, 0x10178 ],
    [ 0x1018A, 0x1018A ],
    [ 0x10320, 0x10323 ],
    [ 0x10858, 0x1085F ],
    [ 0x10916, 0x1091B ],
    [ 0x10A40, 0x10A47 ],
    [ 0x10A7D, 0x10A7E ],
    [ 0x10B58, 0x10B5F ],
    [ 0x10B78, 0x10B7F ],
    [ 0x10E60, 0x10E7E ],
    [ 0x11052, 0x11065 ],
    [ 0x1D360, 0x1D371 ],
    [ 0x1F100, 0x1F10A ],
    ];

    return binarySearch!tableNd(c)
        || binarySearch!tableNl(c)
        || binarySearch!tableNo(c);
}

unittest
{
    for (dchar c = '0'; c < '9'; ++c)
    {
        assert(isNumber(c));
    }
}


/++
    Returns whether $(D c) is a Unicode punctuation character
    (general Unicode category: Pd, Ps, Pe, Pc, Po, Pi, Pf).

    Standards: Unicode 6.0.0.
  +/

bool isPunctuation(dchar c) @safe pure nothrow
{
    static immutable dchar tablePd[][2] =
    [
    [ 0x002D, 0x002D ],
    [ 0x058A, 0x058A ],
    [ 0x05BE, 0x05BE ],
    [ 0x1400, 0x1400 ],
    [ 0x1806, 0x1806 ],
    [ 0x2010, 0x2015 ],
    [ 0x2E17, 0x2E17 ],
    [ 0x2E1A, 0x2E1A ],
    [ 0x301C, 0x301C ],
    [ 0x3030, 0x3030 ],
    [ 0x30A0, 0x30A0 ],
    [ 0xFE31, 0xFE32 ],
    [ 0xFE58, 0xFE58 ],
    [ 0xFE63, 0xFE63 ],
    [ 0xFF0D, 0xFF0D ],
    ];

    static immutable dchar tablePs[][2] =
    [
    [ 0x0028, 0x0028 ],
    [ 0x005B, 0x005B ],
    [ 0x007B, 0x007B ],
    [ 0x0F3A, 0x0F3A ],
    [ 0x0F3C, 0x0F3C ],
    [ 0x169B, 0x169B ],
    [ 0x201A, 0x201A ],
    [ 0x201E, 0x201E ],
    [ 0x2045, 0x2045 ],
    [ 0x207D, 0x207D ],
    [ 0x208D, 0x208D ],
    [ 0x2329, 0x2329 ],
    [ 0x2768, 0x2768 ],
    [ 0x276A, 0x276A ],
    [ 0x276C, 0x276C ],
    [ 0x276E, 0x276E ],
    [ 0x2770, 0x2770 ],
    [ 0x2772, 0x2772 ],
    [ 0x2774, 0x2774 ],
    [ 0x27C5, 0x27C5 ],
    [ 0x27E6, 0x27E6 ],
    [ 0x27E8, 0x27E8 ],
    [ 0x27EA, 0x27EA ],
    [ 0x27EC, 0x27EC ],
    [ 0x27EE, 0x27EE ],
    [ 0x2983, 0x2983 ],
    [ 0x2985, 0x2985 ],
    [ 0x2987, 0x2987 ],
    [ 0x2989, 0x2989 ],
    [ 0x298B, 0x298B ],
    [ 0x298D, 0x298D ],
    [ 0x298F, 0x298F ],
    [ 0x2991, 0x2991 ],
    [ 0x2993, 0x2993 ],
    [ 0x2995, 0x2995 ],
    [ 0x2997, 0x2997 ],
    [ 0x29D8, 0x29D8 ],
    [ 0x29DA, 0x29DA ],
    [ 0x29FC, 0x29FC ],
    [ 0x2E22, 0x2E22 ],
    [ 0x2E24, 0x2E24 ],
    [ 0x2E26, 0x2E26 ],
    [ 0x2E28, 0x2E28 ],
    [ 0x3008, 0x3008 ],
    [ 0x300A, 0x300A ],
    [ 0x300C, 0x300C ],
    [ 0x300E, 0x300E ],
    [ 0x3010, 0x3010 ],
    [ 0x3014, 0x3014 ],
    [ 0x3016, 0x3016 ],
    [ 0x3018, 0x3018 ],
    [ 0x301A, 0x301A ],
    [ 0x301D, 0x301D ],
    [ 0xFD3E, 0xFD3E ],
    [ 0xFE17, 0xFE17 ],
    [ 0xFE35, 0xFE35 ],
    [ 0xFE37, 0xFE37 ],
    [ 0xFE39, 0xFE39 ],
    [ 0xFE3B, 0xFE3B ],
    [ 0xFE3D, 0xFE3D ],
    [ 0xFE3F, 0xFE3F ],
    [ 0xFE41, 0xFE41 ],
    [ 0xFE43, 0xFE43 ],
    [ 0xFE47, 0xFE47 ],
    [ 0xFE59, 0xFE59 ],
    [ 0xFE5B, 0xFE5B ],
    [ 0xFE5D, 0xFE5D ],
    [ 0xFF08, 0xFF08 ],
    [ 0xFF3B, 0xFF3B ],
    [ 0xFF5B, 0xFF5B ],
    [ 0xFF5F, 0xFF5F ],
    [ 0xFF62, 0xFF62 ],
    ];

    static immutable dchar tablePe[][2] =
    [
    [ 0x0029, 0x0029 ],
    [ 0x005D, 0x005D ],
    [ 0x007D, 0x007D ],
    [ 0x0F3B, 0x0F3B ],
    [ 0x0F3D, 0x0F3D ],
    [ 0x169C, 0x169C ],
    [ 0x2046, 0x2046 ],
    [ 0x207E, 0x207E ],
    [ 0x208E, 0x208E ],
    [ 0x232A, 0x232A ],
    [ 0x2769, 0x2769 ],
    [ 0x276B, 0x276B ],
    [ 0x276D, 0x276D ],
    [ 0x276F, 0x276F ],
    [ 0x2771, 0x2771 ],
    [ 0x2773, 0x2773 ],
    [ 0x2775, 0x2775 ],
    [ 0x27C6, 0x27C6 ],
    [ 0x27E7, 0x27E7 ],
    [ 0x27E9, 0x27E9 ],
    [ 0x27EB, 0x27EB ],
    [ 0x27ED, 0x27ED ],
    [ 0x27EF, 0x27EF ],
    [ 0x2984, 0x2984 ],
    [ 0x2986, 0x2986 ],
    [ 0x2988, 0x2988 ],
    [ 0x298A, 0x298A ],
    [ 0x298C, 0x298C ],
    [ 0x298E, 0x298E ],
    [ 0x2990, 0x2990 ],
    [ 0x2992, 0x2992 ],
    [ 0x2994, 0x2994 ],
    [ 0x2996, 0x2996 ],
    [ 0x2998, 0x2998 ],
    [ 0x29D9, 0x29D9 ],
    [ 0x29DB, 0x29DB ],
    [ 0x29FD, 0x29FD ],
    [ 0x2E23, 0x2E23 ],
    [ 0x2E25, 0x2E25 ],
    [ 0x2E27, 0x2E27 ],
    [ 0x2E29, 0x2E29 ],
    [ 0x3009, 0x3009 ],
    [ 0x300B, 0x300B ],
    [ 0x300D, 0x300D ],
    [ 0x300F, 0x300F ],
    [ 0x3011, 0x3011 ],
    [ 0x3015, 0x3015 ],
    [ 0x3017, 0x3017 ],
    [ 0x3019, 0x3019 ],
    [ 0x301B, 0x301B ],
    [ 0x301E, 0x301F ],
    [ 0xFD3F, 0xFD3F ],
    [ 0xFE18, 0xFE18 ],
    [ 0xFE36, 0xFE36 ],
    [ 0xFE38, 0xFE38 ],
    [ 0xFE3A, 0xFE3A ],
    [ 0xFE3C, 0xFE3C ],
    [ 0xFE3E, 0xFE3E ],
    [ 0xFE40, 0xFE40 ],
    [ 0xFE42, 0xFE42 ],
    [ 0xFE44, 0xFE44 ],
    [ 0xFE48, 0xFE48 ],
    [ 0xFE5A, 0xFE5A ],
    [ 0xFE5C, 0xFE5C ],
    [ 0xFE5E, 0xFE5E ],
    [ 0xFF09, 0xFF09 ],
    [ 0xFF3D, 0xFF3D ],
    [ 0xFF5D, 0xFF5D ],
    [ 0xFF60, 0xFF60 ],
    [ 0xFF63, 0xFF63 ],
    ];

    static immutable dchar tablePc[][2] =
    [
    [ 0x005F, 0x005F ],
    [ 0x203F, 0x2040 ],
    [ 0x2054, 0x2054 ],
    [ 0xFE33, 0xFE34 ],
    [ 0xFE4D, 0xFE4F ],
    [ 0xFF3F, 0xFF3F ],
    ];

    static immutable dchar tablePo[][2] =
    [
    [ 0x0021, 0x0023 ],
    [ 0x0025, 0x0027 ],
    [ 0x002A, 0x002A ],
    [ 0x002C, 0x002C ],
    [ 0x002E, 0x002F ],
    [ 0x003A, 0x003B ],
    [ 0x003F, 0x0040 ],
    [ 0x005C, 0x005C ],
    [ 0x00A1, 0x00A1 ],
    [ 0x00B7, 0x00B7 ],
    [ 0x00BF, 0x00BF ],
    [ 0x037E, 0x037E ],
    [ 0x0387, 0x0387 ],
    [ 0x055A, 0x055F ],
    [ 0x0589, 0x0589 ],
    [ 0x05C0, 0x05C0 ],
    [ 0x05C3, 0x05C3 ],
    [ 0x05C6, 0x05C6 ],
    [ 0x05F3, 0x05F4 ],
    [ 0x0609, 0x060A ],
    [ 0x060C, 0x060D ],
    [ 0x061B, 0x061B ],
    [ 0x061E, 0x061F ],
    [ 0x066A, 0x066D ],
    [ 0x06D4, 0x06D4 ],
    [ 0x0700, 0x070D ],
    [ 0x07F7, 0x07F9 ],
    [ 0x0830, 0x083E ],
    [ 0x085E, 0x085E ],
    [ 0x0964, 0x0965 ],
    [ 0x0970, 0x0970 ],
    [ 0x0DF4, 0x0DF4 ],
    [ 0x0E4F, 0x0E4F ],
    [ 0x0E5A, 0x0E5B ],
    [ 0x0F04, 0x0F12 ],
    [ 0x0F85, 0x0F85 ],
    [ 0x0FD0, 0x0FD4 ],
    [ 0x0FD9, 0x0FDA ],
    [ 0x104A, 0x104F ],
    [ 0x10FB, 0x10FB ],
    [ 0x1361, 0x1368 ],
    [ 0x166D, 0x166E ],
    [ 0x16EB, 0x16ED ],
    [ 0x1735, 0x1736 ],
    [ 0x17D4, 0x17D6 ],
    [ 0x17D8, 0x17DA ],
    [ 0x1800, 0x1805 ],
    [ 0x1807, 0x180A ],
    [ 0x1944, 0x1945 ],
    [ 0x1A1E, 0x1A1F ],
    [ 0x1AA0, 0x1AA6 ],
    [ 0x1AA8, 0x1AAD ],
    [ 0x1B5A, 0x1B60 ],
    [ 0x1BFC, 0x1BFF ],
    [ 0x1C3B, 0x1C3F ],
    [ 0x1C7E, 0x1C7F ],
    [ 0x1CD3, 0x1CD3 ],
    [ 0x2016, 0x2017 ],
    [ 0x2020, 0x2027 ],
    [ 0x2030, 0x2038 ],
    [ 0x203B, 0x203E ],
    [ 0x2041, 0x2043 ],
    [ 0x2047, 0x2051 ],
    [ 0x2053, 0x2053 ],
    [ 0x2055, 0x205E ],
    [ 0x2CF9, 0x2CFC ],
    [ 0x2CFE, 0x2CFF ],
    [ 0x2D70, 0x2D70 ],
    [ 0x2E00, 0x2E01 ],
    [ 0x2E06, 0x2E08 ],
    [ 0x2E0B, 0x2E0B ],
    [ 0x2E0E, 0x2E16 ],
    [ 0x2E18, 0x2E19 ],
    [ 0x2E1B, 0x2E1B ],
    [ 0x2E1E, 0x2E1F ],
    [ 0x2E2A, 0x2E2E ],
    [ 0x2E30, 0x2E31 ],
    [ 0x3001, 0x3003 ],
    [ 0x303D, 0x303D ],
    [ 0x30FB, 0x30FB ],
    [ 0xA4FE, 0xA4FF ],
    [ 0xA60D, 0xA60F ],
    [ 0xA673, 0xA673 ],
    [ 0xA67E, 0xA67E ],
    [ 0xA6F2, 0xA6F7 ],
    [ 0xA874, 0xA877 ],
    [ 0xA8CE, 0xA8CF ],
    [ 0xA8F8, 0xA8FA ],
    [ 0xA92E, 0xA92F ],
    [ 0xA95F, 0xA95F ],
    [ 0xA9C1, 0xA9CD ],
    [ 0xA9DE, 0xA9DF ],
    [ 0xAA5C, 0xAA5F ],
    [ 0xAADE, 0xAADF ],
    [ 0xABEB, 0xABEB ],
    [ 0xFE10, 0xFE16 ],
    [ 0xFE19, 0xFE19 ],
    [ 0xFE30, 0xFE30 ],
    [ 0xFE45, 0xFE46 ],
    [ 0xFE49, 0xFE4C ],
    [ 0xFE50, 0xFE52 ],
    [ 0xFE54, 0xFE57 ],
    [ 0xFE5F, 0xFE61 ],
    [ 0xFE68, 0xFE68 ],
    [ 0xFE6A, 0xFE6B ],
    [ 0xFF01, 0xFF03 ],
    [ 0xFF05, 0xFF07 ],
    [ 0xFF0A, 0xFF0A ],
    [ 0xFF0C, 0xFF0C ],
    [ 0xFF0E, 0xFF0F ],
    [ 0xFF1A, 0xFF1B ],
    [ 0xFF1F, 0xFF20 ],
    [ 0xFF3C, 0xFF3C ],
    [ 0xFF61, 0xFF61 ],
    [ 0xFF64, 0xFF65 ],
    [ 0x10100, 0x10101 ],
    [ 0x1039F, 0x1039F ],
    [ 0x103D0, 0x103D0 ],
    [ 0x10857, 0x10857 ],
    [ 0x1091F, 0x1091F ],
    [ 0x1093F, 0x1093F ],
    [ 0x10A50, 0x10A58 ],
    [ 0x10A7F, 0x10A7F ],
    [ 0x10B39, 0x10B3F ],
    [ 0x11047, 0x1104D ],
    [ 0x110BB, 0x110BC ],
    [ 0x110BE, 0x110C1 ],
    [ 0x12470, 0x12473 ],
    ];

    static immutable dchar tablePi[][2] =
    [
    [ 0x00AB, 0x00AB ],
    [ 0x2018, 0x2018 ],
    [ 0x201B, 0x201C ],
    [ 0x201F, 0x201F ],
    [ 0x2039, 0x2039 ],
    [ 0x2E02, 0x2E02 ],
    [ 0x2E04, 0x2E04 ],
    [ 0x2E09, 0x2E09 ],
    [ 0x2E0C, 0x2E0C ],
    [ 0x2E1C, 0x2E1C ],
    [ 0x2E20, 0x2E20 ],
    ];

    static immutable dchar tablePf[][2] =
    [
    [ 0x00BB, 0x00BB ],
    [ 0x2019, 0x2019 ],
    [ 0x201D, 0x201D ],
    [ 0x203A, 0x203A ],
    [ 0x2E03, 0x2E03 ],
    [ 0x2E05, 0x2E05 ],
    [ 0x2E0A, 0x2E0A ],
    [ 0x2E0D, 0x2E0D ],
    [ 0x2E1D, 0x2E1D ],
    [ 0x2E21, 0x2E21 ],
    ];

    return binarySearch!tablePd(c)
        || binarySearch!tablePs(c)
        || binarySearch!tablePe(c)
        || binarySearch!tablePc(c)
        || binarySearch!tablePo(c)
        || binarySearch!tablePi(c)
        || binarySearch!tablePf(c);
}

unittest
{
    assert(isPunctuation('\u0021'));
    assert(isPunctuation('\u0028'));
    assert(isPunctuation('\u0029'));
    assert(isPunctuation('\u002D'));
    assert(isPunctuation('\u005F'));
    assert(isPunctuation('\u00AB'));
    assert(isPunctuation('\u00BB'));
}


/++
    Returns whether $(D c) is a Unicode symbol character
    (general Unicode category: Sm, Sc, Sk, So)

    Standards: Unicode 6.0.0.
  +/
bool isSymbol(dchar c) @safe pure nothrow
{
    static immutable dchar tableSm[][2] =
    [
    [ 0x002B, 0x002B ],
    [ 0x003C, 0x003E ],
    [ 0x007C, 0x007C ],
    [ 0x007E, 0x007E ],
    [ 0x00AC, 0x00AC ],
    [ 0x00B1, 0x00B1 ],
    [ 0x00D7, 0x00D7 ],
    [ 0x00F7, 0x00F7 ],
    [ 0x03F6, 0x03F6 ],
    [ 0x0606, 0x0608 ],
    [ 0x2044, 0x2044 ],
    [ 0x2052, 0x2052 ],
    [ 0x207A, 0x207C ],
    [ 0x208A, 0x208C ],
    [ 0x2118, 0x2118 ],
    [ 0x2140, 0x2144 ],
    [ 0x214B, 0x214B ],
    [ 0x2190, 0x2194 ],
    [ 0x219A, 0x219B ],
    [ 0x21A0, 0x21A0 ],
    [ 0x21A3, 0x21A3 ],
    [ 0x21A6, 0x21A6 ],
    [ 0x21AE, 0x21AE ],
    [ 0x21CE, 0x21CF ],
    [ 0x21D2, 0x21D2 ],
    [ 0x21D4, 0x21D4 ],
    [ 0x21F4, 0x22FF ],
    [ 0x2308, 0x230B ],
    [ 0x2320, 0x2321 ],
    [ 0x237C, 0x237C ],
    [ 0x239B, 0x23B3 ],
    [ 0x23DC, 0x23E1 ],
    [ 0x25B7, 0x25B7 ],
    [ 0x25C1, 0x25C1 ],
    [ 0x25F8, 0x25FF ],
    [ 0x266F, 0x266F ],
    [ 0x27C0, 0x27C4 ],
    [ 0x27C7, 0x27CA ],
    [ 0x27CC, 0x27CC ],
    [ 0x27CE, 0x27E5 ],
    [ 0x27F0, 0x27FF ],
    [ 0x2900, 0x2982 ],
    [ 0x2999, 0x29D7 ],
    [ 0x29DC, 0x29FB ],
    [ 0x29FE, 0x2AFF ],
    [ 0x2B30, 0x2B44 ],
    [ 0x2B47, 0x2B4C ],
    [ 0xFB29, 0xFB29 ],
    [ 0xFE62, 0xFE62 ],
    [ 0xFE64, 0xFE66 ],
    [ 0xFF0B, 0xFF0B ],
    [ 0xFF1C, 0xFF1E ],
    [ 0xFF5C, 0xFF5C ],
    [ 0xFF5E, 0xFF5E ],
    [ 0xFFE2, 0xFFE2 ],
    [ 0xFFE9, 0xFFEC ],
    [ 0x1D6C1, 0x1D6C1 ],
    [ 0x1D6DB, 0x1D6DB ],
    [ 0x1D6FB, 0x1D6FB ],
    [ 0x1D715, 0x1D715 ],
    [ 0x1D735, 0x1D735 ],
    [ 0x1D74F, 0x1D74F ],
    [ 0x1D76F, 0x1D76F ],
    [ 0x1D789, 0x1D789 ],
    [ 0x1D7A9, 0x1D7A9 ],
    [ 0x1D7C3, 0x1D7C3 ],
    ];

    static immutable dchar tableSc[][2] =
    [
    [ 0x0024, 0x0024 ],
    [ 0x00A2, 0x00A5 ],
    [ 0x060B, 0x060B ],
    [ 0x09F2, 0x09F3 ],
    [ 0x09FB, 0x09FB ],
    [ 0x0AF1, 0x0AF1 ],
    [ 0x0BF9, 0x0BF9 ],
    [ 0x0E3F, 0x0E3F ],
    [ 0x17DB, 0x17DB ],
    [ 0x20A0, 0x20B9 ],
    [ 0xA838, 0xA838 ],
    [ 0xFDFC, 0xFDFC ],
    [ 0xFE69, 0xFE69 ],
    [ 0xFF04, 0xFF04 ],
    [ 0xFFE0, 0xFFE1 ],
    [ 0xFFE5, 0xFFE6 ],
    ];

    static immutable dchar tableSk[][2] =
    [
    [ 0x005E, 0x005E ],
    [ 0x0060, 0x0060 ],
    [ 0x00A8, 0x00A8 ],
    [ 0x00AF, 0x00AF ],
    [ 0x00B4, 0x00B4 ],
    [ 0x00B8, 0x00B8 ],
    [ 0x02C2, 0x02C5 ],
    [ 0x02D2, 0x02DF ],
    [ 0x02E5, 0x02EB ],
    [ 0x02ED, 0x02ED ],
    [ 0x02EF, 0x02FF ],
    [ 0x0375, 0x0375 ],
    [ 0x0384, 0x0385 ],
    [ 0x1FBD, 0x1FBD ],
    [ 0x1FBF, 0x1FC1 ],
    [ 0x1FCD, 0x1FCF ],
    [ 0x1FDD, 0x1FDF ],
    [ 0x1FED, 0x1FEF ],
    [ 0x1FFD, 0x1FFE ],
    [ 0x309B, 0x309C ],
    [ 0xA700, 0xA716 ],
    [ 0xA720, 0xA721 ],
    [ 0xA789, 0xA78A ],
    [ 0xFBB2, 0xFBC1 ],
    [ 0xFF3E, 0xFF3E ],
    [ 0xFF40, 0xFF40 ],
    [ 0xFFE3, 0xFFE3 ],
    ];

    static immutable dchar tableSo[][2] =
    [
    [ 0x00A6, 0x00A7 ],
    [ 0x00A9, 0x00A9 ],
    [ 0x00AE, 0x00AE ],
    [ 0x00B0, 0x00B0 ],
    [ 0x00B6, 0x00B6 ],
    [ 0x0482, 0x0482 ],
    [ 0x060E, 0x060F ],
    [ 0x06DE, 0x06DE ],
    [ 0x06E9, 0x06E9 ],
    [ 0x06FD, 0x06FE ],
    [ 0x07F6, 0x07F6 ],
    [ 0x09FA, 0x09FA ],
    [ 0x0B70, 0x0B70 ],
    [ 0x0BF3, 0x0BF8 ],
    [ 0x0BFA, 0x0BFA ],
    [ 0x0C7F, 0x0C7F ],
    [ 0x0D79, 0x0D79 ],
    [ 0x0F01, 0x0F03 ],
    [ 0x0F13, 0x0F17 ],
    [ 0x0F1A, 0x0F1F ],
    [ 0x0F34, 0x0F34 ],
    [ 0x0F36, 0x0F36 ],
    [ 0x0F38, 0x0F38 ],
    [ 0x0FBE, 0x0FC5 ],
    [ 0x0FC7, 0x0FCC ],
    [ 0x0FCE, 0x0FCF ],
    [ 0x0FD5, 0x0FD8 ],
    [ 0x109E, 0x109F ],
    [ 0x1360, 0x1360 ],
    [ 0x1390, 0x1399 ],
    [ 0x1940, 0x1940 ],
    [ 0x19DE, 0x19FF ],
    [ 0x1B61, 0x1B6A ],
    [ 0x1B74, 0x1B7C ],
    [ 0x2100, 0x2101 ],
    [ 0x2103, 0x2106 ],
    [ 0x2108, 0x2109 ],
    [ 0x2114, 0x2114 ],
    [ 0x2116, 0x2117 ],
    [ 0x211E, 0x2123 ],
    [ 0x2125, 0x2125 ],
    [ 0x2127, 0x2127 ],
    [ 0x2129, 0x2129 ],
    [ 0x212E, 0x212E ],
    [ 0x213A, 0x213B ],
    [ 0x214A, 0x214A ],
    [ 0x214C, 0x214D ],
    [ 0x214F, 0x214F ],
    [ 0x2195, 0x2199 ],
    [ 0x219C, 0x219F ],
    [ 0x21A1, 0x21A2 ],
    [ 0x21A4, 0x21A5 ],
    [ 0x21A7, 0x21AD ],
    [ 0x21AF, 0x21CD ],
    [ 0x21D0, 0x21D1 ],
    [ 0x21D3, 0x21D3 ],
    [ 0x21D5, 0x21F3 ],
    [ 0x2300, 0x2307 ],
    [ 0x230C, 0x231F ],
    [ 0x2322, 0x2328 ],
    [ 0x232B, 0x237B ],
    [ 0x237D, 0x239A ],
    [ 0x23B4, 0x23DB ],
    [ 0x23E2, 0x23F3 ],
    [ 0x2400, 0x2426 ],
    [ 0x2440, 0x244A ],
    [ 0x249C, 0x24E9 ],
    [ 0x2500, 0x25B6 ],
    [ 0x25B8, 0x25C0 ],
    [ 0x25C2, 0x25F7 ],
    [ 0x2600, 0x266E ],
    [ 0x2670, 0x26FF ],
    [ 0x2701, 0x2767 ],
    [ 0x2794, 0x27BF ],
    [ 0x2800, 0x28FF ],
    [ 0x2B00, 0x2B2F ],
    [ 0x2B45, 0x2B46 ],
    [ 0x2B50, 0x2B59 ],
    [ 0x2CE5, 0x2CEA ],
    [ 0x2E80, 0x2E99 ],
    [ 0x2E9B, 0x2EF3 ],
    [ 0x2F00, 0x2FD5 ],
    [ 0x2FF0, 0x2FFB ],
    [ 0x3004, 0x3004 ],
    [ 0x3012, 0x3013 ],
    [ 0x3020, 0x3020 ],
    [ 0x3036, 0x3037 ],
    [ 0x303E, 0x303F ],
    [ 0x3190, 0x3191 ],
    [ 0x3196, 0x319F ],
    [ 0x31C0, 0x31E3 ],
    [ 0x3200, 0x321E ],
    [ 0x322A, 0x3250 ],
    [ 0x3260, 0x327F ],
    [ 0x328A, 0x32B0 ],
    [ 0x32C0, 0x32FE ],
    [ 0x3300, 0x33FF ],
    [ 0x4DC0, 0x4DFF ],
    [ 0xA490, 0xA4C6 ],
    [ 0xA828, 0xA82B ],
    [ 0xA836, 0xA837 ],
    [ 0xA839, 0xA839 ],
    [ 0xAA77, 0xAA79 ],
    [ 0xFDFD, 0xFDFD ],
    [ 0xFFE4, 0xFFE4 ],
    [ 0xFFE8, 0xFFE8 ],
    [ 0xFFED, 0xFFEE ],
    [ 0xFFFC, 0xFFFD ],
    [ 0x10102, 0x10102 ],
    [ 0x10137, 0x1013F ],
    [ 0x10179, 0x10189 ],
    [ 0x10190, 0x1019B ],
    [ 0x101D0, 0x101FC ],
    [ 0x1D000, 0x1D0F5 ],
    [ 0x1D100, 0x1D126 ],
    [ 0x1D129, 0x1D164 ],
    [ 0x1D16A, 0x1D16C ],
    [ 0x1D183, 0x1D184 ],
    [ 0x1D18C, 0x1D1A9 ],
    [ 0x1D1AE, 0x1D1DD ],
    [ 0x1D200, 0x1D241 ],
    [ 0x1D245, 0x1D245 ],
    [ 0x1D300, 0x1D356 ],
    [ 0x1F000, 0x1F02B ],
    [ 0x1F030, 0x1F093 ],
    [ 0x1F0A0, 0x1F0AE ],
    [ 0x1F0B1, 0x1F0BE ],
    [ 0x1F0C1, 0x1F0CF ],
    [ 0x1F0D1, 0x1F0DF ],
    [ 0x1F110, 0x1F12E ],
    [ 0x1F130, 0x1F169 ],
    [ 0x1F170, 0x1F19A ],
    [ 0x1F1E6, 0x1F202 ],
    [ 0x1F210, 0x1F23A ],
    [ 0x1F240, 0x1F248 ],
    [ 0x1F250, 0x1F251 ],
    [ 0x1F300, 0x1F320 ],
    [ 0x1F330, 0x1F335 ],
    [ 0x1F337, 0x1F37C ],
    [ 0x1F380, 0x1F393 ],
    [ 0x1F3A0, 0x1F3C4 ],
    [ 0x1F3C6, 0x1F3CA ],
    [ 0x1F3E0, 0x1F3F0 ],
    [ 0x1F400, 0x1F43E ],
    [ 0x1F440, 0x1F440 ],
    [ 0x1F442, 0x1F4F7 ],
    [ 0x1F4F9, 0x1F4FC ],
    [ 0x1F500, 0x1F53D ],
    [ 0x1F550, 0x1F567 ],
    [ 0x1F5FB, 0x1F5FF ],
    [ 0x1F601, 0x1F610 ],
    [ 0x1F612, 0x1F614 ],
    [ 0x1F616, 0x1F616 ],
    [ 0x1F618, 0x1F618 ],
    [ 0x1F61A, 0x1F61A ],
    [ 0x1F61C, 0x1F61E ],
    [ 0x1F620, 0x1F625 ],
    [ 0x1F628, 0x1F62B ],
    [ 0x1F62D, 0x1F62D ],
    [ 0x1F630, 0x1F633 ],
    [ 0x1F635, 0x1F640 ],
    [ 0x1F645, 0x1F64F ],
    [ 0x1F680, 0x1F6C5 ],
    [ 0x1F700, 0x1F773 ],
    ];

    return binarySearch!tableSm(c)
        || binarySearch!tableSc(c)
        || binarySearch!tableSk(c)
        || binarySearch!tableSo(c);
}

unittest
{
    assert(isSymbol('\u0024'));
    assert(isSymbol('\u002B'));
    assert(isSymbol('\u005E'));
    assert(isSymbol('\u00A6'));
}


/++
    Returns whether $(D c) is a Unicode whitespace character
    (general Unicode category: Zs)

    Standards: Unicode 6.0.0.
  +/
bool isSpace(dchar c) @safe pure nothrow
{
    return (c == 0x0020 ||
        c == 0x00A0 || c == 0x1680 || c == 0x180E ||
        (0x2000 <= c && c <= 0x200A) ||
        c == 0x202F || c == 0x205F || c == 0x3000);
}

unittest
{
    assert(isSpace('\u0020'));
}


/++
    Returns whether $(D c) is a Unicode graphical character
    (general Unicode category: L, M, N, P, S, Zs).

    Standards: Unicode 6.0.0.
  +/

bool isGraphical(dchar c) @safe pure nothrow
{
    return isAlpha(c) || isNumber(c) || isSpace(c)
        || isMark(c) || isPunctuation(c) || isSymbol(c);
}

unittest
{
}


/++
    Returns whether $(D c) is a Unicode control character
    (general Unicode category: Cc)

    Standards: Unicode 6.0.0.
  +/

bool isControl(dchar c) @safe pure nothrow
{
    return (c <= 0x1F || (0x80 <= c && c <= 0x9F));
}

unittest
{
    assert(isControl('\u0000'));
}


/++
    Returns whether $(D c) is a Unicode formatting character
    (general Unicode category: Cf)

    Standards: Unicode 6.0.0.
  +/
bool isFormat(dchar c) @safe pure nothrow
{
    static immutable dchar tableCf[][2] =
    [
    [ 0x00AD, 0x00AD ],
    [ 0x0600, 0x0603 ],
    [ 0x06DD, 0x06DD ],
    [ 0x070F, 0x070F ],
    [ 0x17B4, 0x17B5 ],
    [ 0x200B, 0x200F ],
    [ 0x202A, 0x202E ],
    [ 0x2060, 0x2064 ],
    [ 0x206A, 0x206F ],
    [ 0xFEFF, 0xFEFF ],
    [ 0xFFF9, 0xFFFB ],
    [ 0x110BD, 0x110BD ],
    [ 0x1D173, 0x1D17A ],
    [ 0xE0001, 0xE0001 ],
    [ 0xE0020, 0xE007F ],
    ];

    return binarySearch!tableCf(c);
}

unittest
{
    assert(isFormat('\u00AD'));
}


/++
    Returns whether $(D c) is a Unicode Private Use character
    (general Unicode category: Co)

    Standards: Unicode 6.0.0.
  +/
bool isPrivateUse(dchar c) @safe pure nothrow
{
    return (0x00_E000 <= c && c <= 0x00_F8FF)
        || (0x0F_0000 <= c && c <= 0x0F_FFFD)
        || (0x10_0000 <= c && c <= 0x10_FFFD);
}


unittest
{
}


/++
    Returns whether $(D c) is a Unicode surrogate character
    (general Unicode category: Cs)

    Standards: Unicode 6.0.0.
  +/
bool isSurrogate(dchar c) @safe pure nothrow
{
    return (0xD800 <= c && c <= 0xDFFF);
}

/++
    Returns whether $(D c) is a Unicode high surrogate (lead surrogate).

    Standards: Unicode 2.0.
  +/
bool isSurrogateHi(dchar c) @safe pure nothrow
{
    return (0xD800 <= c && c <= 0xDBFF);
}

/++
    Returns whether $(D c) is a Unicode low surrogate (trail surrogate).

    Standards: Unicode 2.0.
  +/
bool isSurrogateLo(dchar c) @safe pure nothrow
{
    return (0xDC00 <= c && c <= 0xDFFF);
}

unittest
{
}


/++
    Returns whether $(D c) is a Unicode non-character
    (general Unicode category: Cn)

    Standards: Unicode 6.0.0.
  +/
bool isNonCharacter(dchar c) @safe pure nothrow
{
    static immutable dchar table[][2] =
    [
    [ 0x0378, 0x0379 ],
    [ 0x037F, 0x0383 ],
    [ 0x038B, 0x038B ],
    [ 0x038D, 0x038D ],
    [ 0x03A2, 0x03A2 ],
    [ 0x0528, 0x0530 ],
    [ 0x0557, 0x0558 ],
    [ 0x0560, 0x0560 ],
    [ 0x0588, 0x0588 ],
    [ 0x058B, 0x0590 ],
    [ 0x05C8, 0x05CF ],
    [ 0x05EB, 0x05EF ],
    [ 0x05F5, 0x05FF ],
    [ 0x0604, 0x0605 ],
    [ 0x061C, 0x061D ],
    [ 0x070E, 0x070E ],
    [ 0x074B, 0x074C ],
    [ 0x07B2, 0x07BF ],
    [ 0x07FB, 0x07FF ],
    [ 0x082E, 0x082F ],
    [ 0x083F, 0x083F ],
    [ 0x085C, 0x085D ],
    [ 0x085F, 0x08FF ],
    [ 0x0978, 0x0978 ],
    [ 0x0980, 0x0980 ],
    [ 0x0984, 0x0984 ],
    [ 0x098D, 0x098E ],
    [ 0x0991, 0x0992 ],
    [ 0x09A9, 0x09A9 ],
    [ 0x09B1, 0x09B1 ],
    [ 0x09B3, 0x09B5 ],
    [ 0x09BA, 0x09BB ],
    [ 0x09C5, 0x09C6 ],
    [ 0x09C9, 0x09CA ],
    [ 0x09CF, 0x09D6 ],
    [ 0x09D8, 0x09DB ],
    [ 0x09DE, 0x09DE ],
    [ 0x09E4, 0x09E5 ],
    [ 0x09FC, 0x0A00 ],
    [ 0x0A04, 0x0A04 ],
    [ 0x0A0B, 0x0A0E ],
    [ 0x0A11, 0x0A12 ],
    [ 0x0A29, 0x0A29 ],
    [ 0x0A31, 0x0A31 ],
    [ 0x0A34, 0x0A34 ],
    [ 0x0A37, 0x0A37 ],
    [ 0x0A3A, 0x0A3B ],
    [ 0x0A3D, 0x0A3D ],
    [ 0x0A43, 0x0A46 ],
    [ 0x0A49, 0x0A4A ],
    [ 0x0A4E, 0x0A50 ],
    [ 0x0A52, 0x0A58 ],
    [ 0x0A5D, 0x0A5D ],
    [ 0x0A5F, 0x0A65 ],
    [ 0x0A76, 0x0A80 ],
    [ 0x0A84, 0x0A84 ],
    [ 0x0A8E, 0x0A8E ],
    [ 0x0A92, 0x0A92 ],
    [ 0x0AA9, 0x0AA9 ],
    [ 0x0AB1, 0x0AB1 ],
    [ 0x0AB4, 0x0AB4 ],
    [ 0x0ABA, 0x0ABB ],
    [ 0x0AC6, 0x0AC6 ],
    [ 0x0ACA, 0x0ACA ],
    [ 0x0ACE, 0x0ACF ],
    [ 0x0AD1, 0x0ADF ],
    [ 0x0AE4, 0x0AE5 ],
    [ 0x0AF0, 0x0AF0 ],
    [ 0x0AF2, 0x0B00 ],
    [ 0x0B04, 0x0B04 ],
    [ 0x0B0D, 0x0B0E ],
    [ 0x0B11, 0x0B12 ],
    [ 0x0B29, 0x0B29 ],
    [ 0x0B31, 0x0B31 ],
    [ 0x0B34, 0x0B34 ],
    [ 0x0B3A, 0x0B3B ],
    [ 0x0B45, 0x0B46 ],
    [ 0x0B49, 0x0B4A ],
    [ 0x0B4E, 0x0B55 ],
    [ 0x0B58, 0x0B5B ],
    [ 0x0B5E, 0x0B5E ],
    [ 0x0B64, 0x0B65 ],
    [ 0x0B78, 0x0B81 ],
    [ 0x0B84, 0x0B84 ],
    [ 0x0B8B, 0x0B8D ],
    [ 0x0B91, 0x0B91 ],
    [ 0x0B96, 0x0B98 ],
    [ 0x0B9B, 0x0B9B ],
    [ 0x0B9D, 0x0B9D ],
    [ 0x0BA0, 0x0BA2 ],
    [ 0x0BA5, 0x0BA7 ],
    [ 0x0BAB, 0x0BAD ],
    [ 0x0BBA, 0x0BBD ],
    [ 0x0BC3, 0x0BC5 ],
    [ 0x0BC9, 0x0BC9 ],
    [ 0x0BCE, 0x0BCF ],
    [ 0x0BD1, 0x0BD6 ],
    [ 0x0BD8, 0x0BE5 ],
    [ 0x0BFB, 0x0C00 ],
    [ 0x0C04, 0x0C04 ],
    [ 0x0C0D, 0x0C0D ],
    [ 0x0C11, 0x0C11 ],
    [ 0x0C29, 0x0C29 ],
    [ 0x0C34, 0x0C34 ],
    [ 0x0C3A, 0x0C3C ],
    [ 0x0C45, 0x0C45 ],
    [ 0x0C49, 0x0C49 ],
    [ 0x0C4E, 0x0C54 ],
    [ 0x0C57, 0x0C57 ],
    [ 0x0C5A, 0x0C5F ],
    [ 0x0C64, 0x0C65 ],
    [ 0x0C70, 0x0C77 ],
    [ 0x0C80, 0x0C81 ],
    [ 0x0C84, 0x0C84 ],
    [ 0x0C8D, 0x0C8D ],
    [ 0x0C91, 0x0C91 ],
    [ 0x0CA9, 0x0CA9 ],
    [ 0x0CB4, 0x0CB4 ],
    [ 0x0CBA, 0x0CBB ],
    [ 0x0CC5, 0x0CC5 ],
    [ 0x0CC9, 0x0CC9 ],
    [ 0x0CCE, 0x0CD4 ],
    [ 0x0CD7, 0x0CDD ],
    [ 0x0CDF, 0x0CDF ],
    [ 0x0CE4, 0x0CE5 ],
    [ 0x0CF0, 0x0CF0 ],
    [ 0x0CF3, 0x0D01 ],
    [ 0x0D04, 0x0D04 ],
    [ 0x0D0D, 0x0D0D ],
    [ 0x0D11, 0x0D11 ],
    [ 0x0D3B, 0x0D3C ],
    [ 0x0D45, 0x0D45 ],
    [ 0x0D49, 0x0D49 ],
    [ 0x0D4F, 0x0D56 ],
    [ 0x0D58, 0x0D5F ],
    [ 0x0D64, 0x0D65 ],
    [ 0x0D76, 0x0D78 ],
    [ 0x0D80, 0x0D81 ],
    [ 0x0D84, 0x0D84 ],
    [ 0x0D97, 0x0D99 ],
    [ 0x0DB2, 0x0DB2 ],
    [ 0x0DBC, 0x0DBC ],
    [ 0x0DBE, 0x0DBF ],
    [ 0x0DC7, 0x0DC9 ],
    [ 0x0DCB, 0x0DCE ],
    [ 0x0DD5, 0x0DD5 ],
    [ 0x0DD7, 0x0DD7 ],
    [ 0x0DE0, 0x0DF1 ],
    [ 0x0DF5, 0x0E00 ],
    [ 0x0E3B, 0x0E3E ],
    [ 0x0E5C, 0x0E80 ],
    [ 0x0E83, 0x0E83 ],
    [ 0x0E85, 0x0E86 ],
    [ 0x0E89, 0x0E89 ],
    [ 0x0E8B, 0x0E8C ],
    [ 0x0E8E, 0x0E93 ],
    [ 0x0E98, 0x0E98 ],
    [ 0x0EA0, 0x0EA0 ],
    [ 0x0EA4, 0x0EA4 ],
    [ 0x0EA6, 0x0EA6 ],
    [ 0x0EA8, 0x0EA9 ],
    [ 0x0EAC, 0x0EAC ],
    [ 0x0EBA, 0x0EBA ],
    [ 0x0EBE, 0x0EBF ],
    [ 0x0EC5, 0x0EC5 ],
    [ 0x0EC7, 0x0EC7 ],
    [ 0x0ECE, 0x0ECF ],
    [ 0x0EDA, 0x0EDB ],
    [ 0x0EDE, 0x0EFF ],
    [ 0x0F48, 0x0F48 ],
    [ 0x0F6D, 0x0F70 ],
    [ 0x0F98, 0x0F98 ],
    [ 0x0FBD, 0x0FBD ],
    [ 0x0FCD, 0x0FCD ],
    [ 0x0FDB, 0x0FFF ],
    [ 0x10C6, 0x10CF ],
    [ 0x10FD, 0x10FF ],
    [ 0x1249, 0x1249 ],
    [ 0x124E, 0x124F ],
    [ 0x1257, 0x1257 ],
    [ 0x1259, 0x1259 ],
    [ 0x125E, 0x125F ],
    [ 0x1289, 0x1289 ],
    [ 0x128E, 0x128F ],
    [ 0x12B1, 0x12B1 ],
    [ 0x12B6, 0x12B7 ],
    [ 0x12BF, 0x12BF ],
    [ 0x12C1, 0x12C1 ],
    [ 0x12C6, 0x12C7 ],
    [ 0x12D7, 0x12D7 ],
    [ 0x1311, 0x1311 ],
    [ 0x1316, 0x1317 ],
    [ 0x135B, 0x135C ],
    [ 0x137D, 0x137F ],
    [ 0x139A, 0x139F ],
    [ 0x13F5, 0x13FF ],
    [ 0x169D, 0x169F ],
    [ 0x16F1, 0x16FF ],
    [ 0x170D, 0x170D ],
    [ 0x1715, 0x171F ],
    [ 0x1737, 0x173F ],
    [ 0x1754, 0x175F ],
    [ 0x176D, 0x176D ],
    [ 0x1771, 0x1771 ],
    [ 0x1774, 0x177F ],
    [ 0x17DE, 0x17DF ],
    [ 0x17EA, 0x17EF ],
    [ 0x17FA, 0x17FF ],
    [ 0x180F, 0x180F ],
    [ 0x181A, 0x181F ],
    [ 0x1878, 0x187F ],
    [ 0x18AB, 0x18AF ],
    [ 0x18F6, 0x18FF ],
    [ 0x191D, 0x191F ],
    [ 0x192C, 0x192F ],
    [ 0x193C, 0x193F ],
    [ 0x1941, 0x1943 ],
    [ 0x196E, 0x196F ],
    [ 0x1975, 0x197F ],
    [ 0x19AC, 0x19AF ],
    [ 0x19CA, 0x19CF ],
    [ 0x19DB, 0x19DD ],
    [ 0x1A1C, 0x1A1D ],
    [ 0x1A5F, 0x1A5F ],
    [ 0x1A7D, 0x1A7E ],
    [ 0x1A8A, 0x1A8F ],
    [ 0x1A9A, 0x1A9F ],
    [ 0x1AAE, 0x1AFF ],
    [ 0x1B4C, 0x1B4F ],
    [ 0x1B7D, 0x1B7F ],
    [ 0x1BAB, 0x1BAD ],
    [ 0x1BBA, 0x1BBF ],
    [ 0x1BF4, 0x1BFB ],
    [ 0x1C38, 0x1C3A ],
    [ 0x1C4A, 0x1C4C ],
    [ 0x1C80, 0x1CCF ],
    [ 0x1CF3, 0x1CFF ],
    [ 0x1DE7, 0x1DFB ],
    [ 0x1F16, 0x1F17 ],
    [ 0x1F1E, 0x1F1F ],
    [ 0x1F46, 0x1F47 ],
    [ 0x1F4E, 0x1F4F ],
    [ 0x1F58, 0x1F58 ],
    [ 0x1F5A, 0x1F5A ],
    [ 0x1F5C, 0x1F5C ],
    [ 0x1F5E, 0x1F5E ],
    [ 0x1F7E, 0x1F7F ],
    [ 0x1FB5, 0x1FB5 ],
    [ 0x1FC5, 0x1FC5 ],
    [ 0x1FD4, 0x1FD5 ],
    [ 0x1FDC, 0x1FDC ],
    [ 0x1FF0, 0x1FF1 ],
    [ 0x1FF5, 0x1FF5 ],
    [ 0x1FFF, 0x1FFF ],
    [ 0x2065, 0x2069 ],
    [ 0x2072, 0x2073 ],
    [ 0x208F, 0x208F ],
    [ 0x209D, 0x209F ],
    [ 0x20BA, 0x20CF ],
    [ 0x20F1, 0x20FF ],
    [ 0x218A, 0x218F ],
    [ 0x23F4, 0x23FF ],
    [ 0x2427, 0x243F ],
    [ 0x244B, 0x245F ],
    [ 0x2700, 0x2700 ],
    [ 0x27CB, 0x27CB ],
    [ 0x27CD, 0x27CD ],
    [ 0x2B4D, 0x2B4F ],
    [ 0x2B5A, 0x2BFF ],
    [ 0x2C2F, 0x2C2F ],
    [ 0x2C5F, 0x2C5F ],
    [ 0x2CF2, 0x2CF8 ],
    [ 0x2D26, 0x2D2F ],
    [ 0x2D66, 0x2D6E ],
    [ 0x2D71, 0x2D7E ],
    [ 0x2D97, 0x2D9F ],
    [ 0x2DA7, 0x2DA7 ],
    [ 0x2DAF, 0x2DAF ],
    [ 0x2DB7, 0x2DB7 ],
    [ 0x2DBF, 0x2DBF ],
    [ 0x2DC7, 0x2DC7 ],
    [ 0x2DCF, 0x2DCF ],
    [ 0x2DD7, 0x2DD7 ],
    [ 0x2DDF, 0x2DDF ],
    [ 0x2E32, 0x2E7F ],
    [ 0x2E9A, 0x2E9A ],
    [ 0x2EF4, 0x2EFF ],
    [ 0x2FD6, 0x2FEF ],
    [ 0x2FFC, 0x2FFF ],
    [ 0x3040, 0x3040 ],
    [ 0x3097, 0x3098 ],
    [ 0x3100, 0x3104 ],
    [ 0x312E, 0x3130 ],
    [ 0x318F, 0x318F ],
    [ 0x31BB, 0x31BF ],
    [ 0x31E4, 0x31EF ],
    [ 0x321F, 0x321F ],
    [ 0x32FF, 0x32FF ],
    [ 0x4DB6, 0x4DBF ],
    [ 0x9FCC, 0x9FFF ],
    [ 0xA48D, 0xA48F ],
    [ 0xA4C7, 0xA4CF ],
    [ 0xA62C, 0xA63F ],
    [ 0xA674, 0xA67B ],
    [ 0xA698, 0xA69F ],
    [ 0xA6F8, 0xA6FF ],
    [ 0xA78F, 0xA78F ],
    [ 0xA792, 0xA79F ],
    [ 0xA7AA, 0xA7F9 ],
    [ 0xA82C, 0xA82F ],
    [ 0xA83A, 0xA83F ],
    [ 0xA878, 0xA87F ],
    [ 0xA8C5, 0xA8CD ],
    [ 0xA8DA, 0xA8DF ],
    [ 0xA8FC, 0xA8FF ],
    [ 0xA954, 0xA95E ],
    [ 0xA97D, 0xA97F ],
    [ 0xA9CE, 0xA9CE ],
    [ 0xA9DA, 0xA9DD ],
    [ 0xA9E0, 0xA9FF ],
    [ 0xAA37, 0xAA3F ],
    [ 0xAA4E, 0xAA4F ],
    [ 0xAA5A, 0xAA5B ],
    [ 0xAA7C, 0xAA7F ],
    [ 0xAAC3, 0xAADA ],
    [ 0xAAE0, 0xAB00 ],
    [ 0xAB07, 0xAB08 ],
    [ 0xAB0F, 0xAB10 ],
    [ 0xAB17, 0xAB1F ],
    [ 0xAB27, 0xAB27 ],
    [ 0xAB2F, 0xABBF ],
    [ 0xABEE, 0xABEF ],
    [ 0xABFA, 0xABFF ],
    [ 0xD7A4, 0xD7AF ],
    [ 0xD7C7, 0xD7CA ],
    [ 0xD7FC, 0xD7FF ],
    [ 0xFA2E, 0xFA2F ],
    [ 0xFA6E, 0xFA6F ],
    [ 0xFADA, 0xFAFF ],
    [ 0xFB07, 0xFB12 ],
    [ 0xFB18, 0xFB1C ],
    [ 0xFB37, 0xFB37 ],
    [ 0xFB3D, 0xFB3D ],
    [ 0xFB3F, 0xFB3F ],
    [ 0xFB42, 0xFB42 ],
    [ 0xFB45, 0xFB45 ],
    [ 0xFBC2, 0xFBD2 ],
    [ 0xFD40, 0xFD4F ],
    [ 0xFD90, 0xFD91 ],
    [ 0xFDC8, 0xFDEF ],
    [ 0xFDFE, 0xFDFF ],
    [ 0xFE1A, 0xFE1F ],
    [ 0xFE27, 0xFE2F ],
    [ 0xFE53, 0xFE53 ],
    [ 0xFE67, 0xFE67 ],
    [ 0xFE6C, 0xFE6F ],
    [ 0xFE75, 0xFE75 ],
    [ 0xFEFD, 0xFEFE ],
    [ 0xFF00, 0xFF00 ],
    [ 0xFFBF, 0xFFC1 ],
    [ 0xFFC8, 0xFFC9 ],
    [ 0xFFD0, 0xFFD1 ],
    [ 0xFFD8, 0xFFD9 ],
    [ 0xFFDD, 0xFFDF ],
    [ 0xFFE7, 0xFFE7 ],
    [ 0xFFEF, 0xFFF8 ],
    [ 0xFFFE, 0xFFFF ],
    [ 0x1000C, 0x1000C ],
    [ 0x10027, 0x10027 ],
    [ 0x1003B, 0x1003B ],
    [ 0x1003E, 0x1003E ],
    [ 0x1004E, 0x1004F ],
    [ 0x1005E, 0x1007F ],
    [ 0x100FB, 0x100FF ],
    [ 0x10103, 0x10106 ],
    [ 0x10134, 0x10136 ],
    [ 0x1018B, 0x1018F ],
    [ 0x1019C, 0x101CF ],
    [ 0x101FE, 0x1027F ],
    [ 0x1029D, 0x1029F ],
    [ 0x102D1, 0x102FF ],
    [ 0x1031F, 0x1031F ],
    [ 0x10324, 0x1032F ],
    [ 0x1034B, 0x1037F ],
    [ 0x1039E, 0x1039E ],
    [ 0x103C4, 0x103C7 ],
    [ 0x103D6, 0x103FF ],
    [ 0x1049E, 0x1049F ],
    [ 0x104AA, 0x107FF ],
    [ 0x10806, 0x10807 ],
    [ 0x10809, 0x10809 ],
    [ 0x10836, 0x10836 ],
    [ 0x10839, 0x1083B ],
    [ 0x1083D, 0x1083E ],
    [ 0x10856, 0x10856 ],
    [ 0x10860, 0x108FF ],
    [ 0x1091C, 0x1091E ],
    [ 0x1093A, 0x1093E ],
    [ 0x10940, 0x109FF ],
    [ 0x10A04, 0x10A04 ],
    [ 0x10A07, 0x10A0B ],
    [ 0x10A14, 0x10A14 ],
    [ 0x10A18, 0x10A18 ],
    [ 0x10A34, 0x10A37 ],
    [ 0x10A3B, 0x10A3E ],
    [ 0x10A48, 0x10A4F ],
    [ 0x10A59, 0x10A5F ],
    [ 0x10A80, 0x10AFF ],
    [ 0x10B36, 0x10B38 ],
    [ 0x10B56, 0x10B57 ],
    [ 0x10B73, 0x10B77 ],
    [ 0x10B80, 0x10BFF ],
    [ 0x10C49, 0x10E5F ],
    [ 0x10E7F, 0x10FFF ],
    [ 0x1104E, 0x11051 ],
    [ 0x11070, 0x1107F ],
    [ 0x110C2, 0x11FFF ],
    [ 0x1236F, 0x123FF ],
    [ 0x12463, 0x1246F ],
    [ 0x12474, 0x12FFF ],
    [ 0x1342F, 0x167FF ],
    [ 0x16A39, 0x1AFFF ],
    [ 0x1B002, 0x1CFFF ],
    [ 0x1D0F6, 0x1D0FF ],
    [ 0x1D127, 0x1D128 ],
    [ 0x1D1DE, 0x1D1FF ],
    [ 0x1D246, 0x1D2FF ],
    [ 0x1D357, 0x1D35F ],
    [ 0x1D372, 0x1D3FF ],
    [ 0x1D455, 0x1D455 ],
    [ 0x1D49D, 0x1D49D ],
    [ 0x1D4A0, 0x1D4A1 ],
    [ 0x1D4A3, 0x1D4A4 ],
    [ 0x1D4A7, 0x1D4A8 ],
    [ 0x1D4AD, 0x1D4AD ],
    [ 0x1D4BA, 0x1D4BA ],
    [ 0x1D4BC, 0x1D4BC ],
    [ 0x1D4C4, 0x1D4C4 ],
    [ 0x1D506, 0x1D506 ],
    [ 0x1D50B, 0x1D50C ],
    [ 0x1D515, 0x1D515 ],
    [ 0x1D51D, 0x1D51D ],
    [ 0x1D53A, 0x1D53A ],
    [ 0x1D53F, 0x1D53F ],
    [ 0x1D545, 0x1D545 ],
    [ 0x1D547, 0x1D549 ],
    [ 0x1D551, 0x1D551 ],
    [ 0x1D6A6, 0x1D6A7 ],
    [ 0x1D7CC, 0x1D7CD ],
    [ 0x1D800, 0x1EFFF ],
    [ 0x1F02C, 0x1F02F ],
    [ 0x1F094, 0x1F09F ],
    [ 0x1F0AF, 0x1F0B0 ],
    [ 0x1F0BF, 0x1F0C0 ],
    [ 0x1F0D0, 0x1F0D0 ],
    [ 0x1F0E0, 0x1F0FF ],
    [ 0x1F10B, 0x1F10F ],
    [ 0x1F12F, 0x1F12F ],
    [ 0x1F16A, 0x1F16F ],
    [ 0x1F19B, 0x1F1E5 ],
    [ 0x1F203, 0x1F20F ],
    [ 0x1F23B, 0x1F23F ],
    [ 0x1F249, 0x1F24F ],
    [ 0x1F252, 0x1F2FF ],
    [ 0x1F321, 0x1F32F ],
    [ 0x1F336, 0x1F336 ],
    [ 0x1F37D, 0x1F37F ],
    [ 0x1F394, 0x1F39F ],
    [ 0x1F3C5, 0x1F3C5 ],
    [ 0x1F3CB, 0x1F3DF ],
    [ 0x1F3F1, 0x1F3FF ],
    [ 0x1F43F, 0x1F43F ],
    [ 0x1F441, 0x1F441 ],
    [ 0x1F4F8, 0x1F4F8 ],
    [ 0x1F4FD, 0x1F4FF ],
    [ 0x1F53E, 0x1F54F ],
    [ 0x1F568, 0x1F5FA ],
    [ 0x1F600, 0x1F600 ],
    [ 0x1F611, 0x1F611 ],
    [ 0x1F615, 0x1F615 ],
    [ 0x1F617, 0x1F617 ],
    [ 0x1F619, 0x1F619 ],
    [ 0x1F61B, 0x1F61B ],
    [ 0x1F61F, 0x1F61F ],
    [ 0x1F626, 0x1F627 ],
    [ 0x1F62C, 0x1F62C ],
    [ 0x1F62E, 0x1F62F ],
    [ 0x1F634, 0x1F634 ],
    [ 0x1F641, 0x1F644 ],
    [ 0x1F650, 0x1F67F ],
    [ 0x1F6C6, 0x1F6FF ],
    [ 0x1F774, 0x1FFFF ],
    [ 0x2A6D7, 0x2A6FF ],
    [ 0x2B735, 0x2B73F ],
    [ 0x2B81E, 0x2F7FF ],
    [ 0x2FA1E, 0xE0000 ],
    [ 0xE0002, 0xE001F ],
    [ 0xE0080, 0xE00FF ],
    [ 0xE01F0, 0xEFFFF ],
    [ 0xFFFFE, 0xFFFFF ],
    [ 0x10FFFE, 0x10FFFF ],
    ];

    return binarySearch!table(c);
}

unittest
{
}


//==============================================================================
// Private Section.
//==============================================================================


bool binarySearch(alias table)(dchar c) @safe pure nothrow
{
    static @property bool checkTableEntry(alias table)()
    {
        foreach(i, entry; table)
        {
            assert(table[i][0] <= table[i][1]);
            if(i < table.length - 1)
                assert(table[i][1] < table[i + 1][0]);
        }
        return true;
    }
    static assert(checkTableEntry!table());

    return binarySearch2(c, table);
}

bool binarySearch2(dchar c, immutable dchar[2][] table) @safe pure nothrow
{
    // Binary search
    size_t mid;
    size_t low;
    size_t high;

    low = 0;
    high = table.length - 1;
    while(cast(int)low <= cast(int)high)
    {
        mid = (low + high) >> 1;

        if(c < table[mid][0])
            high = mid - 1;
        else if(c > table[mid][1])
            low = mid + 1;
        else
            goto Lis;
    }

Lisnot:
    debug
    {
        for(size_t i = 0; i < table.length; ++i)
            assert(c < table[i][0] || c > table[i][1]);
    }

    return false;

Lis:
    debug
    {
        for(size_t i = 0; i < table.length; ++i)
        {
            if(c >= table[i][0] && c <= table[i][1])
                return true;
        }

        assert(0);      // should have been in table
    }
    else
        return true;
};



//Written in the D programming language
/**
* Fast Regular expressions for D, internal tables
*
* License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
*
* Authors: Dmitry Olshansky
*
*/
//Automatically generated from Unicode Character Database files


struct CommonCaseEntry
{
    dchar start, end;
    uint op;
    @property uint delta() const { return op & 0xFF_FFFF; }
    @property uint xor()const {   return op & doXor; }
    @property uint neg()const {   return op & doMinus; }
    enum doXor = 1<<31, doMinus = 1<<30;
}


//sorted by .start
immutable commonCaseTable = [
	CommonCaseEntry(0x00041, 0x0005b, 2147483680),
	CommonCaseEntry(0x00061, 0x0006b, 2147483680),
	CommonCaseEntry(0x0006b, 0x0006c, 2147483680),
	CommonCaseEntry(0x0006b, 0x0006c, 8383),
	CommonCaseEntry(0x0006c, 0x00073, 2147483680),
	CommonCaseEntry(0x00073, 0x00074, 2147483680),
	CommonCaseEntry(0x00073, 0x00074, 2147483916),
	CommonCaseEntry(0x00074, 0x0007b, 2147483680),
	CommonCaseEntry(0x000b5, 0x000b6, 775),
	CommonCaseEntry(0x000c0, 0x000d7, 2147483680),
	CommonCaseEntry(0x000d8, 0x000df, 2147483680),
	CommonCaseEntry(0x000df, 0x000e0, 7615),
	CommonCaseEntry(0x000e0, 0x000e5, 2147483680),
	CommonCaseEntry(0x000e5, 0x000e6, 2147483680),
	CommonCaseEntry(0x000e5, 0x000e6, 8262),
	CommonCaseEntry(0x000e6, 0x000f7, 2147483680),
	CommonCaseEntry(0x000f8, 0x000ff, 2147483680),
	CommonCaseEntry(0x000ff, 0x00100, 121),
	CommonCaseEntry(0x00100, 0x00130, 2147483649),
	CommonCaseEntry(0x00132, 0x00138, 2147483649),
	CommonCaseEntry(0x00139, 0x00149, 2147483649),
	CommonCaseEntry(0x0014a, 0x00178, 2147483649),
	CommonCaseEntry(0x00178, 0x00179, 1073741945),
	CommonCaseEntry(0x00179, 0x0017f, 2147483649),
	CommonCaseEntry(0x0017f, 0x00180, 2147483916),
	CommonCaseEntry(0x00180, 0x00181, 195),
	CommonCaseEntry(0x00181, 0x00182, 210),
	CommonCaseEntry(0x00182, 0x00186, 2147483649),
	CommonCaseEntry(0x00186, 0x00187, 206),
	CommonCaseEntry(0x00187, 0x00189, 2147483649),
	CommonCaseEntry(0x00189, 0x0018b, 205),
	CommonCaseEntry(0x0018b, 0x0018d, 2147483649),
	CommonCaseEntry(0x0018e, 0x0018f, 79),
	CommonCaseEntry(0x0018f, 0x00190, 202),
	CommonCaseEntry(0x00190, 0x00191, 203),
	CommonCaseEntry(0x00191, 0x00193, 2147483649),
	CommonCaseEntry(0x00193, 0x00194, 205),
	CommonCaseEntry(0x00194, 0x00195, 207),
	CommonCaseEntry(0x00195, 0x00196, 97),
	CommonCaseEntry(0x00196, 0x00197, 211),
	CommonCaseEntry(0x00197, 0x00198, 209),
	CommonCaseEntry(0x00198, 0x0019a, 2147483649),
	CommonCaseEntry(0x0019a, 0x0019b, 163),
	CommonCaseEntry(0x0019c, 0x0019d, 211),
	CommonCaseEntry(0x0019d, 0x0019e, 213),
	CommonCaseEntry(0x0019e, 0x0019f, 130),
	CommonCaseEntry(0x0019f, 0x001a0, 214),
	CommonCaseEntry(0x001a0, 0x001a6, 2147483649),
	CommonCaseEntry(0x001a6, 0x001a7, 218),
	CommonCaseEntry(0x001a7, 0x001a9, 2147483649),
	CommonCaseEntry(0x001a9, 0x001aa, 218),
	CommonCaseEntry(0x001ac, 0x001ae, 2147483649),
	CommonCaseEntry(0x001ae, 0x001af, 218),
	CommonCaseEntry(0x001af, 0x001b1, 2147483649),
	CommonCaseEntry(0x001b1, 0x001b3, 217),
	CommonCaseEntry(0x001b3, 0x001b7, 2147483649),
	CommonCaseEntry(0x001b7, 0x001b8, 219),
	CommonCaseEntry(0x001b8, 0x001ba, 2147483649),
	CommonCaseEntry(0x001bc, 0x001be, 2147483649),
	CommonCaseEntry(0x001bf, 0x001c0, 56),
	CommonCaseEntry(0x001c4, 0x001c5, 2147483650),
	CommonCaseEntry(0x001c5, 0x001c6, 2147483649),
	CommonCaseEntry(0x001c6, 0x001c7, 2147483649),
	CommonCaseEntry(0x001c6, 0x001c7, 2147483650),
	CommonCaseEntry(0x001c7, 0x001c8, 2),
	CommonCaseEntry(0x001c8, 0x001c9, 2147483649),
	CommonCaseEntry(0x001c9, 0x001ca, 2147483649),
	CommonCaseEntry(0x001c9, 0x001ca, 1073741826),
	CommonCaseEntry(0x001ca, 0x001cb, 2),
	CommonCaseEntry(0x001cb, 0x001cc, 2147483649),
	CommonCaseEntry(0x001cc, 0x001cd, 2147483649),
	CommonCaseEntry(0x001cc, 0x001cd, 1073741826),
	CommonCaseEntry(0x001cd, 0x001dd, 2147483649),
	CommonCaseEntry(0x001dd, 0x001de, 1073741903),
	CommonCaseEntry(0x001de, 0x001f0, 2147483649),
	CommonCaseEntry(0x001f1, 0x001f2, 2147483650),
	CommonCaseEntry(0x001f2, 0x001f3, 2147483649),
	CommonCaseEntry(0x001f3, 0x001f4, 2147483649),
	CommonCaseEntry(0x001f3, 0x001f4, 2147483650),
	CommonCaseEntry(0x001f4, 0x001f6, 2147483649),
	CommonCaseEntry(0x001f6, 0x001f7, 1073741921),
	CommonCaseEntry(0x001f7, 0x001f8, 1073741880),
	CommonCaseEntry(0x001f8, 0x00220, 2147483649),
	CommonCaseEntry(0x00220, 0x00221, 1073741954),
	CommonCaseEntry(0x00222, 0x00234, 2147483649),
	CommonCaseEntry(0x0023a, 0x0023b, 10795),
	CommonCaseEntry(0x0023b, 0x0023d, 2147483649),
	CommonCaseEntry(0x0023d, 0x0023e, 1073741987),
	CommonCaseEntry(0x0023e, 0x0023f, 10792),
	CommonCaseEntry(0x0023f, 0x00241, 10815),
	CommonCaseEntry(0x00241, 0x00243, 2147483649),
	CommonCaseEntry(0x00243, 0x00244, 1073742019),
	CommonCaseEntry(0x00244, 0x00245, 69),
	CommonCaseEntry(0x00245, 0x00246, 71),
	CommonCaseEntry(0x00246, 0x00250, 2147483649),
	CommonCaseEntry(0x00250, 0x00251, 10783),
	CommonCaseEntry(0x00251, 0x00252, 10780),
	CommonCaseEntry(0x00252, 0x00253, 10782),
	CommonCaseEntry(0x00253, 0x00254, 1073742034),
	CommonCaseEntry(0x00254, 0x00255, 1073742030),
	CommonCaseEntry(0x00256, 0x00258, 1073742029),
	CommonCaseEntry(0x00259, 0x0025a, 1073742026),
	CommonCaseEntry(0x0025b, 0x0025c, 1073742027),
	CommonCaseEntry(0x00260, 0x00261, 1073742029),
	CommonCaseEntry(0x00263, 0x00264, 1073742031),
	CommonCaseEntry(0x00265, 0x00266, 42280),
	CommonCaseEntry(0x00268, 0x00269, 1073742033),
	CommonCaseEntry(0x00269, 0x0026a, 1073742035),
	CommonCaseEntry(0x0026b, 0x0026c, 10743),
	CommonCaseEntry(0x0026f, 0x00270, 1073742035),
	CommonCaseEntry(0x00271, 0x00272, 10749),
	CommonCaseEntry(0x00272, 0x00273, 1073742037),
	CommonCaseEntry(0x00275, 0x00276, 1073742038),
	CommonCaseEntry(0x0027d, 0x0027e, 10727),
	CommonCaseEntry(0x00280, 0x00281, 1073742042),
	CommonCaseEntry(0x00283, 0x00284, 1073742042),
	CommonCaseEntry(0x00288, 0x00289, 1073742042),
	CommonCaseEntry(0x00289, 0x0028a, 1073741893),
	CommonCaseEntry(0x0028a, 0x0028c, 1073742041),
	CommonCaseEntry(0x0028c, 0x0028d, 1073741895),
	CommonCaseEntry(0x00292, 0x00293, 1073742043),
	CommonCaseEntry(0x00345, 0x00346, 116),
	CommonCaseEntry(0x00370, 0x00374, 2147483649),
	CommonCaseEntry(0x00376, 0x00378, 2147483649),
	CommonCaseEntry(0x0037b, 0x0037e, 130),
	CommonCaseEntry(0x00386, 0x00387, 38),
	CommonCaseEntry(0x00388, 0x0038b, 37),
	CommonCaseEntry(0x0038c, 0x0038d, 2147483712),
	CommonCaseEntry(0x0038e, 0x00390, 63),
	CommonCaseEntry(0x00391, 0x003a2, 32),
	CommonCaseEntry(0x003a3, 0x003ac, 32),
	CommonCaseEntry(0x003ac, 0x003ad, 1073741862),
	CommonCaseEntry(0x003ad, 0x003b0, 1073741861),
	CommonCaseEntry(0x003b1, 0x003b2, 1073741856),
	CommonCaseEntry(0x003b2, 0x003b3, 1073741856),
	CommonCaseEntry(0x003b2, 0x003b3, 30),
	CommonCaseEntry(0x003b3, 0x003b5, 1073741856),
	CommonCaseEntry(0x003b5, 0x003b6, 1073741856),
	CommonCaseEntry(0x003b5, 0x003b6, 2147483712),
	CommonCaseEntry(0x003b6, 0x003b8, 1073741856),
	CommonCaseEntry(0x003b8, 0x003b9, 1073741856),
	CommonCaseEntry(0x003b8, 0x003b9, 25),
	CommonCaseEntry(0x003b8, 0x003b9, 60),
	CommonCaseEntry(0x003b9, 0x003ba, 1073741856),
	CommonCaseEntry(0x003b9, 0x003ba, 7173),
	CommonCaseEntry(0x003b9, 0x003ba, 1073741940),
	CommonCaseEntry(0x003ba, 0x003bb, 1073741856),
	CommonCaseEntry(0x003ba, 0x003bb, 54),
	CommonCaseEntry(0x003bb, 0x003bc, 1073741856),
	CommonCaseEntry(0x003bc, 0x003bd, 1073742599),
	CommonCaseEntry(0x003bc, 0x003bd, 1073741856),
	CommonCaseEntry(0x003bd, 0x003c0, 1073741856),
	CommonCaseEntry(0x003c0, 0x003c1, 1073741856),
	CommonCaseEntry(0x003c0, 0x003c1, 2147483670),
	CommonCaseEntry(0x003c1, 0x003c2, 1073741856),
	CommonCaseEntry(0x003c1, 0x003c2, 2147483696),
	CommonCaseEntry(0x003c2, 0x003c3, 2147483649),
	CommonCaseEntry(0x003c3, 0x003c4, 2147483649),
	CommonCaseEntry(0x003c3, 0x003c4, 1073741856),
	CommonCaseEntry(0x003c4, 0x003c6, 1073741856),
	CommonCaseEntry(0x003c6, 0x003c7, 1073741856),
	CommonCaseEntry(0x003c6, 0x003c7, 15),
	CommonCaseEntry(0x003c7, 0x003c9, 1073741856),
	CommonCaseEntry(0x003c9, 0x003ca, 1073741856),
	CommonCaseEntry(0x003c9, 0x003ca, 7517),
	CommonCaseEntry(0x003ca, 0x003cc, 1073741856),
	CommonCaseEntry(0x003cc, 0x003cd, 2147483712),
	CommonCaseEntry(0x003cd, 0x003cf, 1073741887),
	CommonCaseEntry(0x003cf, 0x003d0, 8),
	CommonCaseEntry(0x003d0, 0x003d1, 1073741854),
	CommonCaseEntry(0x003d1, 0x003d2, 1073741849),
	CommonCaseEntry(0x003d5, 0x003d6, 1073741839),
	CommonCaseEntry(0x003d6, 0x003d7, 2147483670),
	CommonCaseEntry(0x003d7, 0x003d8, 1073741832),
	CommonCaseEntry(0x003d8, 0x003f0, 2147483649),
	CommonCaseEntry(0x003f0, 0x003f1, 1073741878),
	CommonCaseEntry(0x003f1, 0x003f2, 2147483696),
	CommonCaseEntry(0x003f2, 0x003f3, 7),
	CommonCaseEntry(0x003f4, 0x003f5, 1073741884),
	CommonCaseEntry(0x003f5, 0x003f6, 2147483712),
	CommonCaseEntry(0x003f7, 0x003f9, 2147483649),
	CommonCaseEntry(0x003f9, 0x003fa, 1073741831),
	CommonCaseEntry(0x003fa, 0x003fc, 2147483649),
	CommonCaseEntry(0x003fd, 0x00400, 1073741954),
	CommonCaseEntry(0x00400, 0x00410, 2147483728),
	CommonCaseEntry(0x00410, 0x00430, 32),
	CommonCaseEntry(0x00430, 0x00450, 1073741856),
	CommonCaseEntry(0x00450, 0x00460, 2147483728),
	CommonCaseEntry(0x00460, 0x00482, 2147483649),
	CommonCaseEntry(0x0048a, 0x004c0, 2147483649),
	CommonCaseEntry(0x004c0, 0x004c1, 2147483663),
	CommonCaseEntry(0x004c1, 0x004cf, 2147483649),
	CommonCaseEntry(0x004cf, 0x004d0, 2147483663),
	CommonCaseEntry(0x004d0, 0x00528, 2147483649),
	CommonCaseEntry(0x00531, 0x00557, 48),
	CommonCaseEntry(0x00561, 0x00587, 1073741872),
	CommonCaseEntry(0x010a0, 0x010c6, 7264),
	CommonCaseEntry(0x01d79, 0x01d7a, 35332),
	CommonCaseEntry(0x01d7d, 0x01d7e, 3814),
	CommonCaseEntry(0x01e00, 0x01e61, 2147483649),
	CommonCaseEntry(0x01e61, 0x01e62, 2147483649),
	CommonCaseEntry(0x01e61, 0x01e62, 58),
	CommonCaseEntry(0x01e62, 0x01e96, 2147483649),
	CommonCaseEntry(0x01e9b, 0x01e9c, 1073741882),
	CommonCaseEntry(0x01e9e, 0x01e9f, 1073749439),
	CommonCaseEntry(0x01ea0, 0x01f00, 2147483649),
	CommonCaseEntry(0x01f00, 0x01f16, 2147483656),
	CommonCaseEntry(0x01f18, 0x01f1e, 2147483656),
	CommonCaseEntry(0x01f20, 0x01f46, 2147483656),
	CommonCaseEntry(0x01f48, 0x01f4e, 2147483656),
	CommonCaseEntry(0x01f51, 0x01f52, 2147483656),
	CommonCaseEntry(0x01f53, 0x01f54, 2147483656),
	CommonCaseEntry(0x01f55, 0x01f56, 2147483656),
	CommonCaseEntry(0x01f57, 0x01f58, 2147483656),
	CommonCaseEntry(0x01f59, 0x01f5a, 2147483656),
	CommonCaseEntry(0x01f5b, 0x01f5c, 2147483656),
	CommonCaseEntry(0x01f5d, 0x01f5e, 2147483656),
	CommonCaseEntry(0x01f5f, 0x01f70, 2147483656),
	CommonCaseEntry(0x01f70, 0x01f72, 74),
	CommonCaseEntry(0x01f72, 0x01f76, 86),
	CommonCaseEntry(0x01f76, 0x01f78, 100),
	CommonCaseEntry(0x01f78, 0x01f7a, 2147483776),
	CommonCaseEntry(0x01f7a, 0x01f7c, 112),
	CommonCaseEntry(0x01f7c, 0x01f7e, 126),
	CommonCaseEntry(0x01f80, 0x01fb2, 2147483656),
	CommonCaseEntry(0x01fb3, 0x01fb4, 9),
	CommonCaseEntry(0x01fb8, 0x01fba, 2147483656),
	CommonCaseEntry(0x01fba, 0x01fbc, 1073741898),
	CommonCaseEntry(0x01fbc, 0x01fbd, 1073741833),
	CommonCaseEntry(0x01fbe, 0x01fbf, 1073748997),
	CommonCaseEntry(0x01fc3, 0x01fc4, 9),
	CommonCaseEntry(0x01fc8, 0x01fcc, 1073741910),
	CommonCaseEntry(0x01fcc, 0x01fcd, 1073741833),
	CommonCaseEntry(0x01fd0, 0x01fd2, 2147483656),
	CommonCaseEntry(0x01fd8, 0x01fda, 2147483656),
	CommonCaseEntry(0x01fda, 0x01fdc, 1073741924),
	CommonCaseEntry(0x01fe0, 0x01fe2, 2147483656),
	CommonCaseEntry(0x01fe5, 0x01fe6, 7),
	CommonCaseEntry(0x01fe8, 0x01fea, 2147483656),
	CommonCaseEntry(0x01fea, 0x01fec, 1073741936),
	CommonCaseEntry(0x01fec, 0x01fed, 1073741831),
	CommonCaseEntry(0x01ff3, 0x01ff4, 9),
	CommonCaseEntry(0x01ff8, 0x01ffa, 2147483776),
	CommonCaseEntry(0x01ffa, 0x01ffc, 1073741950),
	CommonCaseEntry(0x01ffc, 0x01ffd, 1073741833),
	CommonCaseEntry(0x02126, 0x02127, 1073749341),
	CommonCaseEntry(0x0212a, 0x0212b, 1073750207),
	CommonCaseEntry(0x0212b, 0x0212c, 1073750086),
	CommonCaseEntry(0x02132, 0x02133, 28),
	CommonCaseEntry(0x0214e, 0x0214f, 1073741852),
	CommonCaseEntry(0x02160, 0x02180, 2147483664),
	CommonCaseEntry(0x02183, 0x02185, 2147483649),
	CommonCaseEntry(0x024b6, 0x024d0, 26),
	CommonCaseEntry(0x024d0, 0x024ea, 1073741850),
	CommonCaseEntry(0x02c00, 0x02c2f, 48),
	CommonCaseEntry(0x02c30, 0x02c5f, 1073741872),
	CommonCaseEntry(0x02c60, 0x02c62, 2147483649),
	CommonCaseEntry(0x02c62, 0x02c63, 1073752567),
	CommonCaseEntry(0x02c63, 0x02c64, 1073745638),
	CommonCaseEntry(0x02c64, 0x02c65, 1073752551),
	CommonCaseEntry(0x02c65, 0x02c66, 1073752619),
	CommonCaseEntry(0x02c66, 0x02c67, 1073752616),
	CommonCaseEntry(0x02c67, 0x02c6d, 2147483649),
	CommonCaseEntry(0x02c6d, 0x02c6e, 1073752604),
	CommonCaseEntry(0x02c6e, 0x02c6f, 1073752573),
	CommonCaseEntry(0x02c6f, 0x02c70, 1073752607),
	CommonCaseEntry(0x02c70, 0x02c71, 1073752606),
	CommonCaseEntry(0x02c72, 0x02c74, 2147483649),
	CommonCaseEntry(0x02c75, 0x02c77, 2147483649),
	CommonCaseEntry(0x02c7e, 0x02c80, 1073752639),
	CommonCaseEntry(0x02c80, 0x02ce4, 2147483649),
	CommonCaseEntry(0x02ceb, 0x02cef, 2147483649),
	CommonCaseEntry(0x02d00, 0x02d26, 1073749088),
	CommonCaseEntry(0x0a640, 0x0a66e, 2147483649),
	CommonCaseEntry(0x0a680, 0x0a698, 2147483649),
	CommonCaseEntry(0x0a722, 0x0a730, 2147483649),
	CommonCaseEntry(0x0a732, 0x0a770, 2147483649),
	CommonCaseEntry(0x0a779, 0x0a77d, 2147483649),
	CommonCaseEntry(0x0a77d, 0x0a77e, 1073777156),
	CommonCaseEntry(0x0a77e, 0x0a788, 2147483649),
	CommonCaseEntry(0x0a78b, 0x0a78d, 2147483649),
	CommonCaseEntry(0x0a78d, 0x0a78e, 1073784104),
	CommonCaseEntry(0x0a790, 0x0a792, 2147483649),
	CommonCaseEntry(0x0a7a0, 0x0a7aa, 2147483649),
	CommonCaseEntry(0x0ff21, 0x0ff3b, 32),
	CommonCaseEntry(0x0ff41, 0x0ff5b, 1073741856),
	CommonCaseEntry(0x10400, 0x10428, 40),
	CommonCaseEntry(0x10428, 0x10450, 1073741864),
];

struct UnicodeProperty
{
    string name;
    RleBitSet!uint set;
}

immutable(RleBitSet!uint) unicodeInCombining_Diacritical_Marks_Supplement = RleBitSet!uint([
    0x01dc0, 0x01e00,
]);

immutable(RleBitSet!uint) unicodeInCJK_Radicals_Supplement = RleBitSet!uint([
    0x02e80, 0x02f00,
]);

immutable(RleBitSet!uint) unicodeInEthiopic_Supplement = RleBitSet!uint([
    0x01380, 0x013a0,
]);

immutable(RleBitSet!uint) unicodeInPhags_pa = RleBitSet!uint([
    0x0a840, 0x0a880,
]);

immutable(RleBitSet!uint) unicodeInSyriac = RleBitSet!uint([
    0x00700, 0x00750,
]);

immutable(RleBitSet!uint) unicodeInGujarati = RleBitSet!uint([
    0x00a80, 0x00b00,
]);

immutable(RleBitSet!uint) unicodeOther_ID_Start = RleBitSet!uint([
    0x02118, 0x02119,
    0x0212e, 0x0212f,
    0x0309b, 0x0309d,
]);

immutable(RleBitSet!uint) unicodeInKharoshthi = RleBitSet!uint([
    0x10a00, 0x10a60,
]);

immutable(RleBitSet!uint) unicodeInHangul_Jamo = RleBitSet!uint([
    0x01100, 0x01200,
]);

immutable(RleBitSet!uint) unicodeRunic = RleBitSet!uint([
    0x016a0, 0x016eb,
    0x016ee, 0x016f1,
]);

immutable(RleBitSet!uint) unicodeInKayah_Li = RleBitSet!uint([
    0x0a900, 0x0a930,
]);

immutable(RleBitSet!uint) unicodeOther_Default_Ignorable_Code_Point = RleBitSet!uint([
    0x0034f, 0x00350,
    0x0115f, 0x01161,
    0x02065, 0x0206a,
    0x03164, 0x03165,
    0x0ffa0, 0x0ffa1,
    0x0fff0, 0x0fff9,
    0xe0000, 0xe0001,
    0xe0002, 0xe0020,
    0xe0080, 0xe0100,
    0xe01f0, 0xe1000,
]);

immutable(RleBitSet!uint) unicodeOld_Turkic = RleBitSet!uint([
    0x10c00, 0x10c49,
]);

immutable(RleBitSet!uint) unicodeInLydian = RleBitSet!uint([
    0x10920, 0x10940,
]);

immutable(RleBitSet!uint) unicodeIDS_Binary_Operator = RleBitSet!uint([
    0x02ff0, 0x02ff2,
    0x02ff4, 0x02ffc,
]);

immutable(RleBitSet!uint) unicodeTai_Tham = RleBitSet!uint([
    0x01a20, 0x01a5f,
    0x01a60, 0x01a7d,
    0x01a7f, 0x01a8a,
    0x01a90, 0x01a9a,
    0x01aa0, 0x01aae,
]);

immutable(RleBitSet!uint) unicodeLinear_B = RleBitSet!uint([
    0x10000, 0x1000c,
    0x1000d, 0x10027,
    0x10028, 0x1003b,
    0x1003c, 0x1003e,
    0x1003f, 0x1004e,
    0x10050, 0x1005e,
    0x10080, 0x100fb,
]);

immutable(RleBitSet!uint) unicodeInSupplemental_Arrows_A = RleBitSet!uint([
    0x027f0, 0x02800,
]);

immutable(RleBitSet!uint) unicodeInMathematical_Alphanumeric_Symbols = RleBitSet!uint([
    0x1d400, 0x1d800,
]);

immutable(RleBitSet!uint) unicodeArmenian = RleBitSet!uint([
    0x00531, 0x00557,
    0x00559, 0x00560,
    0x00561, 0x00588,
    0x0058a, 0x0058b,
    0x0fb13, 0x0fb18,
]);

immutable(RleBitSet!uint) unicodeInBlock_Elements = RleBitSet!uint([
    0x02580, 0x025a0,
]);

immutable(RleBitSet!uint) unicodeInSupplemental_Arrows_B = RleBitSet!uint([
    0x02900, 0x02980,
]);

immutable(RleBitSet!uint) unicodeInBalinese = RleBitSet!uint([
    0x01b00, 0x01b80,
]);

immutable(RleBitSet!uint) unicodeCyrillic = RleBitSet!uint([
    0x00400, 0x00485,
    0x00487, 0x00528,
    0x01d2b, 0x01d2c,
    0x01d78, 0x01d79,
    0x02de0, 0x02e00,
    0x0a640, 0x0a674,
    0x0a67c, 0x0a698,
]);

immutable(RleBitSet!uint) unicodeInBamum_Supplement = RleBitSet!uint([
    0x16800, 0x16a40,
]);

immutable unicodeWhite_Space = RleBitSet!uint([
    0x00009, 0x0000e,
    0x00020, 0x00021,
    0x00085, 0x00086,
    0x000a0, 0x000a1,
    0x01680, 0x01681,
    0x0180e, 0x0180f,
    0x02000, 0x0200b,
    0x02028, 0x0202a,
    0x0202f, 0x02030,
    0x0205f, 0x02060,
    0x03000, 0x03001,
]);

immutable(RleBitSet!uint) unicodeBatak = RleBitSet!uint([
    0x01bc0, 0x01bf4,
    0x01bfc, 0x01c00,
]);

immutable(RleBitSet!uint) unicodeDeprecated = RleBitSet!uint([
    0x00149, 0x0014a,
    0x00673, 0x00674,
    0x00f77, 0x00f78,
    0x00f79, 0x00f7a,
    0x017a3, 0x017a5,
    0x0206a, 0x02070,
    0x02329, 0x0232b,
    0xe0001, 0xe0002,
    0xe0020, 0xe0080,
]);

immutable(RleBitSet!uint) unicodeInUnified_Canadian_Aboriginal_Syllabics_Extended = RleBitSet!uint([
    0x018b0, 0x01900,
]);

immutable(RleBitSet!uint) unicodeCherokee = RleBitSet!uint([
    0x013a0, 0x013f5,
]);

immutable(RleBitSet!uint) unicodeInInscriptional_Parthian = RleBitSet!uint([
    0x10b40, 0x10b60,
]);

immutable(RleBitSet!uint) unicodeHiragana = RleBitSet!uint([
    0x03041, 0x03097,
    0x0309d, 0x030a0,
    0x1b001, 0x1b002,
    0x1f200, 0x1f201,
]);

immutable(RleBitSet!uint) unicodeInCJK_Compatibility_Ideographs = RleBitSet!uint([
    0x0f900, 0x0fb00,
]);

immutable(RleBitSet!uint) unicodeInAncient_Symbols = RleBitSet!uint([
    0x10190, 0x101d0,
]);

immutable(RleBitSet!uint) unicodeInOld_South_Arabian = RleBitSet!uint([
    0x10a60, 0x10a80,
]);

immutable(RleBitSet!uint) unicodePattern_Syntax = RleBitSet!uint([
    0x00021, 0x00030,
    0x0003a, 0x00041,
    0x0005b, 0x0005f,
    0x00060, 0x00061,
    0x0007b, 0x0007f,
    0x000a1, 0x000a8,
    0x000a9, 0x000aa,
    0x000ab, 0x000ad,
    0x000ae, 0x000af,
    0x000b0, 0x000b2,
    0x000b6, 0x000b7,
    0x000bb, 0x000bc,
    0x000bf, 0x000c0,
    0x000d7, 0x000d8,
    0x000f7, 0x000f8,
    0x02010, 0x02028,
    0x02030, 0x0203f,
    0x02041, 0x02054,
    0x02055, 0x0205f,
    0x02190, 0x02460,
    0x02500, 0x02776,
    0x02794, 0x02c00,
    0x02e00, 0x02e80,
    0x03001, 0x03004,
    0x03008, 0x03021,
    0x03030, 0x03031,
    0x0fd3e, 0x0fd40,
    0x0fe45, 0x0fe47,
]);

immutable(RleBitSet!uint) unicodeNew_Tai_Lue = RleBitSet!uint([
    0x01980, 0x019ac,
    0x019b0, 0x019ca,
    0x019d0, 0x019db,
    0x019de, 0x019e0,
]);

immutable(RleBitSet!uint) unicodeASCII_Hex_Digit = RleBitSet!uint([
    0x00030, 0x0003a,
    0x00041, 0x00047,
    0x00061, 0x00067,
]);

immutable(RleBitSet!uint) unicodeArabic = RleBitSet!uint([
    0x00600, 0x00604,
    0x00606, 0x0060c,
    0x0060d, 0x0061b,
    0x0061e, 0x0061f,
    0x00620, 0x00640,
    0x00641, 0x0064b,
    0x00656, 0x0065f,
    0x0066a, 0x00670,
    0x00671, 0x006dd,
    0x006de, 0x00700,
    0x00750, 0x00780,
    0x0fb50, 0x0fbc2,
    0x0fbd3, 0x0fd3e,
    0x0fd50, 0x0fd90,
    0x0fd92, 0x0fdc8,
    0x0fdf0, 0x0fdfd,
    0x0fe70, 0x0fe75,
    0x0fe76, 0x0fefd,
    0x10e60, 0x10e7f,
]);

immutable(RleBitSet!uint) unicodeInCuneiform_Numbers_and_Punctuation = RleBitSet!uint([
    0x12400, 0x12480,
]);

immutable(RleBitSet!uint) unicodeBrahmi = RleBitSet!uint([
    0x11000, 0x1104e,
    0x11052, 0x11070,
]);

immutable(RleBitSet!uint) unicodeInControl_Pictures = RleBitSet!uint([
    0x02400, 0x02440,
]);

immutable(RleBitSet!uint) unicodeOther_ID_Continue = RleBitSet!uint([
    0x000b7, 0x000b8,
    0x00387, 0x00388,
    0x01369, 0x01372,
    0x019da, 0x019db,
]);

immutable(RleBitSet!uint) unicodeInOl_Chiki = RleBitSet!uint([
    0x01c50, 0x01c80,
]);

immutable(RleBitSet!uint) unicodeInArmenian = RleBitSet!uint([
    0x00530, 0x00590,
]);

immutable(RleBitSet!uint) unicodeInCyrillic = RleBitSet!uint([
    0x00400, 0x00500,
]);

immutable(RleBitSet!uint) unicodeInNew_Tai_Lue = RleBitSet!uint([
    0x01980, 0x019e0,
]);

immutable(RleBitSet!uint) unicodeInHigh_Surrogates = RleBitSet!uint([
    0x0d800, 0x0db80,
]);

immutable(RleBitSet!uint) unicodeInHigh_Private_Use_Surrogates = RleBitSet!uint([
    0x0db80, 0x0dc00,
]);

immutable(RleBitSet!uint) unicodeInAncient_Greek_Numbers = RleBitSet!uint([
    0x10140, 0x10190,
]);

immutable(RleBitSet!uint) unicodeQuotation_Mark = RleBitSet!uint([
    0x00022, 0x00023,
    0x00027, 0x00028,
    0x000ab, 0x000ac,
    0x000bb, 0x000bc,
    0x02018, 0x02020,
    0x02039, 0x0203b,
    0x0300c, 0x03010,
    0x0301d, 0x03020,
    0x0fe41, 0x0fe45,
    0x0ff02, 0x0ff03,
    0x0ff07, 0x0ff08,
    0x0ff62, 0x0ff64,
]);

immutable(RleBitSet!uint) unicodeTai_Le = RleBitSet!uint([
    0x01950, 0x0196e,
    0x01970, 0x01975,
]);

immutable(RleBitSet!uint) unicodeCc = RleBitSet!uint([
    0x00000, 0x00020,
    0x0007f, 0x000a0,
]);

immutable(RleBitSet!uint) unicodeInCherokee = RleBitSet!uint([
    0x013a0, 0x01400,
]);

immutable(RleBitSet!uint) unicodeCoptic = RleBitSet!uint([
    0x003e2, 0x003f0,
    0x02c80, 0x02cf2,
    0x02cf9, 0x02d00,
]);

immutable(RleBitSet!uint) unicodeInCyrillic_Supplement = RleBitSet!uint([
    0x00500, 0x00530,
]);

immutable(RleBitSet!uint) unicodeCf = RleBitSet!uint([
    0x000ad, 0x000ae,
    0x00600, 0x00604,
    0x006dd, 0x006de,
    0x0070f, 0x00710,
    0x017b4, 0x017b6,
    0x0200b, 0x02010,
    0x0202a, 0x0202f,
    0x02060, 0x02065,
    0x0206a, 0x02070,
    0x0feff, 0x0ff00,
    0x0fff9, 0x0fffc,
    0x110bd, 0x110be,
    0x1d173, 0x1d17b,
    0xe0001, 0xe0002,
    0xe0020, 0xe0080,
]);

immutable(RleBitSet!uint) unicodeGothic = RleBitSet!uint([
    0x10330, 0x1034b,
]);

immutable(RleBitSet!uint) unicodeInHiragana = RleBitSet!uint([
    0x03040, 0x030a0,
]);

immutable(RleBitSet!uint) unicodeInIPA_Extensions = RleBitSet!uint([
    0x00250, 0x002b0,
]);

immutable(RleBitSet!uint) unicodeOgham = RleBitSet!uint([
    0x01680, 0x0169d,
]);

immutable(RleBitSet!uint) unicodeInCJK_Compatibility_Forms = RleBitSet!uint([
    0x0fe30, 0x0fe50,
]);

immutable(RleBitSet!uint) unicodeInShavian = RleBitSet!uint([
    0x10450, 0x10480,
]);

immutable(RleBitSet!uint) unicodeCn = RleBitSet!uint([
    0x00378, 0x0037a,
    0x0037f, 0x00384,
    0x0038b, 0x0038c,
    0x0038d, 0x0038e,
    0x003a2, 0x003a3,
    0x00528, 0x00531,
    0x00557, 0x00559,
    0x00560, 0x00561,
    0x00588, 0x00589,
    0x0058b, 0x00591,
    0x005c8, 0x005d0,
    0x005eb, 0x005f0,
    0x005f5, 0x00600,
    0x00604, 0x00606,
    0x0061c, 0x0061e,
    0x0070e, 0x0070f,
    0x0074b, 0x0074d,
    0x007b2, 0x007c0,
    0x007fb, 0x00800,
    0x0082e, 0x00830,
    0x0083f, 0x00840,
    0x0085c, 0x0085e,
    0x0085f, 0x00900,
    0x00978, 0x00979,
    0x00980, 0x00981,
    0x00984, 0x00985,
    0x0098d, 0x0098f,
    0x00991, 0x00993,
    0x009a9, 0x009aa,
    0x009b1, 0x009b2,
    0x009b3, 0x009b6,
    0x009ba, 0x009bc,
    0x009c5, 0x009c7,
    0x009c9, 0x009cb,
    0x009cf, 0x009d7,
    0x009d8, 0x009dc,
    0x009de, 0x009df,
    0x009e4, 0x009e6,
    0x009fc, 0x00a01,
    0x00a04, 0x00a05,
    0x00a0b, 0x00a0f,
    0x00a11, 0x00a13,
    0x00a29, 0x00a2a,
    0x00a31, 0x00a32,
    0x00a34, 0x00a35,
    0x00a37, 0x00a38,
    0x00a3a, 0x00a3c,
    0x00a3d, 0x00a3e,
    0x00a43, 0x00a47,
    0x00a49, 0x00a4b,
    0x00a4e, 0x00a51,
    0x00a52, 0x00a59,
    0x00a5d, 0x00a5e,
    0x00a5f, 0x00a66,
    0x00a76, 0x00a81,
    0x00a84, 0x00a85,
    0x00a8e, 0x00a8f,
    0x00a92, 0x00a93,
    0x00aa9, 0x00aaa,
    0x00ab1, 0x00ab2,
    0x00ab4, 0x00ab5,
    0x00aba, 0x00abc,
    0x00ac6, 0x00ac7,
    0x00aca, 0x00acb,
    0x00ace, 0x00ad0,
    0x00ad1, 0x00ae0,
    0x00ae4, 0x00ae6,
    0x00af0, 0x00af1,
    0x00af2, 0x00b01,
    0x00b04, 0x00b05,
    0x00b0d, 0x00b0f,
    0x00b11, 0x00b13,
    0x00b29, 0x00b2a,
    0x00b31, 0x00b32,
    0x00b34, 0x00b35,
    0x00b3a, 0x00b3c,
    0x00b45, 0x00b47,
    0x00b49, 0x00b4b,
    0x00b4e, 0x00b56,
    0x00b58, 0x00b5c,
    0x00b5e, 0x00b5f,
    0x00b64, 0x00b66,
    0x00b78, 0x00b82,
    0x00b84, 0x00b85,
    0x00b8b, 0x00b8e,
    0x00b91, 0x00b92,
    0x00b96, 0x00b99,
    0x00b9b, 0x00b9c,
    0x00b9d, 0x00b9e,
    0x00ba0, 0x00ba3,
    0x00ba5, 0x00ba8,
    0x00bab, 0x00bae,
    0x00bba, 0x00bbe,
    0x00bc3, 0x00bc6,
    0x00bc9, 0x00bca,
    0x00bce, 0x00bd0,
    0x00bd1, 0x00bd7,
    0x00bd8, 0x00be6,
    0x00bfb, 0x00c01,
    0x00c04, 0x00c05,
    0x00c0d, 0x00c0e,
    0x00c11, 0x00c12,
    0x00c29, 0x00c2a,
    0x00c34, 0x00c35,
    0x00c3a, 0x00c3d,
    0x00c45, 0x00c46,
    0x00c49, 0x00c4a,
    0x00c4e, 0x00c55,
    0x00c57, 0x00c58,
    0x00c5a, 0x00c60,
    0x00c64, 0x00c66,
    0x00c70, 0x00c78,
    0x00c80, 0x00c82,
    0x00c84, 0x00c85,
    0x00c8d, 0x00c8e,
    0x00c91, 0x00c92,
    0x00ca9, 0x00caa,
    0x00cb4, 0x00cb5,
    0x00cba, 0x00cbc,
    0x00cc5, 0x00cc6,
    0x00cc9, 0x00cca,
    0x00cce, 0x00cd5,
    0x00cd7, 0x00cde,
    0x00cdf, 0x00ce0,
    0x00ce4, 0x00ce6,
    0x00cf0, 0x00cf1,
    0x00cf3, 0x00d02,
    0x00d04, 0x00d05,
    0x00d0d, 0x00d0e,
    0x00d11, 0x00d12,
    0x00d3b, 0x00d3d,
    0x00d45, 0x00d46,
    0x00d49, 0x00d4a,
    0x00d4f, 0x00d57,
    0x00d58, 0x00d60,
    0x00d64, 0x00d66,
    0x00d76, 0x00d79,
    0x00d80, 0x00d82,
    0x00d84, 0x00d85,
    0x00d97, 0x00d9a,
    0x00db2, 0x00db3,
    0x00dbc, 0x00dbd,
    0x00dbe, 0x00dc0,
    0x00dc7, 0x00dca,
    0x00dcb, 0x00dcf,
    0x00dd5, 0x00dd6,
    0x00dd7, 0x00dd8,
    0x00de0, 0x00df2,
    0x00df5, 0x00e01,
    0x00e3b, 0x00e3f,
    0x00e5c, 0x00e81,
    0x00e83, 0x00e84,
    0x00e85, 0x00e87,
    0x00e89, 0x00e8a,
    0x00e8b, 0x00e8d,
    0x00e8e, 0x00e94,
    0x00e98, 0x00e99,
    0x00ea0, 0x00ea1,
    0x00ea4, 0x00ea5,
    0x00ea6, 0x00ea7,
    0x00ea8, 0x00eaa,
    0x00eac, 0x00ead,
    0x00eba, 0x00ebb,
    0x00ebe, 0x00ec0,
    0x00ec5, 0x00ec6,
    0x00ec7, 0x00ec8,
    0x00ece, 0x00ed0,
    0x00eda, 0x00edc,
    0x00ede, 0x00f00,
    0x00f48, 0x00f49,
    0x00f6d, 0x00f71,
    0x00f98, 0x00f99,
    0x00fbd, 0x00fbe,
    0x00fcd, 0x00fce,
    0x00fdb, 0x01000,
    0x010c6, 0x010d0,
    0x010fd, 0x01100,
    0x01249, 0x0124a,
    0x0124e, 0x01250,
    0x01257, 0x01258,
    0x01259, 0x0125a,
    0x0125e, 0x01260,
    0x01289, 0x0128a,
    0x0128e, 0x01290,
    0x012b1, 0x012b2,
    0x012b6, 0x012b8,
    0x012bf, 0x012c0,
    0x012c1, 0x012c2,
    0x012c6, 0x012c8,
    0x012d7, 0x012d8,
    0x01311, 0x01312,
    0x01316, 0x01318,
    0x0135b, 0x0135d,
    0x0137d, 0x01380,
    0x0139a, 0x013a0,
    0x013f5, 0x01400,
    0x0169d, 0x016a0,
    0x016f1, 0x01700,
    0x0170d, 0x0170e,
    0x01715, 0x01720,
    0x01737, 0x01740,
    0x01754, 0x01760,
    0x0176d, 0x0176e,
    0x01771, 0x01772,
    0x01774, 0x01780,
    0x017de, 0x017e0,
    0x017ea, 0x017f0,
    0x017fa, 0x01800,
    0x0180f, 0x01810,
    0x0181a, 0x01820,
    0x01878, 0x01880,
    0x018ab, 0x018b0,
    0x018f6, 0x01900,
    0x0191d, 0x01920,
    0x0192c, 0x01930,
    0x0193c, 0x01940,
    0x01941, 0x01944,
    0x0196e, 0x01970,
    0x01975, 0x01980,
    0x019ac, 0x019b0,
    0x019ca, 0x019d0,
    0x019db, 0x019de,
    0x01a1c, 0x01a1e,
    0x01a5f, 0x01a60,
    0x01a7d, 0x01a7f,
    0x01a8a, 0x01a90,
    0x01a9a, 0x01aa0,
    0x01aae, 0x01b00,
    0x01b4c, 0x01b50,
    0x01b7d, 0x01b80,
    0x01bab, 0x01bae,
    0x01bba, 0x01bc0,
    0x01bf4, 0x01bfc,
    0x01c38, 0x01c3b,
    0x01c4a, 0x01c4d,
    0x01c80, 0x01cd0,
    0x01cf3, 0x01d00,
    0x01de7, 0x01dfc,
    0x01f16, 0x01f18,
    0x01f1e, 0x01f20,
    0x01f46, 0x01f48,
    0x01f4e, 0x01f50,
    0x01f58, 0x01f59,
    0x01f5a, 0x01f5b,
    0x01f5c, 0x01f5d,
    0x01f5e, 0x01f5f,
    0x01f7e, 0x01f80,
    0x01fb5, 0x01fb6,
    0x01fc5, 0x01fc6,
    0x01fd4, 0x01fd6,
    0x01fdc, 0x01fdd,
    0x01ff0, 0x01ff2,
    0x01ff5, 0x01ff6,
    0x01fff, 0x02000,
    0x02065, 0x0206a,
    0x02072, 0x02074,
    0x0208f, 0x02090,
    0x0209d, 0x020a0,
    0x020ba, 0x020d0,
    0x020f1, 0x02100,
    0x0218a, 0x02190,
    0x023f4, 0x02400,
    0x02427, 0x02440,
    0x0244b, 0x02460,
    0x02700, 0x02701,
    0x027cb, 0x027cc,
    0x027cd, 0x027ce,
    0x02b4d, 0x02b50,
    0x02b5a, 0x02c00,
    0x02c2f, 0x02c30,
    0x02c5f, 0x02c60,
    0x02cf2, 0x02cf9,
    0x02d26, 0x02d30,
    0x02d66, 0x02d6f,
    0x02d71, 0x02d7f,
    0x02d97, 0x02da0,
    0x02da7, 0x02da8,
    0x02daf, 0x02db0,
    0x02db7, 0x02db8,
    0x02dbf, 0x02dc0,
    0x02dc7, 0x02dc8,
    0x02dcf, 0x02dd0,
    0x02dd7, 0x02dd8,
    0x02ddf, 0x02de0,
    0x02e32, 0x02e80,
    0x02e9a, 0x02e9b,
    0x02ef4, 0x02f00,
    0x02fd6, 0x02ff0,
    0x02ffc, 0x03000,
    0x03040, 0x03041,
    0x03097, 0x03099,
    0x03100, 0x03105,
    0x0312e, 0x03131,
    0x0318f, 0x03190,
    0x031bb, 0x031c0,
    0x031e4, 0x031f0,
    0x0321f, 0x03220,
    0x032ff, 0x03300,
    0x04db6, 0x04dc0,
    0x09fcc, 0x0a000,
    0x0a48d, 0x0a490,
    0x0a4c7, 0x0a4d0,
    0x0a62c, 0x0a640,
    0x0a674, 0x0a67c,
    0x0a698, 0x0a6a0,
    0x0a6f8, 0x0a700,
    0x0a78f, 0x0a790,
    0x0a792, 0x0a7a0,
    0x0a7aa, 0x0a7fa,
    0x0a82c, 0x0a830,
    0x0a83a, 0x0a840,
    0x0a878, 0x0a880,
    0x0a8c5, 0x0a8ce,
    0x0a8da, 0x0a8e0,
    0x0a8fc, 0x0a900,
    0x0a954, 0x0a95f,
    0x0a97d, 0x0a980,
    0x0a9ce, 0x0a9cf,
    0x0a9da, 0x0a9de,
    0x0a9e0, 0x0aa00,
    0x0aa37, 0x0aa40,
    0x0aa4e, 0x0aa50,
    0x0aa5a, 0x0aa5c,
    0x0aa7c, 0x0aa80,
    0x0aac3, 0x0aadb,
    0x0aae0, 0x0ab01,
    0x0ab07, 0x0ab09,
    0x0ab0f, 0x0ab11,
    0x0ab17, 0x0ab20,
    0x0ab27, 0x0ab28,
    0x0ab2f, 0x0abc0,
    0x0abee, 0x0abf0,
    0x0abfa, 0x0ac00,
    0x0d7a4, 0x0d7b0,
    0x0d7c7, 0x0d7cb,
    0x0d7fc, 0x0d800,
    0x0fa2e, 0x0fa30,
    0x0fa6e, 0x0fa70,
    0x0fada, 0x0fb00,
    0x0fb07, 0x0fb13,
    0x0fb18, 0x0fb1d,
    0x0fb37, 0x0fb38,
    0x0fb3d, 0x0fb3e,
    0x0fb3f, 0x0fb40,
    0x0fb42, 0x0fb43,
    0x0fb45, 0x0fb46,
    0x0fbc2, 0x0fbd3,
    0x0fd40, 0x0fd50,
    0x0fd90, 0x0fd92,
    0x0fdc8, 0x0fdf0,
    0x0fdfe, 0x0fe00,
    0x0fe1a, 0x0fe20,
    0x0fe27, 0x0fe30,
    0x0fe53, 0x0fe54,
    0x0fe67, 0x0fe68,
    0x0fe6c, 0x0fe70,
    0x0fe75, 0x0fe76,
    0x0fefd, 0x0feff,
    0x0ff00, 0x0ff01,
    0x0ffbf, 0x0ffc2,
    0x0ffc8, 0x0ffca,
    0x0ffd0, 0x0ffd2,
    0x0ffd8, 0x0ffda,
    0x0ffdd, 0x0ffe0,
    0x0ffe7, 0x0ffe8,
    0x0ffef, 0x0fff9,
    0x0fffe, 0x10000,
    0x1000c, 0x1000d,
    0x10027, 0x10028,
    0x1003b, 0x1003c,
    0x1003e, 0x1003f,
    0x1004e, 0x10050,
    0x1005e, 0x10080,
    0x100fb, 0x10100,
    0x10103, 0x10107,
    0x10134, 0x10137,
    0x1018b, 0x10190,
    0x1019c, 0x101d0,
    0x101fe, 0x10280,
    0x1029d, 0x102a0,
    0x102d1, 0x10300,
    0x1031f, 0x10320,
    0x10324, 0x10330,
    0x1034b, 0x10380,
    0x1039e, 0x1039f,
    0x103c4, 0x103c8,
    0x103d6, 0x10400,
    0x1049e, 0x104a0,
    0x104aa, 0x10800,
    0x10806, 0x10808,
    0x10809, 0x1080a,
    0x10836, 0x10837,
    0x10839, 0x1083c,
    0x1083d, 0x1083f,
    0x10856, 0x10857,
    0x10860, 0x10900,
    0x1091c, 0x1091f,
    0x1093a, 0x1093f,
    0x10940, 0x10a00,
    0x10a04, 0x10a05,
    0x10a07, 0x10a0c,
    0x10a14, 0x10a15,
    0x10a18, 0x10a19,
    0x10a34, 0x10a38,
    0x10a3b, 0x10a3f,
    0x10a48, 0x10a50,
    0x10a59, 0x10a60,
    0x10a80, 0x10b00,
    0x10b36, 0x10b39,
    0x10b56, 0x10b58,
    0x10b73, 0x10b78,
    0x10b80, 0x10c00,
    0x10c49, 0x10e60,
    0x10e7f, 0x11000,
    0x1104e, 0x11052,
    0x11070, 0x11080,
    0x110c2, 0x12000,
    0x1236f, 0x12400,
    0x12463, 0x12470,
    0x12474, 0x13000,
    0x1342f, 0x16800,
    0x16a39, 0x1b000,
    0x1b002, 0x1d000,
    0x1d0f6, 0x1d100,
    0x1d127, 0x1d129,
    0x1d1de, 0x1d200,
    0x1d246, 0x1d300,
    0x1d357, 0x1d360,
    0x1d372, 0x1d400,
    0x1d455, 0x1d456,
    0x1d49d, 0x1d49e,
    0x1d4a0, 0x1d4a2,
    0x1d4a3, 0x1d4a5,
    0x1d4a7, 0x1d4a9,
    0x1d4ad, 0x1d4ae,
    0x1d4ba, 0x1d4bb,
    0x1d4bc, 0x1d4bd,
    0x1d4c4, 0x1d4c5,
    0x1d506, 0x1d507,
    0x1d50b, 0x1d50d,
    0x1d515, 0x1d516,
    0x1d51d, 0x1d51e,
    0x1d53a, 0x1d53b,
    0x1d53f, 0x1d540,
    0x1d545, 0x1d546,
    0x1d547, 0x1d54a,
    0x1d551, 0x1d552,
    0x1d6a6, 0x1d6a8,
    0x1d7cc, 0x1d7ce,
    0x1d800, 0x1f000,
    0x1f02c, 0x1f030,
    0x1f094, 0x1f0a0,
    0x1f0af, 0x1f0b1,
    0x1f0bf, 0x1f0c1,
    0x1f0d0, 0x1f0d1,
    0x1f0e0, 0x1f100,
    0x1f10b, 0x1f110,
    0x1f12f, 0x1f130,
    0x1f16a, 0x1f170,
    0x1f19b, 0x1f1e6,
    0x1f203, 0x1f210,
    0x1f23b, 0x1f240,
    0x1f249, 0x1f250,
    0x1f252, 0x1f300,
    0x1f321, 0x1f330,
    0x1f336, 0x1f337,
    0x1f37d, 0x1f380,
    0x1f394, 0x1f3a0,
    0x1f3c5, 0x1f3c6,
    0x1f3cb, 0x1f3e0,
    0x1f3f1, 0x1f400,
    0x1f43f, 0x1f440,
    0x1f441, 0x1f442,
    0x1f4f8, 0x1f4f9,
    0x1f4fd, 0x1f500,
    0x1f53e, 0x1f550,
    0x1f568, 0x1f5fb,
    0x1f600, 0x1f601,
    0x1f611, 0x1f612,
    0x1f615, 0x1f616,
    0x1f617, 0x1f618,
    0x1f619, 0x1f61a,
    0x1f61b, 0x1f61c,
    0x1f61f, 0x1f620,
    0x1f626, 0x1f628,
    0x1f62c, 0x1f62d,
    0x1f62e, 0x1f630,
    0x1f634, 0x1f635,
    0x1f641, 0x1f645,
    0x1f650, 0x1f680,
    0x1f6c6, 0x1f700,
    0x1f774, 0x20000,
    0x2a6d7, 0x2a700,
    0x2b735, 0x2b740,
    0x2b81e, 0x2f800,
    0x2fa1e, 0xe0001,
    0xe0002, 0xe0020,
    0xe0080, 0xe0100,
    0xe01f0, 0xf0000,
    0xffffe, 0x100000,
    0x10fffe, 0x110000,
]);

immutable(RleBitSet!uint) unicodeInPhaistos_Disc = RleBitSet!uint([
    0x101d0, 0x10200,
]);

immutable(RleBitSet!uint) unicodeCo = RleBitSet!uint([
    0x0e000, 0x0f900,
    0xf0000, 0xffffe,
    0x100000, 0x10fffe,
]);

immutable(RleBitSet!uint) unicodeInKana_Supplement = RleBitSet!uint([
    0x1b000, 0x1b100,
]);

immutable(RleBitSet!uint) unicodeMath = RleBitSet!uint([
    0x0002b, 0x0002c,
    0x0003c, 0x0003f,
    0x0005e, 0x0005f,
    0x0007c, 0x0007d,
    0x0007e, 0x0007f,
    0x000ac, 0x000ad,
    0x000b1, 0x000b2,
    0x000d7, 0x000d8,
    0x000f7, 0x000f8,
    0x003d0, 0x003d3,
    0x003d5, 0x003d6,
    0x003f0, 0x003f2,
    0x003f4, 0x003f7,
    0x00606, 0x00609,
    0x02016, 0x02017,
    0x02032, 0x02035,
    0x02040, 0x02041,
    0x02044, 0x02045,
    0x02052, 0x02053,
    0x02061, 0x02065,
    0x0207a, 0x0207f,
    0x0208a, 0x0208f,
    0x020d0, 0x020dd,
    0x020e1, 0x020e2,
    0x020e5, 0x020e7,
    0x020eb, 0x020f0,
    0x02102, 0x02103,
    0x02107, 0x02108,
    0x0210a, 0x02114,
    0x02115, 0x02116,
    0x02118, 0x0211e,
    0x02124, 0x02125,
    0x02128, 0x0212a,
    0x0212c, 0x0212e,
    0x0212f, 0x02132,
    0x02133, 0x02139,
    0x0213c, 0x0214a,
    0x0214b, 0x0214c,
    0x02190, 0x021a8,
    0x021a9, 0x021af,
    0x021b0, 0x021b2,
    0x021b6, 0x021b8,
    0x021bc, 0x021dc,
    0x021dd, 0x021de,
    0x021e4, 0x021e6,
    0x021f4, 0x02300,
    0x02308, 0x0230c,
    0x02320, 0x02322,
    0x0237c, 0x0237d,
    0x0239b, 0x023b6,
    0x023b7, 0x023b8,
    0x023d0, 0x023d1,
    0x023dc, 0x023e3,
    0x025a0, 0x025a2,
    0x025ae, 0x025b8,
    0x025bc, 0x025c2,
    0x025c6, 0x025c8,
    0x025ca, 0x025cc,
    0x025cf, 0x025d4,
    0x025e2, 0x025e3,
    0x025e4, 0x025e5,
    0x025e7, 0x025ed,
    0x025f8, 0x02600,
    0x02605, 0x02607,
    0x02640, 0x02641,
    0x02642, 0x02643,
    0x02660, 0x02664,
    0x0266d, 0x02670,
    0x027c0, 0x027cb,
    0x027cc, 0x027cd,
    0x027ce, 0x02800,
    0x02900, 0x02b00,
    0x02b30, 0x02b45,
    0x02b47, 0x02b4d,
    0x0fb29, 0x0fb2a,
    0x0fe61, 0x0fe67,
    0x0fe68, 0x0fe69,
    0x0ff0b, 0x0ff0c,
    0x0ff1c, 0x0ff1f,
    0x0ff3c, 0x0ff3d,
    0x0ff3e, 0x0ff3f,
    0x0ff5c, 0x0ff5d,
    0x0ff5e, 0x0ff5f,
    0x0ffe2, 0x0ffe3,
    0x0ffe9, 0x0ffed,
    0x1d400, 0x1d455,
    0x1d456, 0x1d49d,
    0x1d49e, 0x1d4a0,
    0x1d4a2, 0x1d4a3,
    0x1d4a5, 0x1d4a7,
    0x1d4a9, 0x1d4ad,
    0x1d4ae, 0x1d4ba,
    0x1d4bb, 0x1d4bc,
    0x1d4bd, 0x1d4c4,
    0x1d4c5, 0x1d506,
    0x1d507, 0x1d50b,
    0x1d50d, 0x1d515,
    0x1d516, 0x1d51d,
    0x1d51e, 0x1d53a,
    0x1d53b, 0x1d53f,
    0x1d540, 0x1d545,
    0x1d546, 0x1d547,
    0x1d54a, 0x1d551,
    0x1d552, 0x1d6a6,
    0x1d6a8, 0x1d7cc,
    0x1d7ce, 0x1d800,
]);

immutable(RleBitSet!uint) unicodeInLinear_B_Ideograms = RleBitSet!uint([
    0x10080, 0x10100,
]);

immutable(RleBitSet!uint) unicodeInYi_Syllables = RleBitSet!uint([
    0x0a000, 0x0a490,
]);

immutable(RleBitSet!uint) unicodeInInscriptional_Pahlavi = RleBitSet!uint([
    0x10b60, 0x10b80,
]);

immutable(RleBitSet!uint) unicodeCs = RleBitSet!uint([
    0x0d800, 0x0e000,
]);

immutable(RleBitSet!uint) unicodeInHangul_Jamo_Extended_A = RleBitSet!uint([
    0x0a960, 0x0a980,
]);

immutable(RleBitSet!uint) unicodeInGlagolitic = RleBitSet!uint([
    0x02c00, 0x02c60,
]);

immutable(RleBitSet!uint) unicodeInHangul_Jamo_Extended_B = RleBitSet!uint([
    0x0d7b0, 0x0d800,
]);

immutable(RleBitSet!uint) unicodeInSpecials = RleBitSet!uint([
    0x0fff0, 0x10000,
]);

immutable(RleBitSet!uint) unicodeBopomofo = RleBitSet!uint([
    0x002ea, 0x002ec,
    0x03105, 0x0312e,
    0x031a0, 0x031bb,
]);

immutable(RleBitSet!uint) unicodeInBamum = RleBitSet!uint([
    0x0a6a0, 0x0a700,
]);

immutable(RleBitSet!uint) unicodeInPlaying_Cards = RleBitSet!uint([
    0x1f0a0, 0x1f100,
]);

immutable(RleBitSet!uint) unicodeInArabic = RleBitSet!uint([
    0x00600, 0x00700,
]);

immutable(RleBitSet!uint) unicodeVariation_Selector = RleBitSet!uint([
    0x0180b, 0x0180e,
    0x0fe00, 0x0fe10,
    0xe0100, 0xe01f0,
]);

immutable(RleBitSet!uint) unicodeInMathematical_Operators = RleBitSet!uint([
    0x02200, 0x02300,
]);

immutable(RleBitSet!uint) unicodeInPhonetic_Extensions_Supplement = RleBitSet!uint([
    0x01d80, 0x01dc0,
]);

immutable(RleBitSet!uint) unicodeInBrahmi = RleBitSet!uint([
    0x11000, 0x11080,
]);

immutable(RleBitSet!uint) unicodeMalayalam = RleBitSet!uint([
    0x00d02, 0x00d04,
    0x00d05, 0x00d0d,
    0x00d0e, 0x00d11,
    0x00d12, 0x00d3b,
    0x00d3d, 0x00d45,
    0x00d46, 0x00d49,
    0x00d4a, 0x00d4f,
    0x00d57, 0x00d58,
    0x00d60, 0x00d64,
    0x00d66, 0x00d76,
    0x00d79, 0x00d80,
]);

immutable(RleBitSet!uint) unicodeInCJK_Unified_Ideographs_Extension_A = RleBitSet!uint([
    0x03400, 0x04dc0,
]);

immutable(RleBitSet!uint) unicodeInCJK_Unified_Ideographs_Extension_B = RleBitSet!uint([
    0x20000, 0x2a6e0,
]);

immutable(RleBitSet!uint) unicodeInYijing_Hexagram_Symbols = RleBitSet!uint([
    0x04dc0, 0x04e00,
]);

immutable(RleBitSet!uint) unicodeInCJK_Unified_Ideographs_Extension_C = RleBitSet!uint([
    0x2a700, 0x2b740,
]);

immutable(RleBitSet!uint) unicodeInTibetan = RleBitSet!uint([
    0x00f00, 0x01000,
]);

immutable(RleBitSet!uint) unicodeInImperial_Aramaic = RleBitSet!uint([
    0x10840, 0x10860,
]);

immutable(RleBitSet!uint) unicodeInCJK_Unified_Ideographs_Extension_D = RleBitSet!uint([
    0x2b740, 0x2b820,
]);

immutable(RleBitSet!uint) unicodeInAlchemical_Symbols = RleBitSet!uint([
    0x1f700, 0x1f780,
]);

immutable(RleBitSet!uint) unicodePattern_White_Space = RleBitSet!uint([
    0x00009, 0x0000e,
    0x00020, 0x00021,
    0x00085, 0x00086,
    0x0200e, 0x02010,
    0x02028, 0x0202a,
]);

immutable(RleBitSet!uint) unicodeInEmoticons = RleBitSet!uint([
    0x1f600, 0x1f650,
]);

immutable(RleBitSet!uint) unicodeInYi_Radicals = RleBitSet!uint([
    0x0a490, 0x0a4d0,
]);

immutable(RleBitSet!uint) unicodeInBraille_Patterns = RleBitSet!uint([
    0x02800, 0x02900,
]);

immutable(RleBitSet!uint) unicodeInscriptional_Pahlavi = RleBitSet!uint([
    0x10b60, 0x10b73,
    0x10b78, 0x10b80,
]);

immutable(RleBitSet!uint) unicodeInCoptic = RleBitSet!uint([
    0x02c80, 0x02d00,
]);

immutable(RleBitSet!uint) unicodeInGothic = RleBitSet!uint([
    0x10330, 0x10350,
]);

immutable(RleBitSet!uint) unicodeInDevanagari = RleBitSet!uint([
    0x00900, 0x00980,
]);

immutable(RleBitSet!uint) unicodeSTerm = RleBitSet!uint([
    0x00021, 0x00022,
    0x0002e, 0x0002f,
    0x0003f, 0x00040,
    0x0055c, 0x0055d,
    0x0055e, 0x0055f,
    0x00589, 0x0058a,
    0x0061f, 0x00620,
    0x006d4, 0x006d5,
    0x00700, 0x00703,
    0x007f9, 0x007fa,
    0x00964, 0x00966,
    0x0104a, 0x0104c,
    0x01362, 0x01363,
    0x01367, 0x01369,
    0x0166e, 0x0166f,
    0x01735, 0x01737,
    0x01803, 0x01804,
    0x01809, 0x0180a,
    0x01944, 0x01946,
    0x01aa8, 0x01aac,
    0x01b5a, 0x01b5c,
    0x01b5e, 0x01b60,
    0x01c3b, 0x01c3d,
    0x01c7e, 0x01c80,
    0x0203c, 0x0203e,
    0x02047, 0x0204a,
    0x02e2e, 0x02e2f,
    0x03002, 0x03003,
    0x0a4ff, 0x0a500,
    0x0a60e, 0x0a610,
    0x0a6f3, 0x0a6f4,
    0x0a6f7, 0x0a6f8,
    0x0a876, 0x0a878,
    0x0a8ce, 0x0a8d0,
    0x0a92f, 0x0a930,
    0x0a9c8, 0x0a9ca,
    0x0aa5d, 0x0aa60,
    0x0abeb, 0x0abec,
    0x0fe52, 0x0fe53,
    0x0fe56, 0x0fe58,
    0x0ff01, 0x0ff02,
    0x0ff0e, 0x0ff0f,
    0x0ff1f, 0x0ff20,
    0x0ff61, 0x0ff62,
    0x10a56, 0x10a58,
    0x11047, 0x11049,
    0x110be, 0x110c2,
]);

immutable(RleBitSet!uint) unicodeInTai_Viet = RleBitSet!uint([
    0x0aa80, 0x0aae0,
]);

immutable(RleBitSet!uint) unicodeInDeseret = RleBitSet!uint([
    0x10400, 0x10450,
]);

immutable(RleBitSet!uint) unicodeHebrew = RleBitSet!uint([
    0x00591, 0x005c8,
    0x005d0, 0x005eb,
    0x005f0, 0x005f5,
    0x0fb1d, 0x0fb37,
    0x0fb38, 0x0fb3d,
    0x0fb3e, 0x0fb3f,
    0x0fb40, 0x0fb42,
    0x0fb43, 0x0fb45,
    0x0fb46, 0x0fb50,
]);

immutable(RleBitSet!uint) unicodeLisu = RleBitSet!uint([
    0x0a4d0, 0x0a500,
]);

immutable(RleBitSet!uint) unicodeInSupplementary_Private_Use_Area_A = RleBitSet!uint([
    0xf0000, 0x100000,
]);

immutable(RleBitSet!uint) unicodeInSupplementary_Private_Use_Area_B = RleBitSet!uint([
    0x100000, 0x110000,
]);

immutable(RleBitSet!uint) unicodeUgaritic = RleBitSet!uint([
    0x10380, 0x1039e,
    0x1039f, 0x103a0,
]);

immutable(RleBitSet!uint) unicodeInMongolian = RleBitSet!uint([
    0x01800, 0x018b0,
]);

immutable(RleBitSet!uint) unicodeInBopomofo = RleBitSet!uint([
    0x03100, 0x03130,
]);

immutable(RleBitSet!uint) unicodeOld_Persian = RleBitSet!uint([
    0x103a0, 0x103c4,
    0x103c8, 0x103d6,
]);

immutable(RleBitSet!uint) unicodeInCJK_Compatibility_Ideographs_Supplement = RleBitSet!uint([
    0x2f800, 0x2fa20,
]);

immutable(RleBitSet!uint) unicodeInMahjong_Tiles = RleBitSet!uint([
    0x1f000, 0x1f030,
]);

immutable(RleBitSet!uint) unicodePhoenician = RleBitSet!uint([
    0x10900, 0x1091c,
    0x1091f, 0x10920,
]);

immutable(RleBitSet!uint) unicodeEgyptian_Hieroglyphs = RleBitSet!uint([
    0x13000, 0x1342f,
]);

immutable(RleBitSet!uint) unicodeInVariation_Selectors_Supplement = RleBitSet!uint([
    0xe0100, 0xe01f0,
]);

immutable(RleBitSet!uint) unicodeInLisu = RleBitSet!uint([
    0x0a4d0, 0x0a500,
]);

immutable(RleBitSet!uint) unicodeInLow_Surrogates = RleBitSet!uint([
    0x0dc00, 0x0e000,
]);

immutable(RleBitSet!uint) unicodeInUgaritic = RleBitSet!uint([
    0x10380, 0x103a0,
]);

immutable(RleBitSet!uint) unicodeInAncient_Greek_Musical_Notation = RleBitSet!uint([
    0x1d200, 0x1d250,
]);

immutable(RleBitSet!uint) unicodeInSupplemental_Punctuation = RleBitSet!uint([
    0x02e00, 0x02e80,
]);

immutable(RleBitSet!uint) unicodeInTamil = RleBitSet!uint([
    0x00b80, 0x00c00,
]);

immutable(RleBitSet!uint) unicodeBidi_Control = RleBitSet!uint([
    0x0200e, 0x02010,
    0x0202a, 0x0202f,
]);

immutable(RleBitSet!uint) unicodeKatakana = RleBitSet!uint([
    0x030a1, 0x030fb,
    0x030fd, 0x03100,
    0x031f0, 0x03200,
    0x032d0, 0x032ff,
    0x03300, 0x03358,
    0x0ff66, 0x0ff70,
    0x0ff71, 0x0ff9e,
    0x1b000, 0x1b001,
]);

immutable(RleBitSet!uint) unicodeInKanbun = RleBitSet!uint([
    0x03190, 0x031a0,
]);

immutable(RleBitSet!uint) unicodeInHebrew = RleBitSet!uint([
    0x00590, 0x00600,
]);

immutable(RleBitSet!uint) unicodeTai_Viet = RleBitSet!uint([
    0x0aa80, 0x0aac3,
    0x0aadb, 0x0aae0,
]);

immutable(RleBitSet!uint) unicodeShavian = RleBitSet!uint([
    0x10450, 0x10480,
]);

immutable(RleBitSet!uint) unicodeInHanunoo = RleBitSet!uint([
    0x01720, 0x01740,
]);

immutable(RleBitSet!uint) unicodeOther_Lowercase = RleBitSet!uint([
    0x002b0, 0x002b9,
    0x002c0, 0x002c2,
    0x002e0, 0x002e5,
    0x00345, 0x00346,
    0x0037a, 0x0037b,
    0x01d2c, 0x01d62,
    0x01d78, 0x01d79,
    0x01d9b, 0x01dc0,
    0x02090, 0x02095,
    0x02170, 0x02180,
    0x024d0, 0x024ea,
    0x02c7d, 0x02c7e,
    0x0a770, 0x0a771,
]);

immutable(RleBitSet!uint) unicodeDash = RleBitSet!uint([
    0x0002d, 0x0002e,
    0x0058a, 0x0058b,
    0x005be, 0x005bf,
    0x01400, 0x01401,
    0x01806, 0x01807,
    0x02010, 0x02016,
    0x02053, 0x02054,
    0x0207b, 0x0207c,
    0x0208b, 0x0208c,
    0x02212, 0x02213,
    0x02e17, 0x02e18,
    0x02e1a, 0x02e1b,
    0x0301c, 0x0301d,
    0x03030, 0x03031,
    0x030a0, 0x030a1,
    0x0fe31, 0x0fe33,
    0x0fe58, 0x0fe59,
    0x0fe63, 0x0fe64,
    0x0ff0d, 0x0ff0e,
]);

immutable(RleBitSet!uint) unicodeInSinhala = RleBitSet!uint([
    0x00d80, 0x00e00,
]);

immutable(RleBitSet!uint) unicodeKaithi = RleBitSet!uint([
    0x11080, 0x110c2,
]);

immutable(RleBitSet!uint) unicodeInSundanese = RleBitSet!uint([
    0x01b80, 0x01bc0,
]);

immutable(RleBitSet!uint) unicodeInEthiopic_Extended_A = RleBitSet!uint([
    0x0ab00, 0x0ab30,
]);

immutable(RleBitSet!uint) unicodeInAegean_Numbers = RleBitSet!uint([
    0x10100, 0x10140,
]);

immutable(RleBitSet!uint) unicodeTibetan = RleBitSet!uint([
    0x00f00, 0x00f48,
    0x00f49, 0x00f6d,
    0x00f71, 0x00f98,
    0x00f99, 0x00fbd,
    0x00fbe, 0x00fcd,
    0x00fce, 0x00fd5,
    0x00fd9, 0x00fdb,
]);

immutable(RleBitSet!uint) unicodeSamaritan = RleBitSet!uint([
    0x00800, 0x0082e,
    0x00830, 0x0083f,
]);

immutable(RleBitSet!uint) unicodeInOptical_Character_Recognition = RleBitSet!uint([
    0x02440, 0x02460,
]);

immutable(RleBitSet!uint) unicodeInKatakana = RleBitSet!uint([
    0x030a0, 0x03100,
]);

immutable(RleBitSet!uint) unicodeInVariation_Selectors = RleBitSet!uint([
    0x0fe00, 0x0fe10,
]);

immutable(RleBitSet!uint) unicodeOld_Italic = RleBitSet!uint([
    0x10300, 0x1031f,
    0x10320, 0x10324,
]);

immutable(RleBitSet!uint) unicodeInEnclosed_CJK_Letters_and_Months = RleBitSet!uint([
    0x03200, 0x03300,
]);

immutable(RleBitSet!uint) unicodeInLatin_1_Supplement = RleBitSet!uint([
    0x00080, 0x00100,
]);

immutable(RleBitSet!uint) unicodeHan = RleBitSet!uint([
    0x02e80, 0x02e9a,
    0x02e9b, 0x02ef4,
    0x02f00, 0x02fd6,
    0x03005, 0x03006,
    0x03007, 0x03008,
    0x03021, 0x0302a,
    0x03038, 0x0303c,
    0x03400, 0x04db6,
    0x04e00, 0x09fcc,
    0x0f900, 0x0fa2e,
    0x0fa30, 0x0fa6e,
    0x0fa70, 0x0fada,
    0x20000, 0x2a6d7,
    0x2a700, 0x2b735,
    0x2b740, 0x2b81e,
    0x2f800, 0x2fa1e,
]);

immutable(RleBitSet!uint) unicodeDeseret = RleBitSet!uint([
    0x10400, 0x10450,
]);

immutable(RleBitSet!uint) unicodeInMyanmar = RleBitSet!uint([
    0x01000, 0x010a0,
]);

immutable(RleBitSet!uint) unicodeLl = RleBitSet!uint([
    0x00061, 0x0007b,
    0x000aa, 0x000ab,
    0x000b5, 0x000b6,
    0x000ba, 0x000bb,
    0x000df, 0x000f7,
    0x000f8, 0x00100,
    0x00101, 0x00102,
    0x00103, 0x00104,
    0x00105, 0x00106,
    0x00107, 0x00108,
    0x00109, 0x0010a,
    0x0010b, 0x0010c,
    0x0010d, 0x0010e,
    0x0010f, 0x00110,
    0x00111, 0x00112,
    0x00113, 0x00114,
    0x00115, 0x00116,
    0x00117, 0x00118,
    0x00119, 0x0011a,
    0x0011b, 0x0011c,
    0x0011d, 0x0011e,
    0x0011f, 0x00120,
    0x00121, 0x00122,
    0x00123, 0x00124,
    0x00125, 0x00126,
    0x00127, 0x00128,
    0x00129, 0x0012a,
    0x0012b, 0x0012c,
    0x0012d, 0x0012e,
    0x0012f, 0x00130,
    0x00131, 0x00132,
    0x00133, 0x00134,
    0x00135, 0x00136,
    0x00137, 0x00139,
    0x0013a, 0x0013b,
    0x0013c, 0x0013d,
    0x0013e, 0x0013f,
    0x00140, 0x00141,
    0x00142, 0x00143,
    0x00144, 0x00145,
    0x00146, 0x00147,
    0x00148, 0x0014a,
    0x0014b, 0x0014c,
    0x0014d, 0x0014e,
    0x0014f, 0x00150,
    0x00151, 0x00152,
    0x00153, 0x00154,
    0x00155, 0x00156,
    0x00157, 0x00158,
    0x00159, 0x0015a,
    0x0015b, 0x0015c,
    0x0015d, 0x0015e,
    0x0015f, 0x00160,
    0x00161, 0x00162,
    0x00163, 0x00164,
    0x00165, 0x00166,
    0x00167, 0x00168,
    0x00169, 0x0016a,
    0x0016b, 0x0016c,
    0x0016d, 0x0016e,
    0x0016f, 0x00170,
    0x00171, 0x00172,
    0x00173, 0x00174,
    0x00175, 0x00176,
    0x00177, 0x00178,
    0x0017a, 0x0017b,
    0x0017c, 0x0017d,
    0x0017e, 0x00181,
    0x00183, 0x00184,
    0x00185, 0x00186,
    0x00188, 0x00189,
    0x0018c, 0x0018e,
    0x00192, 0x00193,
    0x00195, 0x00196,
    0x00199, 0x0019c,
    0x0019e, 0x0019f,
    0x001a1, 0x001a2,
    0x001a3, 0x001a4,
    0x001a5, 0x001a6,
    0x001a8, 0x001a9,
    0x001aa, 0x001ac,
    0x001ad, 0x001ae,
    0x001b0, 0x001b1,
    0x001b4, 0x001b5,
    0x001b6, 0x001b7,
    0x001b9, 0x001bb,
    0x001bd, 0x001c0,
    0x001c6, 0x001c7,
    0x001c9, 0x001ca,
    0x001cc, 0x001cd,
    0x001ce, 0x001cf,
    0x001d0, 0x001d1,
    0x001d2, 0x001d3,
    0x001d4, 0x001d5,
    0x001d6, 0x001d7,
    0x001d8, 0x001d9,
    0x001da, 0x001db,
    0x001dc, 0x001de,
    0x001df, 0x001e0,
    0x001e1, 0x001e2,
    0x001e3, 0x001e4,
    0x001e5, 0x001e6,
    0x001e7, 0x001e8,
    0x001e9, 0x001ea,
    0x001eb, 0x001ec,
    0x001ed, 0x001ee,
    0x001ef, 0x001f1,
    0x001f3, 0x001f4,
    0x001f5, 0x001f6,
    0x001f9, 0x001fa,
    0x001fb, 0x001fc,
    0x001fd, 0x001fe,
    0x001ff, 0x00200,
    0x00201, 0x00202,
    0x00203, 0x00204,
    0x00205, 0x00206,
    0x00207, 0x00208,
    0x00209, 0x0020a,
    0x0020b, 0x0020c,
    0x0020d, 0x0020e,
    0x0020f, 0x00210,
    0x00211, 0x00212,
    0x00213, 0x00214,
    0x00215, 0x00216,
    0x00217, 0x00218,
    0x00219, 0x0021a,
    0x0021b, 0x0021c,
    0x0021d, 0x0021e,
    0x0021f, 0x00220,
    0x00221, 0x00222,
    0x00223, 0x00224,
    0x00225, 0x00226,
    0x00227, 0x00228,
    0x00229, 0x0022a,
    0x0022b, 0x0022c,
    0x0022d, 0x0022e,
    0x0022f, 0x00230,
    0x00231, 0x00232,
    0x00233, 0x0023a,
    0x0023c, 0x0023d,
    0x0023f, 0x00241,
    0x00242, 0x00243,
    0x00247, 0x00248,
    0x00249, 0x0024a,
    0x0024b, 0x0024c,
    0x0024d, 0x0024e,
    0x0024f, 0x00294,
    0x00295, 0x002b0,
    0x00371, 0x00372,
    0x00373, 0x00374,
    0x00377, 0x00378,
    0x0037b, 0x0037e,
    0x00390, 0x00391,
    0x003ac, 0x003cf,
    0x003d0, 0x003d2,
    0x003d5, 0x003d8,
    0x003d9, 0x003da,
    0x003db, 0x003dc,
    0x003dd, 0x003de,
    0x003df, 0x003e0,
    0x003e1, 0x003e2,
    0x003e3, 0x003e4,
    0x003e5, 0x003e6,
    0x003e7, 0x003e8,
    0x003e9, 0x003ea,
    0x003eb, 0x003ec,
    0x003ed, 0x003ee,
    0x003ef, 0x003f4,
    0x003f5, 0x003f6,
    0x003f8, 0x003f9,
    0x003fb, 0x003fd,
    0x00430, 0x00460,
    0x00461, 0x00462,
    0x00463, 0x00464,
    0x00465, 0x00466,
    0x00467, 0x00468,
    0x00469, 0x0046a,
    0x0046b, 0x0046c,
    0x0046d, 0x0046e,
    0x0046f, 0x00470,
    0x00471, 0x00472,
    0x00473, 0x00474,
    0x00475, 0x00476,
    0x00477, 0x00478,
    0x00479, 0x0047a,
    0x0047b, 0x0047c,
    0x0047d, 0x0047e,
    0x0047f, 0x00480,
    0x00481, 0x00482,
    0x0048b, 0x0048c,
    0x0048d, 0x0048e,
    0x0048f, 0x00490,
    0x00491, 0x00492,
    0x00493, 0x00494,
    0x00495, 0x00496,
    0x00497, 0x00498,
    0x00499, 0x0049a,
    0x0049b, 0x0049c,
    0x0049d, 0x0049e,
    0x0049f, 0x004a0,
    0x004a1, 0x004a2,
    0x004a3, 0x004a4,
    0x004a5, 0x004a6,
    0x004a7, 0x004a8,
    0x004a9, 0x004aa,
    0x004ab, 0x004ac,
    0x004ad, 0x004ae,
    0x004af, 0x004b0,
    0x004b1, 0x004b2,
    0x004b3, 0x004b4,
    0x004b5, 0x004b6,
    0x004b7, 0x004b8,
    0x004b9, 0x004ba,
    0x004bb, 0x004bc,
    0x004bd, 0x004be,
    0x004bf, 0x004c0,
    0x004c2, 0x004c3,
    0x004c4, 0x004c5,
    0x004c6, 0x004c7,
    0x004c8, 0x004c9,
    0x004ca, 0x004cb,
    0x004cc, 0x004cd,
    0x004ce, 0x004d0,
    0x004d1, 0x004d2,
    0x004d3, 0x004d4,
    0x004d5, 0x004d6,
    0x004d7, 0x004d8,
    0x004d9, 0x004da,
    0x004db, 0x004dc,
    0x004dd, 0x004de,
    0x004df, 0x004e0,
    0x004e1, 0x004e2,
    0x004e3, 0x004e4,
    0x004e5, 0x004e6,
    0x004e7, 0x004e8,
    0x004e9, 0x004ea,
    0x004eb, 0x004ec,
    0x004ed, 0x004ee,
    0x004ef, 0x004f0,
    0x004f1, 0x004f2,
    0x004f3, 0x004f4,
    0x004f5, 0x004f6,
    0x004f7, 0x004f8,
    0x004f9, 0x004fa,
    0x004fb, 0x004fc,
    0x004fd, 0x004fe,
    0x004ff, 0x00500,
    0x00501, 0x00502,
    0x00503, 0x00504,
    0x00505, 0x00506,
    0x00507, 0x00508,
    0x00509, 0x0050a,
    0x0050b, 0x0050c,
    0x0050d, 0x0050e,
    0x0050f, 0x00510,
    0x00511, 0x00512,
    0x00513, 0x00514,
    0x00515, 0x00516,
    0x00517, 0x00518,
    0x00519, 0x0051a,
    0x0051b, 0x0051c,
    0x0051d, 0x0051e,
    0x0051f, 0x00520,
    0x00521, 0x00522,
    0x00523, 0x00524,
    0x00525, 0x00526,
    0x00527, 0x00528,
    0x00561, 0x00588,
    0x01d00, 0x01d2c,
    0x01d62, 0x01d78,
    0x01d79, 0x01d9b,
    0x01e01, 0x01e02,
    0x01e03, 0x01e04,
    0x01e05, 0x01e06,
    0x01e07, 0x01e08,
    0x01e09, 0x01e0a,
    0x01e0b, 0x01e0c,
    0x01e0d, 0x01e0e,
    0x01e0f, 0x01e10,
    0x01e11, 0x01e12,
    0x01e13, 0x01e14,
    0x01e15, 0x01e16,
    0x01e17, 0x01e18,
    0x01e19, 0x01e1a,
    0x01e1b, 0x01e1c,
    0x01e1d, 0x01e1e,
    0x01e1f, 0x01e20,
    0x01e21, 0x01e22,
    0x01e23, 0x01e24,
    0x01e25, 0x01e26,
    0x01e27, 0x01e28,
    0x01e29, 0x01e2a,
    0x01e2b, 0x01e2c,
    0x01e2d, 0x01e2e,
    0x01e2f, 0x01e30,
    0x01e31, 0x01e32,
    0x01e33, 0x01e34,
    0x01e35, 0x01e36,
    0x01e37, 0x01e38,
    0x01e39, 0x01e3a,
    0x01e3b, 0x01e3c,
    0x01e3d, 0x01e3e,
    0x01e3f, 0x01e40,
    0x01e41, 0x01e42,
    0x01e43, 0x01e44,
    0x01e45, 0x01e46,
    0x01e47, 0x01e48,
    0x01e49, 0x01e4a,
    0x01e4b, 0x01e4c,
    0x01e4d, 0x01e4e,
    0x01e4f, 0x01e50,
    0x01e51, 0x01e52,
    0x01e53, 0x01e54,
    0x01e55, 0x01e56,
    0x01e57, 0x01e58,
    0x01e59, 0x01e5a,
    0x01e5b, 0x01e5c,
    0x01e5d, 0x01e5e,
    0x01e5f, 0x01e60,
    0x01e61, 0x01e62,
    0x01e63, 0x01e64,
    0x01e65, 0x01e66,
    0x01e67, 0x01e68,
    0x01e69, 0x01e6a,
    0x01e6b, 0x01e6c,
    0x01e6d, 0x01e6e,
    0x01e6f, 0x01e70,
    0x01e71, 0x01e72,
    0x01e73, 0x01e74,
    0x01e75, 0x01e76,
    0x01e77, 0x01e78,
    0x01e79, 0x01e7a,
    0x01e7b, 0x01e7c,
    0x01e7d, 0x01e7e,
    0x01e7f, 0x01e80,
    0x01e81, 0x01e82,
    0x01e83, 0x01e84,
    0x01e85, 0x01e86,
    0x01e87, 0x01e88,
    0x01e89, 0x01e8a,
    0x01e8b, 0x01e8c,
    0x01e8d, 0x01e8e,
    0x01e8f, 0x01e90,
    0x01e91, 0x01e92,
    0x01e93, 0x01e94,
    0x01e95, 0x01e9e,
    0x01e9f, 0x01ea0,
    0x01ea1, 0x01ea2,
    0x01ea3, 0x01ea4,
    0x01ea5, 0x01ea6,
    0x01ea7, 0x01ea8,
    0x01ea9, 0x01eaa,
    0x01eab, 0x01eac,
    0x01ead, 0x01eae,
    0x01eaf, 0x01eb0,
    0x01eb1, 0x01eb2,
    0x01eb3, 0x01eb4,
    0x01eb5, 0x01eb6,
    0x01eb7, 0x01eb8,
    0x01eb9, 0x01eba,
    0x01ebb, 0x01ebc,
    0x01ebd, 0x01ebe,
    0x01ebf, 0x01ec0,
    0x01ec1, 0x01ec2,
    0x01ec3, 0x01ec4,
    0x01ec5, 0x01ec6,
    0x01ec7, 0x01ec8,
    0x01ec9, 0x01eca,
    0x01ecb, 0x01ecc,
    0x01ecd, 0x01ece,
    0x01ecf, 0x01ed0,
    0x01ed1, 0x01ed2,
    0x01ed3, 0x01ed4,
    0x01ed5, 0x01ed6,
    0x01ed7, 0x01ed8,
    0x01ed9, 0x01eda,
    0x01edb, 0x01edc,
    0x01edd, 0x01ede,
    0x01edf, 0x01ee0,
    0x01ee1, 0x01ee2,
    0x01ee3, 0x01ee4,
    0x01ee5, 0x01ee6,
    0x01ee7, 0x01ee8,
    0x01ee9, 0x01eea,
    0x01eeb, 0x01eec,
    0x01eed, 0x01eee,
    0x01eef, 0x01ef0,
    0x01ef1, 0x01ef2,
    0x01ef3, 0x01ef4,
    0x01ef5, 0x01ef6,
    0x01ef7, 0x01ef8,
    0x01ef9, 0x01efa,
    0x01efb, 0x01efc,
    0x01efd, 0x01efe,
    0x01eff, 0x01f08,
    0x01f10, 0x01f16,
    0x01f20, 0x01f28,
    0x01f30, 0x01f38,
    0x01f40, 0x01f46,
    0x01f50, 0x01f58,
    0x01f60, 0x01f68,
    0x01f70, 0x01f7e,
    0x01f80, 0x01f88,
    0x01f90, 0x01f98,
    0x01fa0, 0x01fa8,
    0x01fb0, 0x01fb5,
    0x01fb6, 0x01fb8,
    0x01fbe, 0x01fbf,
    0x01fc2, 0x01fc5,
    0x01fc6, 0x01fc8,
    0x01fd0, 0x01fd4,
    0x01fd6, 0x01fd8,
    0x01fe0, 0x01fe8,
    0x01ff2, 0x01ff5,
    0x01ff6, 0x01ff8,
    0x0210a, 0x0210b,
    0x0210e, 0x02110,
    0x02113, 0x02114,
    0x0212f, 0x02130,
    0x02134, 0x02135,
    0x02139, 0x0213a,
    0x0213c, 0x0213e,
    0x02146, 0x0214a,
    0x0214e, 0x0214f,
    0x02184, 0x02185,
    0x02c30, 0x02c5f,
    0x02c61, 0x02c62,
    0x02c65, 0x02c67,
    0x02c68, 0x02c69,
    0x02c6a, 0x02c6b,
    0x02c6c, 0x02c6d,
    0x02c71, 0x02c72,
    0x02c73, 0x02c75,
    0x02c76, 0x02c7d,
    0x02c81, 0x02c82,
    0x02c83, 0x02c84,
    0x02c85, 0x02c86,
    0x02c87, 0x02c88,
    0x02c89, 0x02c8a,
    0x02c8b, 0x02c8c,
    0x02c8d, 0x02c8e,
    0x02c8f, 0x02c90,
    0x02c91, 0x02c92,
    0x02c93, 0x02c94,
    0x02c95, 0x02c96,
    0x02c97, 0x02c98,
    0x02c99, 0x02c9a,
    0x02c9b, 0x02c9c,
    0x02c9d, 0x02c9e,
    0x02c9f, 0x02ca0,
    0x02ca1, 0x02ca2,
    0x02ca3, 0x02ca4,
    0x02ca5, 0x02ca6,
    0x02ca7, 0x02ca8,
    0x02ca9, 0x02caa,
    0x02cab, 0x02cac,
    0x02cad, 0x02cae,
    0x02caf, 0x02cb0,
    0x02cb1, 0x02cb2,
    0x02cb3, 0x02cb4,
    0x02cb5, 0x02cb6,
    0x02cb7, 0x02cb8,
    0x02cb9, 0x02cba,
    0x02cbb, 0x02cbc,
    0x02cbd, 0x02cbe,
    0x02cbf, 0x02cc0,
    0x02cc1, 0x02cc2,
    0x02cc3, 0x02cc4,
    0x02cc5, 0x02cc6,
    0x02cc7, 0x02cc8,
    0x02cc9, 0x02cca,
    0x02ccb, 0x02ccc,
    0x02ccd, 0x02cce,
    0x02ccf, 0x02cd0,
    0x02cd1, 0x02cd2,
    0x02cd3, 0x02cd4,
    0x02cd5, 0x02cd6,
    0x02cd7, 0x02cd8,
    0x02cd9, 0x02cda,
    0x02cdb, 0x02cdc,
    0x02cdd, 0x02cde,
    0x02cdf, 0x02ce0,
    0x02ce1, 0x02ce2,
    0x02ce3, 0x02ce5,
    0x02cec, 0x02ced,
    0x02cee, 0x02cef,
    0x02d00, 0x02d26,
    0x0a641, 0x0a642,
    0x0a643, 0x0a644,
    0x0a645, 0x0a646,
    0x0a647, 0x0a648,
    0x0a649, 0x0a64a,
    0x0a64b, 0x0a64c,
    0x0a64d, 0x0a64e,
    0x0a64f, 0x0a650,
    0x0a651, 0x0a652,
    0x0a653, 0x0a654,
    0x0a655, 0x0a656,
    0x0a657, 0x0a658,
    0x0a659, 0x0a65a,
    0x0a65b, 0x0a65c,
    0x0a65d, 0x0a65e,
    0x0a65f, 0x0a660,
    0x0a661, 0x0a662,
    0x0a663, 0x0a664,
    0x0a665, 0x0a666,
    0x0a667, 0x0a668,
    0x0a669, 0x0a66a,
    0x0a66b, 0x0a66c,
    0x0a66d, 0x0a66e,
    0x0a681, 0x0a682,
    0x0a683, 0x0a684,
    0x0a685, 0x0a686,
    0x0a687, 0x0a688,
    0x0a689, 0x0a68a,
    0x0a68b, 0x0a68c,
    0x0a68d, 0x0a68e,
    0x0a68f, 0x0a690,
    0x0a691, 0x0a692,
    0x0a693, 0x0a694,
    0x0a695, 0x0a696,
    0x0a697, 0x0a698,
    0x0a723, 0x0a724,
    0x0a725, 0x0a726,
    0x0a727, 0x0a728,
    0x0a729, 0x0a72a,
    0x0a72b, 0x0a72c,
    0x0a72d, 0x0a72e,
    0x0a72f, 0x0a732,
    0x0a733, 0x0a734,
    0x0a735, 0x0a736,
    0x0a737, 0x0a738,
    0x0a739, 0x0a73a,
    0x0a73b, 0x0a73c,
    0x0a73d, 0x0a73e,
    0x0a73f, 0x0a740,
    0x0a741, 0x0a742,
    0x0a743, 0x0a744,
    0x0a745, 0x0a746,
    0x0a747, 0x0a748,
    0x0a749, 0x0a74a,
    0x0a74b, 0x0a74c,
    0x0a74d, 0x0a74e,
    0x0a74f, 0x0a750,
    0x0a751, 0x0a752,
    0x0a753, 0x0a754,
    0x0a755, 0x0a756,
    0x0a757, 0x0a758,
    0x0a759, 0x0a75a,
    0x0a75b, 0x0a75c,
    0x0a75d, 0x0a75e,
    0x0a75f, 0x0a760,
    0x0a761, 0x0a762,
    0x0a763, 0x0a764,
    0x0a765, 0x0a766,
    0x0a767, 0x0a768,
    0x0a769, 0x0a76a,
    0x0a76b, 0x0a76c,
    0x0a76d, 0x0a76e,
    0x0a76f, 0x0a770,
    0x0a771, 0x0a779,
    0x0a77a, 0x0a77b,
    0x0a77c, 0x0a77d,
    0x0a77f, 0x0a780,
    0x0a781, 0x0a782,
    0x0a783, 0x0a784,
    0x0a785, 0x0a786,
    0x0a787, 0x0a788,
    0x0a78c, 0x0a78d,
    0x0a78e, 0x0a78f,
    0x0a791, 0x0a792,
    0x0a7a1, 0x0a7a2,
    0x0a7a3, 0x0a7a4,
    0x0a7a5, 0x0a7a6,
    0x0a7a7, 0x0a7a8,
    0x0a7a9, 0x0a7aa,
    0x0a7fa, 0x0a7fb,
    0x0fb00, 0x0fb07,
    0x0fb13, 0x0fb18,
    0x0ff41, 0x0ff5b,
    0x10428, 0x10450,
    0x1d41a, 0x1d434,
    0x1d44e, 0x1d455,
    0x1d456, 0x1d468,
    0x1d482, 0x1d49c,
    0x1d4b6, 0x1d4ba,
    0x1d4bb, 0x1d4bc,
    0x1d4bd, 0x1d4c4,
    0x1d4c5, 0x1d4d0,
    0x1d4ea, 0x1d504,
    0x1d51e, 0x1d538,
    0x1d552, 0x1d56c,
    0x1d586, 0x1d5a0,
    0x1d5ba, 0x1d5d4,
    0x1d5ee, 0x1d608,
    0x1d622, 0x1d63c,
    0x1d656, 0x1d670,
    0x1d68a, 0x1d6a6,
    0x1d6c2, 0x1d6db,
    0x1d6dc, 0x1d6e2,
    0x1d6fc, 0x1d715,
    0x1d716, 0x1d71c,
    0x1d736, 0x1d74f,
    0x1d750, 0x1d756,
    0x1d770, 0x1d789,
    0x1d78a, 0x1d790,
    0x1d7aa, 0x1d7c3,
    0x1d7c4, 0x1d7ca,
    0x1d7cb, 0x1d7cc,
]);

immutable(RleBitSet!uint) unicodeSaurashtra = RleBitSet!uint([
    0x0a880, 0x0a8c5,
    0x0a8ce, 0x0a8da,
]);

immutable(RleBitSet!uint) unicodeInLinear_B_Syllabary = RleBitSet!uint([
    0x10000, 0x10080,
]);

immutable(RleBitSet!uint) unicodeLm = RleBitSet!uint([
    0x002b0, 0x002c2,
    0x002c6, 0x002d2,
    0x002e0, 0x002e5,
    0x002ec, 0x002ed,
    0x002ee, 0x002ef,
    0x00374, 0x00375,
    0x0037a, 0x0037b,
    0x00559, 0x0055a,
    0x00640, 0x00641,
    0x006e5, 0x006e7,
    0x007f4, 0x007f6,
    0x007fa, 0x007fb,
    0x0081a, 0x0081b,
    0x00824, 0x00825,
    0x00828, 0x00829,
    0x00971, 0x00972,
    0x00e46, 0x00e47,
    0x00ec6, 0x00ec7,
    0x010fc, 0x010fd,
    0x017d7, 0x017d8,
    0x01843, 0x01844,
    0x01aa7, 0x01aa8,
    0x01c78, 0x01c7e,
    0x01d2c, 0x01d62,
    0x01d78, 0x01d79,
    0x01d9b, 0x01dc0,
    0x02071, 0x02072,
    0x0207f, 0x02080,
    0x02090, 0x0209d,
    0x02c7d, 0x02c7e,
    0x02d6f, 0x02d70,
    0x02e2f, 0x02e30,
    0x03005, 0x03006,
    0x03031, 0x03036,
    0x0303b, 0x0303c,
    0x0309d, 0x0309f,
    0x030fc, 0x030ff,
    0x0a015, 0x0a016,
    0x0a4f8, 0x0a4fe,
    0x0a60c, 0x0a60d,
    0x0a67f, 0x0a680,
    0x0a717, 0x0a720,
    0x0a770, 0x0a771,
    0x0a788, 0x0a789,
    0x0a9cf, 0x0a9d0,
    0x0aa70, 0x0aa71,
    0x0aadd, 0x0aade,
    0x0ff70, 0x0ff71,
    0x0ff9e, 0x0ffa0,
]);

immutable(RleBitSet!uint) unicodeInKannada = RleBitSet!uint([
    0x00c80, 0x00d00,
]);

immutable(RleBitSet!uint) unicodeInLimbu = RleBitSet!uint([
    0x01900, 0x01950,
]);

immutable(RleBitSet!uint) unicodeInByzantine_Musical_Symbols = RleBitSet!uint([
    0x1d000, 0x1d100,
]);

immutable(RleBitSet!uint) unicodeMc = RleBitSet!uint([
    0x00903, 0x00904,
    0x0093b, 0x0093c,
    0x0093e, 0x00941,
    0x00949, 0x0094d,
    0x0094e, 0x00950,
    0x00982, 0x00984,
    0x009be, 0x009c1,
    0x009c7, 0x009c9,
    0x009cb, 0x009cd,
    0x009d7, 0x009d8,
    0x00a03, 0x00a04,
    0x00a3e, 0x00a41,
    0x00a83, 0x00a84,
    0x00abe, 0x00ac1,
    0x00ac9, 0x00aca,
    0x00acb, 0x00acd,
    0x00b02, 0x00b04,
    0x00b3e, 0x00b3f,
    0x00b40, 0x00b41,
    0x00b47, 0x00b49,
    0x00b4b, 0x00b4d,
    0x00b57, 0x00b58,
    0x00bbe, 0x00bc0,
    0x00bc1, 0x00bc3,
    0x00bc6, 0x00bc9,
    0x00bca, 0x00bcd,
    0x00bd7, 0x00bd8,
    0x00c01, 0x00c04,
    0x00c41, 0x00c45,
    0x00c82, 0x00c84,
    0x00cbe, 0x00cbf,
    0x00cc0, 0x00cc5,
    0x00cc7, 0x00cc9,
    0x00cca, 0x00ccc,
    0x00cd5, 0x00cd7,
    0x00d02, 0x00d04,
    0x00d3e, 0x00d41,
    0x00d46, 0x00d49,
    0x00d4a, 0x00d4d,
    0x00d57, 0x00d58,
    0x00d82, 0x00d84,
    0x00dcf, 0x00dd2,
    0x00dd8, 0x00de0,
    0x00df2, 0x00df4,
    0x00f3e, 0x00f40,
    0x00f7f, 0x00f80,
    0x0102b, 0x0102d,
    0x01031, 0x01032,
    0x01038, 0x01039,
    0x0103b, 0x0103d,
    0x01056, 0x01058,
    0x01062, 0x01065,
    0x01067, 0x0106e,
    0x01083, 0x01085,
    0x01087, 0x0108d,
    0x0108f, 0x01090,
    0x0109a, 0x0109d,
    0x017b6, 0x017b7,
    0x017be, 0x017c6,
    0x017c7, 0x017c9,
    0x01923, 0x01927,
    0x01929, 0x0192c,
    0x01930, 0x01932,
    0x01933, 0x01939,
    0x019b0, 0x019c1,
    0x019c8, 0x019ca,
    0x01a19, 0x01a1c,
    0x01a55, 0x01a56,
    0x01a57, 0x01a58,
    0x01a61, 0x01a62,
    0x01a63, 0x01a65,
    0x01a6d, 0x01a73,
    0x01b04, 0x01b05,
    0x01b35, 0x01b36,
    0x01b3b, 0x01b3c,
    0x01b3d, 0x01b42,
    0x01b43, 0x01b45,
    0x01b82, 0x01b83,
    0x01ba1, 0x01ba2,
    0x01ba6, 0x01ba8,
    0x01baa, 0x01bab,
    0x01be7, 0x01be8,
    0x01bea, 0x01bed,
    0x01bee, 0x01bef,
    0x01bf2, 0x01bf4,
    0x01c24, 0x01c2c,
    0x01c34, 0x01c36,
    0x01ce1, 0x01ce2,
    0x01cf2, 0x01cf3,
    0x0a823, 0x0a825,
    0x0a827, 0x0a828,
    0x0a880, 0x0a882,
    0x0a8b4, 0x0a8c4,
    0x0a952, 0x0a954,
    0x0a983, 0x0a984,
    0x0a9b4, 0x0a9b6,
    0x0a9ba, 0x0a9bc,
    0x0a9bd, 0x0a9c1,
    0x0aa2f, 0x0aa31,
    0x0aa33, 0x0aa35,
    0x0aa4d, 0x0aa4e,
    0x0aa7b, 0x0aa7c,
    0x0abe3, 0x0abe5,
    0x0abe6, 0x0abe8,
    0x0abe9, 0x0abeb,
    0x0abec, 0x0abed,
    0x11000, 0x11001,
    0x11002, 0x11003,
    0x11082, 0x11083,
    0x110b0, 0x110b3,
    0x110b7, 0x110b9,
    0x1d165, 0x1d167,
    0x1d16d, 0x1d173,
]);

immutable(RleBitSet!uint) unicodeLo = RleBitSet!uint([
    0x001bb, 0x001bc,
    0x001c0, 0x001c4,
    0x00294, 0x00295,
    0x005d0, 0x005eb,
    0x005f0, 0x005f3,
    0x00620, 0x00640,
    0x00641, 0x0064b,
    0x0066e, 0x00670,
    0x00671, 0x006d4,
    0x006d5, 0x006d6,
    0x006ee, 0x006f0,
    0x006fa, 0x006fd,
    0x006ff, 0x00700,
    0x00710, 0x00711,
    0x00712, 0x00730,
    0x0074d, 0x007a6,
    0x007b1, 0x007b2,
    0x007ca, 0x007eb,
    0x00800, 0x00816,
    0x00840, 0x00859,
    0x00904, 0x0093a,
    0x0093d, 0x0093e,
    0x00950, 0x00951,
    0x00958, 0x00962,
    0x00972, 0x00978,
    0x00979, 0x00980,
    0x00985, 0x0098d,
    0x0098f, 0x00991,
    0x00993, 0x009a9,
    0x009aa, 0x009b1,
    0x009b2, 0x009b3,
    0x009b6, 0x009ba,
    0x009bd, 0x009be,
    0x009ce, 0x009cf,
    0x009dc, 0x009de,
    0x009df, 0x009e2,
    0x009f0, 0x009f2,
    0x00a05, 0x00a0b,
    0x00a0f, 0x00a11,
    0x00a13, 0x00a29,
    0x00a2a, 0x00a31,
    0x00a32, 0x00a34,
    0x00a35, 0x00a37,
    0x00a38, 0x00a3a,
    0x00a59, 0x00a5d,
    0x00a5e, 0x00a5f,
    0x00a72, 0x00a75,
    0x00a85, 0x00a8e,
    0x00a8f, 0x00a92,
    0x00a93, 0x00aa9,
    0x00aaa, 0x00ab1,
    0x00ab2, 0x00ab4,
    0x00ab5, 0x00aba,
    0x00abd, 0x00abe,
    0x00ad0, 0x00ad1,
    0x00ae0, 0x00ae2,
    0x00b05, 0x00b0d,
    0x00b0f, 0x00b11,
    0x00b13, 0x00b29,
    0x00b2a, 0x00b31,
    0x00b32, 0x00b34,
    0x00b35, 0x00b3a,
    0x00b3d, 0x00b3e,
    0x00b5c, 0x00b5e,
    0x00b5f, 0x00b62,
    0x00b71, 0x00b72,
    0x00b83, 0x00b84,
    0x00b85, 0x00b8b,
    0x00b8e, 0x00b91,
    0x00b92, 0x00b96,
    0x00b99, 0x00b9b,
    0x00b9c, 0x00b9d,
    0x00b9e, 0x00ba0,
    0x00ba3, 0x00ba5,
    0x00ba8, 0x00bab,
    0x00bae, 0x00bba,
    0x00bd0, 0x00bd1,
    0x00c05, 0x00c0d,
    0x00c0e, 0x00c11,
    0x00c12, 0x00c29,
    0x00c2a, 0x00c34,
    0x00c35, 0x00c3a,
    0x00c3d, 0x00c3e,
    0x00c58, 0x00c5a,
    0x00c60, 0x00c62,
    0x00c85, 0x00c8d,
    0x00c8e, 0x00c91,
    0x00c92, 0x00ca9,
    0x00caa, 0x00cb4,
    0x00cb5, 0x00cba,
    0x00cbd, 0x00cbe,
    0x00cde, 0x00cdf,
    0x00ce0, 0x00ce2,
    0x00cf1, 0x00cf3,
    0x00d05, 0x00d0d,
    0x00d0e, 0x00d11,
    0x00d12, 0x00d3b,
    0x00d3d, 0x00d3e,
    0x00d4e, 0x00d4f,
    0x00d60, 0x00d62,
    0x00d7a, 0x00d80,
    0x00d85, 0x00d97,
    0x00d9a, 0x00db2,
    0x00db3, 0x00dbc,
    0x00dbd, 0x00dbe,
    0x00dc0, 0x00dc7,
    0x00e01, 0x00e31,
    0x00e32, 0x00e34,
    0x00e40, 0x00e46,
    0x00e81, 0x00e83,
    0x00e84, 0x00e85,
    0x00e87, 0x00e89,
    0x00e8a, 0x00e8b,
    0x00e8d, 0x00e8e,
    0x00e94, 0x00e98,
    0x00e99, 0x00ea0,
    0x00ea1, 0x00ea4,
    0x00ea5, 0x00ea6,
    0x00ea7, 0x00ea8,
    0x00eaa, 0x00eac,
    0x00ead, 0x00eb1,
    0x00eb2, 0x00eb4,
    0x00ebd, 0x00ebe,
    0x00ec0, 0x00ec5,
    0x00edc, 0x00ede,
    0x00f00, 0x00f01,
    0x00f40, 0x00f48,
    0x00f49, 0x00f6d,
    0x00f88, 0x00f8d,
    0x01000, 0x0102b,
    0x0103f, 0x01040,
    0x01050, 0x01056,
    0x0105a, 0x0105e,
    0x01061, 0x01062,
    0x01065, 0x01067,
    0x0106e, 0x01071,
    0x01075, 0x01082,
    0x0108e, 0x0108f,
    0x010d0, 0x010fb,
    0x01100, 0x01249,
    0x0124a, 0x0124e,
    0x01250, 0x01257,
    0x01258, 0x01259,
    0x0125a, 0x0125e,
    0x01260, 0x01289,
    0x0128a, 0x0128e,
    0x01290, 0x012b1,
    0x012b2, 0x012b6,
    0x012b8, 0x012bf,
    0x012c0, 0x012c1,
    0x012c2, 0x012c6,
    0x012c8, 0x012d7,
    0x012d8, 0x01311,
    0x01312, 0x01316,
    0x01318, 0x0135b,
    0x01380, 0x01390,
    0x013a0, 0x013f5,
    0x01401, 0x0166d,
    0x0166f, 0x01680,
    0x01681, 0x0169b,
    0x016a0, 0x016eb,
    0x01700, 0x0170d,
    0x0170e, 0x01712,
    0x01720, 0x01732,
    0x01740, 0x01752,
    0x01760, 0x0176d,
    0x0176e, 0x01771,
    0x01780, 0x017b4,
    0x017dc, 0x017dd,
    0x01820, 0x01843,
    0x01844, 0x01878,
    0x01880, 0x018a9,
    0x018aa, 0x018ab,
    0x018b0, 0x018f6,
    0x01900, 0x0191d,
    0x01950, 0x0196e,
    0x01970, 0x01975,
    0x01980, 0x019ac,
    0x019c1, 0x019c8,
    0x01a00, 0x01a17,
    0x01a20, 0x01a55,
    0x01b05, 0x01b34,
    0x01b45, 0x01b4c,
    0x01b83, 0x01ba1,
    0x01bae, 0x01bb0,
    0x01bc0, 0x01be6,
    0x01c00, 0x01c24,
    0x01c4d, 0x01c50,
    0x01c5a, 0x01c78,
    0x01ce9, 0x01ced,
    0x01cee, 0x01cf2,
    0x02135, 0x02139,
    0x02d30, 0x02d66,
    0x02d80, 0x02d97,
    0x02da0, 0x02da7,
    0x02da8, 0x02daf,
    0x02db0, 0x02db7,
    0x02db8, 0x02dbf,
    0x02dc0, 0x02dc7,
    0x02dc8, 0x02dcf,
    0x02dd0, 0x02dd7,
    0x02dd8, 0x02ddf,
    0x03006, 0x03007,
    0x0303c, 0x0303d,
    0x03041, 0x03097,
    0x0309f, 0x030a0,
    0x030a1, 0x030fb,
    0x030ff, 0x03100,
    0x03105, 0x0312e,
    0x03131, 0x0318f,
    0x031a0, 0x031bb,
    0x031f0, 0x03200,
    0x03400, 0x04db6,
    0x04e00, 0x09fcc,
    0x0a000, 0x0a015,
    0x0a016, 0x0a48d,
    0x0a4d0, 0x0a4f8,
    0x0a500, 0x0a60c,
    0x0a610, 0x0a620,
    0x0a62a, 0x0a62c,
    0x0a66e, 0x0a66f,
    0x0a6a0, 0x0a6e6,
    0x0a7fb, 0x0a802,
    0x0a803, 0x0a806,
    0x0a807, 0x0a80b,
    0x0a80c, 0x0a823,
    0x0a840, 0x0a874,
    0x0a882, 0x0a8b4,
    0x0a8f2, 0x0a8f8,
    0x0a8fb, 0x0a8fc,
    0x0a90a, 0x0a926,
    0x0a930, 0x0a947,
    0x0a960, 0x0a97d,
    0x0a984, 0x0a9b3,
    0x0aa00, 0x0aa29,
    0x0aa40, 0x0aa43,
    0x0aa44, 0x0aa4c,
    0x0aa60, 0x0aa70,
    0x0aa71, 0x0aa77,
    0x0aa7a, 0x0aa7b,
    0x0aa80, 0x0aab0,
    0x0aab1, 0x0aab2,
    0x0aab5, 0x0aab7,
    0x0aab9, 0x0aabe,
    0x0aac0, 0x0aac1,
    0x0aac2, 0x0aac3,
    0x0aadb, 0x0aadd,
    0x0ab01, 0x0ab07,
    0x0ab09, 0x0ab0f,
    0x0ab11, 0x0ab17,
    0x0ab20, 0x0ab27,
    0x0ab28, 0x0ab2f,
    0x0abc0, 0x0abe3,
    0x0ac00, 0x0d7a4,
    0x0d7b0, 0x0d7c7,
    0x0d7cb, 0x0d7fc,
    0x0f900, 0x0fa2e,
    0x0fa30, 0x0fa6e,
    0x0fa70, 0x0fada,
    0x0fb1d, 0x0fb1e,
    0x0fb1f, 0x0fb29,
    0x0fb2a, 0x0fb37,
    0x0fb38, 0x0fb3d,
    0x0fb3e, 0x0fb3f,
    0x0fb40, 0x0fb42,
    0x0fb43, 0x0fb45,
    0x0fb46, 0x0fbb2,
    0x0fbd3, 0x0fd3e,
    0x0fd50, 0x0fd90,
    0x0fd92, 0x0fdc8,
    0x0fdf0, 0x0fdfc,
    0x0fe70, 0x0fe75,
    0x0fe76, 0x0fefd,
    0x0ff66, 0x0ff70,
    0x0ff71, 0x0ff9e,
    0x0ffa0, 0x0ffbf,
    0x0ffc2, 0x0ffc8,
    0x0ffca, 0x0ffd0,
    0x0ffd2, 0x0ffd8,
    0x0ffda, 0x0ffdd,
    0x10000, 0x1000c,
    0x1000d, 0x10027,
    0x10028, 0x1003b,
    0x1003c, 0x1003e,
    0x1003f, 0x1004e,
    0x10050, 0x1005e,
    0x10080, 0x100fb,
    0x10280, 0x1029d,
    0x102a0, 0x102d1,
    0x10300, 0x1031f,
    0x10330, 0x10341,
    0x10342, 0x1034a,
    0x10380, 0x1039e,
    0x103a0, 0x103c4,
    0x103c8, 0x103d0,
    0x10450, 0x1049e,
    0x10800, 0x10806,
    0x10808, 0x10809,
    0x1080a, 0x10836,
    0x10837, 0x10839,
    0x1083c, 0x1083d,
    0x1083f, 0x10856,
    0x10900, 0x10916,
    0x10920, 0x1093a,
    0x10a00, 0x10a01,
    0x10a10, 0x10a14,
    0x10a15, 0x10a18,
    0x10a19, 0x10a34,
    0x10a60, 0x10a7d,
    0x10b00, 0x10b36,
    0x10b40, 0x10b56,
    0x10b60, 0x10b73,
    0x10c00, 0x10c49,
    0x11003, 0x11038,
    0x11083, 0x110b0,
    0x12000, 0x1236f,
    0x13000, 0x1342f,
    0x16800, 0x16a39,
    0x1b000, 0x1b002,
    0x20000, 0x2a6d7,
    0x2a700, 0x2b735,
    0x2b740, 0x2b81e,
    0x2f800, 0x2fa1e,
]);

immutable(RleBitSet!uint) unicodeMe = RleBitSet!uint([
    0x00488, 0x0048a,
    0x020dd, 0x020e1,
    0x020e2, 0x020e5,
    0x0a670, 0x0a673,
]);

immutable(RleBitSet!uint) unicodeInMeetei_Mayek = RleBitSet!uint([
    0x0abc0, 0x0ac00,
]);

immutable(RleBitSet!uint) unicodeLt = RleBitSet!uint([
    0x001c5, 0x001c6,
    0x001c8, 0x001c9,
    0x001cb, 0x001cc,
    0x001f2, 0x001f3,
    0x01f88, 0x01f90,
    0x01f98, 0x01fa0,
    0x01fa8, 0x01fb0,
    0x01fbc, 0x01fbd,
    0x01fcc, 0x01fcd,
    0x01ffc, 0x01ffd,
]);

immutable(RleBitSet!uint) unicodeInKhmer = RleBitSet!uint([
    0x01780, 0x01800,
]);

immutable(RleBitSet!uint) unicodeInEthiopic_Extended = RleBitSet!uint([
    0x02d80, 0x02de0,
]);

immutable(RleBitSet!uint) unicodeLu = RleBitSet!uint([
    0x00041, 0x0005b,
    0x000c0, 0x000d7,
    0x000d8, 0x000df,
    0x00100, 0x00101,
    0x00102, 0x00103,
    0x00104, 0x00105,
    0x00106, 0x00107,
    0x00108, 0x00109,
    0x0010a, 0x0010b,
    0x0010c, 0x0010d,
    0x0010e, 0x0010f,
    0x00110, 0x00111,
    0x00112, 0x00113,
    0x00114, 0x00115,
    0x00116, 0x00117,
    0x00118, 0x00119,
    0x0011a, 0x0011b,
    0x0011c, 0x0011d,
    0x0011e, 0x0011f,
    0x00120, 0x00121,
    0x00122, 0x00123,
    0x00124, 0x00125,
    0x00126, 0x00127,
    0x00128, 0x00129,
    0x0012a, 0x0012b,
    0x0012c, 0x0012d,
    0x0012e, 0x0012f,
    0x00130, 0x00131,
    0x00132, 0x00133,
    0x00134, 0x00135,
    0x00136, 0x00137,
    0x00139, 0x0013a,
    0x0013b, 0x0013c,
    0x0013d, 0x0013e,
    0x0013f, 0x00140,
    0x00141, 0x00142,
    0x00143, 0x00144,
    0x00145, 0x00146,
    0x00147, 0x00148,
    0x0014a, 0x0014b,
    0x0014c, 0x0014d,
    0x0014e, 0x0014f,
    0x00150, 0x00151,
    0x00152, 0x00153,
    0x00154, 0x00155,
    0x00156, 0x00157,
    0x00158, 0x00159,
    0x0015a, 0x0015b,
    0x0015c, 0x0015d,
    0x0015e, 0x0015f,
    0x00160, 0x00161,
    0x00162, 0x00163,
    0x00164, 0x00165,
    0x00166, 0x00167,
    0x00168, 0x00169,
    0x0016a, 0x0016b,
    0x0016c, 0x0016d,
    0x0016e, 0x0016f,
    0x00170, 0x00171,
    0x00172, 0x00173,
    0x00174, 0x00175,
    0x00176, 0x00177,
    0x00178, 0x0017a,
    0x0017b, 0x0017c,
    0x0017d, 0x0017e,
    0x00181, 0x00183,
    0x00184, 0x00185,
    0x00186, 0x00188,
    0x00189, 0x0018c,
    0x0018e, 0x00192,
    0x00193, 0x00195,
    0x00196, 0x00199,
    0x0019c, 0x0019e,
    0x0019f, 0x001a1,
    0x001a2, 0x001a3,
    0x001a4, 0x001a5,
    0x001a6, 0x001a8,
    0x001a9, 0x001aa,
    0x001ac, 0x001ad,
    0x001ae, 0x001b0,
    0x001b1, 0x001b4,
    0x001b5, 0x001b6,
    0x001b7, 0x001b9,
    0x001bc, 0x001bd,
    0x001c4, 0x001c5,
    0x001c7, 0x001c8,
    0x001ca, 0x001cb,
    0x001cd, 0x001ce,
    0x001cf, 0x001d0,
    0x001d1, 0x001d2,
    0x001d3, 0x001d4,
    0x001d5, 0x001d6,
    0x001d7, 0x001d8,
    0x001d9, 0x001da,
    0x001db, 0x001dc,
    0x001de, 0x001df,
    0x001e0, 0x001e1,
    0x001e2, 0x001e3,
    0x001e4, 0x001e5,
    0x001e6, 0x001e7,
    0x001e8, 0x001e9,
    0x001ea, 0x001eb,
    0x001ec, 0x001ed,
    0x001ee, 0x001ef,
    0x001f1, 0x001f2,
    0x001f4, 0x001f5,
    0x001f6, 0x001f9,
    0x001fa, 0x001fb,
    0x001fc, 0x001fd,
    0x001fe, 0x001ff,
    0x00200, 0x00201,
    0x00202, 0x00203,
    0x00204, 0x00205,
    0x00206, 0x00207,
    0x00208, 0x00209,
    0x0020a, 0x0020b,
    0x0020c, 0x0020d,
    0x0020e, 0x0020f,
    0x00210, 0x00211,
    0x00212, 0x00213,
    0x00214, 0x00215,
    0x00216, 0x00217,
    0x00218, 0x00219,
    0x0021a, 0x0021b,
    0x0021c, 0x0021d,
    0x0021e, 0x0021f,
    0x00220, 0x00221,
    0x00222, 0x00223,
    0x00224, 0x00225,
    0x00226, 0x00227,
    0x00228, 0x00229,
    0x0022a, 0x0022b,
    0x0022c, 0x0022d,
    0x0022e, 0x0022f,
    0x00230, 0x00231,
    0x00232, 0x00233,
    0x0023a, 0x0023c,
    0x0023d, 0x0023f,
    0x00241, 0x00242,
    0x00243, 0x00247,
    0x00248, 0x00249,
    0x0024a, 0x0024b,
    0x0024c, 0x0024d,
    0x0024e, 0x0024f,
    0x00370, 0x00371,
    0x00372, 0x00373,
    0x00376, 0x00377,
    0x00386, 0x00387,
    0x00388, 0x0038b,
    0x0038c, 0x0038d,
    0x0038e, 0x00390,
    0x00391, 0x003a2,
    0x003a3, 0x003ac,
    0x003cf, 0x003d0,
    0x003d2, 0x003d5,
    0x003d8, 0x003d9,
    0x003da, 0x003db,
    0x003dc, 0x003dd,
    0x003de, 0x003df,
    0x003e0, 0x003e1,
    0x003e2, 0x003e3,
    0x003e4, 0x003e5,
    0x003e6, 0x003e7,
    0x003e8, 0x003e9,
    0x003ea, 0x003eb,
    0x003ec, 0x003ed,
    0x003ee, 0x003ef,
    0x003f4, 0x003f5,
    0x003f7, 0x003f8,
    0x003f9, 0x003fb,
    0x003fd, 0x00430,
    0x00460, 0x00461,
    0x00462, 0x00463,
    0x00464, 0x00465,
    0x00466, 0x00467,
    0x00468, 0x00469,
    0x0046a, 0x0046b,
    0x0046c, 0x0046d,
    0x0046e, 0x0046f,
    0x00470, 0x00471,
    0x00472, 0x00473,
    0x00474, 0x00475,
    0x00476, 0x00477,
    0x00478, 0x00479,
    0x0047a, 0x0047b,
    0x0047c, 0x0047d,
    0x0047e, 0x0047f,
    0x00480, 0x00481,
    0x0048a, 0x0048b,
    0x0048c, 0x0048d,
    0x0048e, 0x0048f,
    0x00490, 0x00491,
    0x00492, 0x00493,
    0x00494, 0x00495,
    0x00496, 0x00497,
    0x00498, 0x00499,
    0x0049a, 0x0049b,
    0x0049c, 0x0049d,
    0x0049e, 0x0049f,
    0x004a0, 0x004a1,
    0x004a2, 0x004a3,
    0x004a4, 0x004a5,
    0x004a6, 0x004a7,
    0x004a8, 0x004a9,
    0x004aa, 0x004ab,
    0x004ac, 0x004ad,
    0x004ae, 0x004af,
    0x004b0, 0x004b1,
    0x004b2, 0x004b3,
    0x004b4, 0x004b5,
    0x004b6, 0x004b7,
    0x004b8, 0x004b9,
    0x004ba, 0x004bb,
    0x004bc, 0x004bd,
    0x004be, 0x004bf,
    0x004c0, 0x004c2,
    0x004c3, 0x004c4,
    0x004c5, 0x004c6,
    0x004c7, 0x004c8,
    0x004c9, 0x004ca,
    0x004cb, 0x004cc,
    0x004cd, 0x004ce,
    0x004d0, 0x004d1,
    0x004d2, 0x004d3,
    0x004d4, 0x004d5,
    0x004d6, 0x004d7,
    0x004d8, 0x004d9,
    0x004da, 0x004db,
    0x004dc, 0x004dd,
    0x004de, 0x004df,
    0x004e0, 0x004e1,
    0x004e2, 0x004e3,
    0x004e4, 0x004e5,
    0x004e6, 0x004e7,
    0x004e8, 0x004e9,
    0x004ea, 0x004eb,
    0x004ec, 0x004ed,
    0x004ee, 0x004ef,
    0x004f0, 0x004f1,
    0x004f2, 0x004f3,
    0x004f4, 0x004f5,
    0x004f6, 0x004f7,
    0x004f8, 0x004f9,
    0x004fa, 0x004fb,
    0x004fc, 0x004fd,
    0x004fe, 0x004ff,
    0x00500, 0x00501,
    0x00502, 0x00503,
    0x00504, 0x00505,
    0x00506, 0x00507,
    0x00508, 0x00509,
    0x0050a, 0x0050b,
    0x0050c, 0x0050d,
    0x0050e, 0x0050f,
    0x00510, 0x00511,
    0x00512, 0x00513,
    0x00514, 0x00515,
    0x00516, 0x00517,
    0x00518, 0x00519,
    0x0051a, 0x0051b,
    0x0051c, 0x0051d,
    0x0051e, 0x0051f,
    0x00520, 0x00521,
    0x00522, 0x00523,
    0x00524, 0x00525,
    0x00526, 0x00527,
    0x00531, 0x00557,
    0x010a0, 0x010c6,
    0x01e00, 0x01e01,
    0x01e02, 0x01e03,
    0x01e04, 0x01e05,
    0x01e06, 0x01e07,
    0x01e08, 0x01e09,
    0x01e0a, 0x01e0b,
    0x01e0c, 0x01e0d,
    0x01e0e, 0x01e0f,
    0x01e10, 0x01e11,
    0x01e12, 0x01e13,
    0x01e14, 0x01e15,
    0x01e16, 0x01e17,
    0x01e18, 0x01e19,
    0x01e1a, 0x01e1b,
    0x01e1c, 0x01e1d,
    0x01e1e, 0x01e1f,
    0x01e20, 0x01e21,
    0x01e22, 0x01e23,
    0x01e24, 0x01e25,
    0x01e26, 0x01e27,
    0x01e28, 0x01e29,
    0x01e2a, 0x01e2b,
    0x01e2c, 0x01e2d,
    0x01e2e, 0x01e2f,
    0x01e30, 0x01e31,
    0x01e32, 0x01e33,
    0x01e34, 0x01e35,
    0x01e36, 0x01e37,
    0x01e38, 0x01e39,
    0x01e3a, 0x01e3b,
    0x01e3c, 0x01e3d,
    0x01e3e, 0x01e3f,
    0x01e40, 0x01e41,
    0x01e42, 0x01e43,
    0x01e44, 0x01e45,
    0x01e46, 0x01e47,
    0x01e48, 0x01e49,
    0x01e4a, 0x01e4b,
    0x01e4c, 0x01e4d,
    0x01e4e, 0x01e4f,
    0x01e50, 0x01e51,
    0x01e52, 0x01e53,
    0x01e54, 0x01e55,
    0x01e56, 0x01e57,
    0x01e58, 0x01e59,
    0x01e5a, 0x01e5b,
    0x01e5c, 0x01e5d,
    0x01e5e, 0x01e5f,
    0x01e60, 0x01e61,
    0x01e62, 0x01e63,
    0x01e64, 0x01e65,
    0x01e66, 0x01e67,
    0x01e68, 0x01e69,
    0x01e6a, 0x01e6b,
    0x01e6c, 0x01e6d,
    0x01e6e, 0x01e6f,
    0x01e70, 0x01e71,
    0x01e72, 0x01e73,
    0x01e74, 0x01e75,
    0x01e76, 0x01e77,
    0x01e78, 0x01e79,
    0x01e7a, 0x01e7b,
    0x01e7c, 0x01e7d,
    0x01e7e, 0x01e7f,
    0x01e80, 0x01e81,
    0x01e82, 0x01e83,
    0x01e84, 0x01e85,
    0x01e86, 0x01e87,
    0x01e88, 0x01e89,
    0x01e8a, 0x01e8b,
    0x01e8c, 0x01e8d,
    0x01e8e, 0x01e8f,
    0x01e90, 0x01e91,
    0x01e92, 0x01e93,
    0x01e94, 0x01e95,
    0x01e9e, 0x01e9f,
    0x01ea0, 0x01ea1,
    0x01ea2, 0x01ea3,
    0x01ea4, 0x01ea5,
    0x01ea6, 0x01ea7,
    0x01ea8, 0x01ea9,
    0x01eaa, 0x01eab,
    0x01eac, 0x01ead,
    0x01eae, 0x01eaf,
    0x01eb0, 0x01eb1,
    0x01eb2, 0x01eb3,
    0x01eb4, 0x01eb5,
    0x01eb6, 0x01eb7,
    0x01eb8, 0x01eb9,
    0x01eba, 0x01ebb,
    0x01ebc, 0x01ebd,
    0x01ebe, 0x01ebf,
    0x01ec0, 0x01ec1,
    0x01ec2, 0x01ec3,
    0x01ec4, 0x01ec5,
    0x01ec6, 0x01ec7,
    0x01ec8, 0x01ec9,
    0x01eca, 0x01ecb,
    0x01ecc, 0x01ecd,
    0x01ece, 0x01ecf,
    0x01ed0, 0x01ed1,
    0x01ed2, 0x01ed3,
    0x01ed4, 0x01ed5,
    0x01ed6, 0x01ed7,
    0x01ed8, 0x01ed9,
    0x01eda, 0x01edb,
    0x01edc, 0x01edd,
    0x01ede, 0x01edf,
    0x01ee0, 0x01ee1,
    0x01ee2, 0x01ee3,
    0x01ee4, 0x01ee5,
    0x01ee6, 0x01ee7,
    0x01ee8, 0x01ee9,
    0x01eea, 0x01eeb,
    0x01eec, 0x01eed,
    0x01eee, 0x01eef,
    0x01ef0, 0x01ef1,
    0x01ef2, 0x01ef3,
    0x01ef4, 0x01ef5,
    0x01ef6, 0x01ef7,
    0x01ef8, 0x01ef9,
    0x01efa, 0x01efb,
    0x01efc, 0x01efd,
    0x01efe, 0x01eff,
    0x01f08, 0x01f10,
    0x01f18, 0x01f1e,
    0x01f28, 0x01f30,
    0x01f38, 0x01f40,
    0x01f48, 0x01f4e,
    0x01f59, 0x01f5a,
    0x01f5b, 0x01f5c,
    0x01f5d, 0x01f5e,
    0x01f5f, 0x01f60,
    0x01f68, 0x01f70,
    0x01fb8, 0x01fbc,
    0x01fc8, 0x01fcc,
    0x01fd8, 0x01fdc,
    0x01fe8, 0x01fed,
    0x01ff8, 0x01ffc,
    0x02102, 0x02103,
    0x02107, 0x02108,
    0x0210b, 0x0210e,
    0x02110, 0x02113,
    0x02115, 0x02116,
    0x02119, 0x0211e,
    0x02124, 0x02125,
    0x02126, 0x02127,
    0x02128, 0x02129,
    0x0212a, 0x0212e,
    0x02130, 0x02134,
    0x0213e, 0x02140,
    0x02145, 0x02146,
    0x02183, 0x02184,
    0x02c00, 0x02c2f,
    0x02c60, 0x02c61,
    0x02c62, 0x02c65,
    0x02c67, 0x02c68,
    0x02c69, 0x02c6a,
    0x02c6b, 0x02c6c,
    0x02c6d, 0x02c71,
    0x02c72, 0x02c73,
    0x02c75, 0x02c76,
    0x02c7e, 0x02c81,
    0x02c82, 0x02c83,
    0x02c84, 0x02c85,
    0x02c86, 0x02c87,
    0x02c88, 0x02c89,
    0x02c8a, 0x02c8b,
    0x02c8c, 0x02c8d,
    0x02c8e, 0x02c8f,
    0x02c90, 0x02c91,
    0x02c92, 0x02c93,
    0x02c94, 0x02c95,
    0x02c96, 0x02c97,
    0x02c98, 0x02c99,
    0x02c9a, 0x02c9b,
    0x02c9c, 0x02c9d,
    0x02c9e, 0x02c9f,
    0x02ca0, 0x02ca1,
    0x02ca2, 0x02ca3,
    0x02ca4, 0x02ca5,
    0x02ca6, 0x02ca7,
    0x02ca8, 0x02ca9,
    0x02caa, 0x02cab,
    0x02cac, 0x02cad,
    0x02cae, 0x02caf,
    0x02cb0, 0x02cb1,
    0x02cb2, 0x02cb3,
    0x02cb4, 0x02cb5,
    0x02cb6, 0x02cb7,
    0x02cb8, 0x02cb9,
    0x02cba, 0x02cbb,
    0x02cbc, 0x02cbd,
    0x02cbe, 0x02cbf,
    0x02cc0, 0x02cc1,
    0x02cc2, 0x02cc3,
    0x02cc4, 0x02cc5,
    0x02cc6, 0x02cc7,
    0x02cc8, 0x02cc9,
    0x02cca, 0x02ccb,
    0x02ccc, 0x02ccd,
    0x02cce, 0x02ccf,
    0x02cd0, 0x02cd1,
    0x02cd2, 0x02cd3,
    0x02cd4, 0x02cd5,
    0x02cd6, 0x02cd7,
    0x02cd8, 0x02cd9,
    0x02cda, 0x02cdb,
    0x02cdc, 0x02cdd,
    0x02cde, 0x02cdf,
    0x02ce0, 0x02ce1,
    0x02ce2, 0x02ce3,
    0x02ceb, 0x02cec,
    0x02ced, 0x02cee,
    0x0a640, 0x0a641,
    0x0a642, 0x0a643,
    0x0a644, 0x0a645,
    0x0a646, 0x0a647,
    0x0a648, 0x0a649,
    0x0a64a, 0x0a64b,
    0x0a64c, 0x0a64d,
    0x0a64e, 0x0a64f,
    0x0a650, 0x0a651,
    0x0a652, 0x0a653,
    0x0a654, 0x0a655,
    0x0a656, 0x0a657,
    0x0a658, 0x0a659,
    0x0a65a, 0x0a65b,
    0x0a65c, 0x0a65d,
    0x0a65e, 0x0a65f,
    0x0a660, 0x0a661,
    0x0a662, 0x0a663,
    0x0a664, 0x0a665,
    0x0a666, 0x0a667,
    0x0a668, 0x0a669,
    0x0a66a, 0x0a66b,
    0x0a66c, 0x0a66d,
    0x0a680, 0x0a681,
    0x0a682, 0x0a683,
    0x0a684, 0x0a685,
    0x0a686, 0x0a687,
    0x0a688, 0x0a689,
    0x0a68a, 0x0a68b,
    0x0a68c, 0x0a68d,
    0x0a68e, 0x0a68f,
    0x0a690, 0x0a691,
    0x0a692, 0x0a693,
    0x0a694, 0x0a695,
    0x0a696, 0x0a697,
    0x0a722, 0x0a723,
    0x0a724, 0x0a725,
    0x0a726, 0x0a727,
    0x0a728, 0x0a729,
    0x0a72a, 0x0a72b,
    0x0a72c, 0x0a72d,
    0x0a72e, 0x0a72f,
    0x0a732, 0x0a733,
    0x0a734, 0x0a735,
    0x0a736, 0x0a737,
    0x0a738, 0x0a739,
    0x0a73a, 0x0a73b,
    0x0a73c, 0x0a73d,
    0x0a73e, 0x0a73f,
    0x0a740, 0x0a741,
    0x0a742, 0x0a743,
    0x0a744, 0x0a745,
    0x0a746, 0x0a747,
    0x0a748, 0x0a749,
    0x0a74a, 0x0a74b,
    0x0a74c, 0x0a74d,
    0x0a74e, 0x0a74f,
    0x0a750, 0x0a751,
    0x0a752, 0x0a753,
    0x0a754, 0x0a755,
    0x0a756, 0x0a757,
    0x0a758, 0x0a759,
    0x0a75a, 0x0a75b,
    0x0a75c, 0x0a75d,
    0x0a75e, 0x0a75f,
    0x0a760, 0x0a761,
    0x0a762, 0x0a763,
    0x0a764, 0x0a765,
    0x0a766, 0x0a767,
    0x0a768, 0x0a769,
    0x0a76a, 0x0a76b,
    0x0a76c, 0x0a76d,
    0x0a76e, 0x0a76f,
    0x0a779, 0x0a77a,
    0x0a77b, 0x0a77c,
    0x0a77d, 0x0a77f,
    0x0a780, 0x0a781,
    0x0a782, 0x0a783,
    0x0a784, 0x0a785,
    0x0a786, 0x0a787,
    0x0a78b, 0x0a78c,
    0x0a78d, 0x0a78e,
    0x0a790, 0x0a791,
    0x0a7a0, 0x0a7a1,
    0x0a7a2, 0x0a7a3,
    0x0a7a4, 0x0a7a5,
    0x0a7a6, 0x0a7a7,
    0x0a7a8, 0x0a7a9,
    0x0ff21, 0x0ff3b,
    0x10400, 0x10428,
    0x1d400, 0x1d41a,
    0x1d434, 0x1d44e,
    0x1d468, 0x1d482,
    0x1d49c, 0x1d49d,
    0x1d49e, 0x1d4a0,
    0x1d4a2, 0x1d4a3,
    0x1d4a5, 0x1d4a7,
    0x1d4a9, 0x1d4ad,
    0x1d4ae, 0x1d4b6,
    0x1d4d0, 0x1d4ea,
    0x1d504, 0x1d506,
    0x1d507, 0x1d50b,
    0x1d50d, 0x1d515,
    0x1d516, 0x1d51d,
    0x1d538, 0x1d53a,
    0x1d53b, 0x1d53f,
    0x1d540, 0x1d545,
    0x1d546, 0x1d547,
    0x1d54a, 0x1d551,
    0x1d56c, 0x1d586,
    0x1d5a0, 0x1d5ba,
    0x1d5d4, 0x1d5ee,
    0x1d608, 0x1d622,
    0x1d63c, 0x1d656,
    0x1d670, 0x1d68a,
    0x1d6a8, 0x1d6c1,
    0x1d6e2, 0x1d6fb,
    0x1d71c, 0x1d735,
    0x1d756, 0x1d76f,
    0x1d790, 0x1d7a9,
    0x1d7ca, 0x1d7cb,
]);

immutable(RleBitSet!uint) unicodeCarian = RleBitSet!uint([
    0x102a0, 0x102d1,
]);

immutable(RleBitSet!uint) unicodeInKaithi = RleBitSet!uint([
    0x11080, 0x110d0,
]);

immutable(RleBitSet!uint) unicodeInVedic_Extensions = RleBitSet!uint([
    0x01cd0, 0x01d00,
]);

immutable(RleBitSet!uint) unicodeMn = RleBitSet!uint([
    0x00300, 0x00370,
    0x00483, 0x00488,
    0x00591, 0x005be,
    0x005bf, 0x005c0,
    0x005c1, 0x005c3,
    0x005c4, 0x005c6,
    0x005c7, 0x005c8,
    0x00610, 0x0061b,
    0x0064b, 0x00660,
    0x00670, 0x00671,
    0x006d6, 0x006dd,
    0x006df, 0x006e5,
    0x006e7, 0x006e9,
    0x006ea, 0x006ee,
    0x00711, 0x00712,
    0x00730, 0x0074b,
    0x007a6, 0x007b1,
    0x007eb, 0x007f4,
    0x00816, 0x0081a,
    0x0081b, 0x00824,
    0x00825, 0x00828,
    0x00829, 0x0082e,
    0x00859, 0x0085c,
    0x00900, 0x00903,
    0x0093a, 0x0093b,
    0x0093c, 0x0093d,
    0x00941, 0x00949,
    0x0094d, 0x0094e,
    0x00951, 0x00958,
    0x00962, 0x00964,
    0x00981, 0x00982,
    0x009bc, 0x009bd,
    0x009c1, 0x009c5,
    0x009cd, 0x009ce,
    0x009e2, 0x009e4,
    0x00a01, 0x00a03,
    0x00a3c, 0x00a3d,
    0x00a41, 0x00a43,
    0x00a47, 0x00a49,
    0x00a4b, 0x00a4e,
    0x00a51, 0x00a52,
    0x00a70, 0x00a72,
    0x00a75, 0x00a76,
    0x00a81, 0x00a83,
    0x00abc, 0x00abd,
    0x00ac1, 0x00ac6,
    0x00ac7, 0x00ac9,
    0x00acd, 0x00ace,
    0x00ae2, 0x00ae4,
    0x00b01, 0x00b02,
    0x00b3c, 0x00b3d,
    0x00b3f, 0x00b40,
    0x00b41, 0x00b45,
    0x00b4d, 0x00b4e,
    0x00b56, 0x00b57,
    0x00b62, 0x00b64,
    0x00b82, 0x00b83,
    0x00bc0, 0x00bc1,
    0x00bcd, 0x00bce,
    0x00c3e, 0x00c41,
    0x00c46, 0x00c49,
    0x00c4a, 0x00c4e,
    0x00c55, 0x00c57,
    0x00c62, 0x00c64,
    0x00cbc, 0x00cbd,
    0x00cbf, 0x00cc0,
    0x00cc6, 0x00cc7,
    0x00ccc, 0x00cce,
    0x00ce2, 0x00ce4,
    0x00d41, 0x00d45,
    0x00d4d, 0x00d4e,
    0x00d62, 0x00d64,
    0x00dca, 0x00dcb,
    0x00dd2, 0x00dd5,
    0x00dd6, 0x00dd7,
    0x00e31, 0x00e32,
    0x00e34, 0x00e3b,
    0x00e47, 0x00e4f,
    0x00eb1, 0x00eb2,
    0x00eb4, 0x00eba,
    0x00ebb, 0x00ebd,
    0x00ec8, 0x00ece,
    0x00f18, 0x00f1a,
    0x00f35, 0x00f36,
    0x00f37, 0x00f38,
    0x00f39, 0x00f3a,
    0x00f71, 0x00f7f,
    0x00f80, 0x00f85,
    0x00f86, 0x00f88,
    0x00f8d, 0x00f98,
    0x00f99, 0x00fbd,
    0x00fc6, 0x00fc7,
    0x0102d, 0x01031,
    0x01032, 0x01038,
    0x01039, 0x0103b,
    0x0103d, 0x0103f,
    0x01058, 0x0105a,
    0x0105e, 0x01061,
    0x01071, 0x01075,
    0x01082, 0x01083,
    0x01085, 0x01087,
    0x0108d, 0x0108e,
    0x0109d, 0x0109e,
    0x0135d, 0x01360,
    0x01712, 0x01715,
    0x01732, 0x01735,
    0x01752, 0x01754,
    0x01772, 0x01774,
    0x017b7, 0x017be,
    0x017c6, 0x017c7,
    0x017c9, 0x017d4,
    0x017dd, 0x017de,
    0x0180b, 0x0180e,
    0x018a9, 0x018aa,
    0x01920, 0x01923,
    0x01927, 0x01929,
    0x01932, 0x01933,
    0x01939, 0x0193c,
    0x01a17, 0x01a19,
    0x01a56, 0x01a57,
    0x01a58, 0x01a5f,
    0x01a60, 0x01a61,
    0x01a62, 0x01a63,
    0x01a65, 0x01a6d,
    0x01a73, 0x01a7d,
    0x01a7f, 0x01a80,
    0x01b00, 0x01b04,
    0x01b34, 0x01b35,
    0x01b36, 0x01b3b,
    0x01b3c, 0x01b3d,
    0x01b42, 0x01b43,
    0x01b6b, 0x01b74,
    0x01b80, 0x01b82,
    0x01ba2, 0x01ba6,
    0x01ba8, 0x01baa,
    0x01be6, 0x01be7,
    0x01be8, 0x01bea,
    0x01bed, 0x01bee,
    0x01bef, 0x01bf2,
    0x01c2c, 0x01c34,
    0x01c36, 0x01c38,
    0x01cd0, 0x01cd3,
    0x01cd4, 0x01ce1,
    0x01ce2, 0x01ce9,
    0x01ced, 0x01cee,
    0x01dc0, 0x01de7,
    0x01dfc, 0x01e00,
    0x020d0, 0x020dd,
    0x020e1, 0x020e2,
    0x020e5, 0x020f1,
    0x02cef, 0x02cf2,
    0x02d7f, 0x02d80,
    0x02de0, 0x02e00,
    0x0302a, 0x03030,
    0x03099, 0x0309b,
    0x0a66f, 0x0a670,
    0x0a67c, 0x0a67e,
    0x0a6f0, 0x0a6f2,
    0x0a802, 0x0a803,
    0x0a806, 0x0a807,
    0x0a80b, 0x0a80c,
    0x0a825, 0x0a827,
    0x0a8c4, 0x0a8c5,
    0x0a8e0, 0x0a8f2,
    0x0a926, 0x0a92e,
    0x0a947, 0x0a952,
    0x0a980, 0x0a983,
    0x0a9b3, 0x0a9b4,
    0x0a9b6, 0x0a9ba,
    0x0a9bc, 0x0a9bd,
    0x0aa29, 0x0aa2f,
    0x0aa31, 0x0aa33,
    0x0aa35, 0x0aa37,
    0x0aa43, 0x0aa44,
    0x0aa4c, 0x0aa4d,
    0x0aab0, 0x0aab1,
    0x0aab2, 0x0aab5,
    0x0aab7, 0x0aab9,
    0x0aabe, 0x0aac0,
    0x0aac1, 0x0aac2,
    0x0abe5, 0x0abe6,
    0x0abe8, 0x0abe9,
    0x0abed, 0x0abee,
    0x0fb1e, 0x0fb1f,
    0x0fe00, 0x0fe10,
    0x0fe20, 0x0fe27,
    0x101fd, 0x101fe,
    0x10a01, 0x10a04,
    0x10a05, 0x10a07,
    0x10a0c, 0x10a10,
    0x10a38, 0x10a3b,
    0x10a3f, 0x10a40,
    0x11001, 0x11002,
    0x11038, 0x11047,
    0x11080, 0x11082,
    0x110b3, 0x110b7,
    0x110b9, 0x110bb,
    0x1d167, 0x1d16a,
    0x1d17b, 0x1d183,
    0x1d185, 0x1d18c,
    0x1d1aa, 0x1d1ae,
    0x1d242, 0x1d245,
    0xe0100, 0xe01f0,
]);

immutable(RleBitSet!uint) unicodeNd = RleBitSet!uint([
    0x00030, 0x0003a,
    0x00660, 0x0066a,
    0x006f0, 0x006fa,
    0x007c0, 0x007ca,
    0x00966, 0x00970,
    0x009e6, 0x009f0,
    0x00a66, 0x00a70,
    0x00ae6, 0x00af0,
    0x00b66, 0x00b70,
    0x00be6, 0x00bf0,
    0x00c66, 0x00c70,
    0x00ce6, 0x00cf0,
    0x00d66, 0x00d70,
    0x00e50, 0x00e5a,
    0x00ed0, 0x00eda,
    0x00f20, 0x00f2a,
    0x01040, 0x0104a,
    0x01090, 0x0109a,
    0x017e0, 0x017ea,
    0x01810, 0x0181a,
    0x01946, 0x01950,
    0x019d0, 0x019da,
    0x01a80, 0x01a8a,
    0x01a90, 0x01a9a,
    0x01b50, 0x01b5a,
    0x01bb0, 0x01bba,
    0x01c40, 0x01c4a,
    0x01c50, 0x01c5a,
    0x0a620, 0x0a62a,
    0x0a8d0, 0x0a8da,
    0x0a900, 0x0a90a,
    0x0a9d0, 0x0a9da,
    0x0aa50, 0x0aa5a,
    0x0abf0, 0x0abfa,
    0x0ff10, 0x0ff1a,
    0x104a0, 0x104aa,
    0x11066, 0x11070,
    0x1d7ce, 0x1d800,
]);

immutable(RleBitSet!uint) unicodeBamum = RleBitSet!uint([
    0x0a6a0, 0x0a6f8,
    0x16800, 0x16a39,
]);

immutable(RleBitSet!uint) unicodeJavanese = RleBitSet!uint([
    0x0a980, 0x0a9ce,
    0x0a9cf, 0x0a9da,
    0x0a9de, 0x0a9e0,
]);

immutable(RleBitSet!uint) unicodeTifinagh = RleBitSet!uint([
    0x02d30, 0x02d66,
    0x02d6f, 0x02d71,
    0x02d7f, 0x02d80,
]);

immutable(RleBitSet!uint) unicodeInHalfwidth_and_Fullwidth_Forms = RleBitSet!uint([
    0x0ff00, 0x0fff0,
]);

immutable(RleBitSet!uint) unicodeInDevanagari_Extended = RleBitSet!uint([
    0x0a8e0, 0x0a900,
]);

immutable(RleBitSet!uint) unicodeInIdeographic_Description_Characters = RleBitSet!uint([
    0x02ff0, 0x03000,
]);

immutable(RleBitSet!uint) unicodeInCuneiform = RleBitSet!uint([
    0x12000, 0x12400,
]);

immutable(RleBitSet!uint) unicodeNl = RleBitSet!uint([
    0x016ee, 0x016f1,
    0x02160, 0x02183,
    0x02185, 0x02189,
    0x03007, 0x03008,
    0x03021, 0x0302a,
    0x03038, 0x0303b,
    0x0a6e6, 0x0a6f0,
    0x10140, 0x10175,
    0x10341, 0x10342,
    0x1034a, 0x1034b,
    0x103d1, 0x103d6,
    0x12400, 0x12463,
]);

immutable(RleBitSet!uint) unicodeInHangul_Compatibility_Jamo = RleBitSet!uint([
    0x03130, 0x03190,
]);

immutable(RleBitSet!uint) unicodeNo = RleBitSet!uint([
    0x000b2, 0x000b4,
    0x000b9, 0x000ba,
    0x000bc, 0x000bf,
    0x009f4, 0x009fa,
    0x00b72, 0x00b78,
    0x00bf0, 0x00bf3,
    0x00c78, 0x00c7f,
    0x00d70, 0x00d76,
    0x00f2a, 0x00f34,
    0x01369, 0x0137d,
    0x017f0, 0x017fa,
    0x019da, 0x019db,
    0x02070, 0x02071,
    0x02074, 0x0207a,
    0x02080, 0x0208a,
    0x02150, 0x02160,
    0x02189, 0x0218a,
    0x02460, 0x0249c,
    0x024ea, 0x02500,
    0x02776, 0x02794,
    0x02cfd, 0x02cfe,
    0x03192, 0x03196,
    0x03220, 0x0322a,
    0x03251, 0x03260,
    0x03280, 0x0328a,
    0x032b1, 0x032c0,
    0x0a830, 0x0a836,
    0x10107, 0x10134,
    0x10175, 0x10179,
    0x1018a, 0x1018b,
    0x10320, 0x10324,
    0x10858, 0x10860,
    0x10916, 0x1091c,
    0x10a40, 0x10a48,
    0x10a7d, 0x10a7f,
    0x10b58, 0x10b60,
    0x10b78, 0x10b80,
    0x10e60, 0x10e7f,
    0x11052, 0x11066,
    0x1d360, 0x1d372,
    0x1f100, 0x1f10b,
]);

immutable(RleBitSet!uint) unicodeInOriya = RleBitSet!uint([
    0x00b00, 0x00b80,
]);

immutable(RleBitSet!uint) unicodeLogical_Order_Exception = RleBitSet!uint([
    0x00e40, 0x00e45,
    0x00ec0, 0x00ec5,
    0x0aab5, 0x0aab7,
    0x0aab9, 0x0aaba,
    0x0aabb, 0x0aabd,
]);

immutable(RleBitSet!uint) unicodeInscriptional_Parthian = RleBitSet!uint([
    0x10b40, 0x10b56,
    0x10b58, 0x10b60,
]);

immutable(RleBitSet!uint) unicodeSyloti_Nagri = RleBitSet!uint([
    0x0a800, 0x0a82c,
]);

immutable(RleBitSet!uint) unicodeInBengali = RleBitSet!uint([
    0x00980, 0x00a00,
]);

immutable(RleBitSet!uint) unicodeInTagalog = RleBitSet!uint([
    0x01700, 0x01720,
]);

immutable(RleBitSet!uint) unicodeInMyanmar_Extended_A = RleBitSet!uint([
    0x0aa60, 0x0aa80,
]);

immutable(RleBitSet!uint) unicodeInAvestan = RleBitSet!uint([
    0x10b00, 0x10b40,
]);

immutable(RleBitSet!uint) unicodePc = RleBitSet!uint([
    0x0005f, 0x00060,
    0x0203f, 0x02041,
    0x02054, 0x02055,
    0x0fe33, 0x0fe35,
    0x0fe4d, 0x0fe50,
    0x0ff3f, 0x0ff40,
]);

immutable(RleBitSet!uint) unicodeInCyrillic_Extended_A = RleBitSet!uint([
    0x02de0, 0x02e00,
]);

immutable(RleBitSet!uint) unicodePd = RleBitSet!uint([
    0x0002d, 0x0002e,
    0x0058a, 0x0058b,
    0x005be, 0x005bf,
    0x01400, 0x01401,
    0x01806, 0x01807,
    0x02010, 0x02016,
    0x02e17, 0x02e18,
    0x02e1a, 0x02e1b,
    0x0301c, 0x0301d,
    0x03030, 0x03031,
    0x030a0, 0x030a1,
    0x0fe31, 0x0fe33,
    0x0fe58, 0x0fe59,
    0x0fe63, 0x0fe64,
    0x0ff0d, 0x0ff0e,
]);

immutable(RleBitSet!uint) unicodeInCyrillic_Extended_B = RleBitSet!uint([
    0x0a640, 0x0a6a0,
]);

immutable(RleBitSet!uint) unicodeInBasic_Latin = RleBitSet!uint([
    0x00000, 0x00080,
]);

immutable(RleBitSet!uint) unicodePe = RleBitSet!uint([
    0x00029, 0x0002a,
    0x0005d, 0x0005e,
    0x0007d, 0x0007e,
    0x00f3b, 0x00f3c,
    0x00f3d, 0x00f3e,
    0x0169c, 0x0169d,
    0x02046, 0x02047,
    0x0207e, 0x0207f,
    0x0208e, 0x0208f,
    0x0232a, 0x0232b,
    0x02769, 0x0276a,
    0x0276b, 0x0276c,
    0x0276d, 0x0276e,
    0x0276f, 0x02770,
    0x02771, 0x02772,
    0x02773, 0x02774,
    0x02775, 0x02776,
    0x027c6, 0x027c7,
    0x027e7, 0x027e8,
    0x027e9, 0x027ea,
    0x027eb, 0x027ec,
    0x027ed, 0x027ee,
    0x027ef, 0x027f0,
    0x02984, 0x02985,
    0x02986, 0x02987,
    0x02988, 0x02989,
    0x0298a, 0x0298b,
    0x0298c, 0x0298d,
    0x0298e, 0x0298f,
    0x02990, 0x02991,
    0x02992, 0x02993,
    0x02994, 0x02995,
    0x02996, 0x02997,
    0x02998, 0x02999,
    0x029d9, 0x029da,
    0x029db, 0x029dc,
    0x029fd, 0x029fe,
    0x02e23, 0x02e24,
    0x02e25, 0x02e26,
    0x02e27, 0x02e28,
    0x02e29, 0x02e2a,
    0x03009, 0x0300a,
    0x0300b, 0x0300c,
    0x0300d, 0x0300e,
    0x0300f, 0x03010,
    0x03011, 0x03012,
    0x03015, 0x03016,
    0x03017, 0x03018,
    0x03019, 0x0301a,
    0x0301b, 0x0301c,
    0x0301e, 0x03020,
    0x0fd3f, 0x0fd40,
    0x0fe18, 0x0fe19,
    0x0fe36, 0x0fe37,
    0x0fe38, 0x0fe39,
    0x0fe3a, 0x0fe3b,
    0x0fe3c, 0x0fe3d,
    0x0fe3e, 0x0fe3f,
    0x0fe40, 0x0fe41,
    0x0fe42, 0x0fe43,
    0x0fe44, 0x0fe45,
    0x0fe48, 0x0fe49,
    0x0fe5a, 0x0fe5b,
    0x0fe5c, 0x0fe5d,
    0x0fe5e, 0x0fe5f,
    0x0ff09, 0x0ff0a,
    0x0ff3d, 0x0ff3e,
    0x0ff5d, 0x0ff5e,
    0x0ff60, 0x0ff61,
    0x0ff63, 0x0ff64,
]);

immutable(RleBitSet!uint) unicodeHanunoo = RleBitSet!uint([
    0x01720, 0x01735,
]);

immutable(RleBitSet!uint) unicodePf = RleBitSet!uint([
    0x000bb, 0x000bc,
    0x02019, 0x0201a,
    0x0201d, 0x0201e,
    0x0203a, 0x0203b,
    0x02e03, 0x02e04,
    0x02e05, 0x02e06,
    0x02e0a, 0x02e0b,
    0x02e0d, 0x02e0e,
    0x02e1d, 0x02e1e,
    0x02e21, 0x02e22,
]);

immutable(RleBitSet!uint) unicodePi = RleBitSet!uint([
    0x000ab, 0x000ac,
    0x02018, 0x02019,
    0x0201b, 0x0201d,
    0x0201f, 0x02020,
    0x02039, 0x0203a,
    0x02e02, 0x02e03,
    0x02e04, 0x02e05,
    0x02e09, 0x02e0a,
    0x02e0c, 0x02e0d,
    0x02e1c, 0x02e1d,
    0x02e20, 0x02e21,
]);

immutable(RleBitSet!uint) unicodeSinhala = RleBitSet!uint([
    0x00d82, 0x00d84,
    0x00d85, 0x00d97,
    0x00d9a, 0x00db2,
    0x00db3, 0x00dbc,
    0x00dbd, 0x00dbe,
    0x00dc0, 0x00dc7,
    0x00dca, 0x00dcb,
    0x00dcf, 0x00dd5,
    0x00dd6, 0x00dd7,
    0x00dd8, 0x00de0,
    0x00df2, 0x00df5,
]);

immutable(RleBitSet!uint) unicodeInJavanese = RleBitSet!uint([
    0x0a980, 0x0a9e0,
]);

immutable(RleBitSet!uint) unicodeInCarian = RleBitSet!uint([
    0x102a0, 0x102e0,
]);

immutable(RleBitSet!uint) unicodeInDomino_Tiles = RleBitSet!uint([
    0x1f030, 0x1f0a0,
]);

immutable(RleBitSet!uint) unicodeInTifinagh = RleBitSet!uint([
    0x02d30, 0x02d80,
]);

immutable(RleBitSet!uint) unicodeLycian = RleBitSet!uint([
    0x10280, 0x1029d,
]);

immutable(RleBitSet!uint) unicodeInGeometric_Shapes = RleBitSet!uint([
    0x025a0, 0x02600,
]);

immutable(RleBitSet!uint) unicodeInArabic_Presentation_Forms_A = RleBitSet!uint([
    0x0fb50, 0x0fe00,
]);

immutable(RleBitSet!uint) unicodeInArabic_Presentation_Forms_B = RleBitSet!uint([
    0x0fe70, 0x0ff00,
]);

immutable(RleBitSet!uint) unicodePo = RleBitSet!uint([
    0x00021, 0x00024,
    0x00025, 0x00028,
    0x0002a, 0x0002b,
    0x0002c, 0x0002d,
    0x0002e, 0x00030,
    0x0003a, 0x0003c,
    0x0003f, 0x00041,
    0x0005c, 0x0005d,
    0x000a1, 0x000a2,
    0x000b7, 0x000b8,
    0x000bf, 0x000c0,
    0x0037e, 0x0037f,
    0x00387, 0x00388,
    0x0055a, 0x00560,
    0x00589, 0x0058a,
    0x005c0, 0x005c1,
    0x005c3, 0x005c4,
    0x005c6, 0x005c7,
    0x005f3, 0x005f5,
    0x00609, 0x0060b,
    0x0060c, 0x0060e,
    0x0061b, 0x0061c,
    0x0061e, 0x00620,
    0x0066a, 0x0066e,
    0x006d4, 0x006d5,
    0x00700, 0x0070e,
    0x007f7, 0x007fa,
    0x00830, 0x0083f,
    0x0085e, 0x0085f,
    0x00964, 0x00966,
    0x00970, 0x00971,
    0x00df4, 0x00df5,
    0x00e4f, 0x00e50,
    0x00e5a, 0x00e5c,
    0x00f04, 0x00f13,
    0x00f85, 0x00f86,
    0x00fd0, 0x00fd5,
    0x00fd9, 0x00fdb,
    0x0104a, 0x01050,
    0x010fb, 0x010fc,
    0x01361, 0x01369,
    0x0166d, 0x0166f,
    0x016eb, 0x016ee,
    0x01735, 0x01737,
    0x017d4, 0x017d7,
    0x017d8, 0x017db,
    0x01800, 0x01806,
    0x01807, 0x0180b,
    0x01944, 0x01946,
    0x01a1e, 0x01a20,
    0x01aa0, 0x01aa7,
    0x01aa8, 0x01aae,
    0x01b5a, 0x01b61,
    0x01bfc, 0x01c00,
    0x01c3b, 0x01c40,
    0x01c7e, 0x01c80,
    0x01cd3, 0x01cd4,
    0x02016, 0x02018,
    0x02020, 0x02028,
    0x02030, 0x02039,
    0x0203b, 0x0203f,
    0x02041, 0x02044,
    0x02047, 0x02052,
    0x02053, 0x02054,
    0x02055, 0x0205f,
    0x02cf9, 0x02cfd,
    0x02cfe, 0x02d00,
    0x02d70, 0x02d71,
    0x02e00, 0x02e02,
    0x02e06, 0x02e09,
    0x02e0b, 0x02e0c,
    0x02e0e, 0x02e17,
    0x02e18, 0x02e1a,
    0x02e1b, 0x02e1c,
    0x02e1e, 0x02e20,
    0x02e2a, 0x02e2f,
    0x02e30, 0x02e32,
    0x03001, 0x03004,
    0x0303d, 0x0303e,
    0x030fb, 0x030fc,
    0x0a4fe, 0x0a500,
    0x0a60d, 0x0a610,
    0x0a673, 0x0a674,
    0x0a67e, 0x0a67f,
    0x0a6f2, 0x0a6f8,
    0x0a874, 0x0a878,
    0x0a8ce, 0x0a8d0,
    0x0a8f8, 0x0a8fb,
    0x0a92e, 0x0a930,
    0x0a95f, 0x0a960,
    0x0a9c1, 0x0a9ce,
    0x0a9de, 0x0a9e0,
    0x0aa5c, 0x0aa60,
    0x0aade, 0x0aae0,
    0x0abeb, 0x0abec,
    0x0fe10, 0x0fe17,
    0x0fe19, 0x0fe1a,
    0x0fe30, 0x0fe31,
    0x0fe45, 0x0fe47,
    0x0fe49, 0x0fe4d,
    0x0fe50, 0x0fe53,
    0x0fe54, 0x0fe58,
    0x0fe5f, 0x0fe62,
    0x0fe68, 0x0fe69,
    0x0fe6a, 0x0fe6c,
    0x0ff01, 0x0ff04,
    0x0ff05, 0x0ff08,
    0x0ff0a, 0x0ff0b,
    0x0ff0c, 0x0ff0d,
    0x0ff0e, 0x0ff10,
    0x0ff1a, 0x0ff1c,
    0x0ff1f, 0x0ff21,
    0x0ff3c, 0x0ff3d,
    0x0ff61, 0x0ff62,
    0x0ff64, 0x0ff66,
    0x10100, 0x10102,
    0x1039f, 0x103a0,
    0x103d0, 0x103d1,
    0x10857, 0x10858,
    0x1091f, 0x10920,
    0x1093f, 0x10940,
    0x10a50, 0x10a59,
    0x10a7f, 0x10a80,
    0x10b39, 0x10b40,
    0x11047, 0x1104e,
    0x110bb, 0x110bd,
    0x110be, 0x110c2,
    0x12470, 0x12474,
]);

immutable(RleBitSet!uint) unicodeTerminal_Punctuation = RleBitSet!uint([
    0x00021, 0x00022,
    0x0002c, 0x0002d,
    0x0002e, 0x0002f,
    0x0003a, 0x0003c,
    0x0003f, 0x00040,
    0x0037e, 0x0037f,
    0x00387, 0x00388,
    0x00589, 0x0058a,
    0x005c3, 0x005c4,
    0x0060c, 0x0060d,
    0x0061b, 0x0061c,
    0x0061f, 0x00620,
    0x006d4, 0x006d5,
    0x00700, 0x0070b,
    0x0070c, 0x0070d,
    0x007f8, 0x007fa,
    0x00830, 0x0083f,
    0x0085e, 0x0085f,
    0x00964, 0x00966,
    0x00e5a, 0x00e5c,
    0x00f08, 0x00f09,
    0x00f0d, 0x00f13,
    0x0104a, 0x0104c,
    0x01361, 0x01369,
    0x0166d, 0x0166f,
    0x016eb, 0x016ee,
    0x017d4, 0x017d7,
    0x017da, 0x017db,
    0x01802, 0x01806,
    0x01808, 0x0180a,
    0x01944, 0x01946,
    0x01aa8, 0x01aac,
    0x01b5a, 0x01b5c,
    0x01b5d, 0x01b60,
    0x01c3b, 0x01c40,
    0x01c7e, 0x01c80,
    0x0203c, 0x0203e,
    0x02047, 0x0204a,
    0x02e2e, 0x02e2f,
    0x03001, 0x03003,
    0x0a4fe, 0x0a500,
    0x0a60d, 0x0a610,
    0x0a6f3, 0x0a6f8,
    0x0a876, 0x0a878,
    0x0a8ce, 0x0a8d0,
    0x0a92f, 0x0a930,
    0x0a9c7, 0x0a9ca,
    0x0aa5d, 0x0aa60,
    0x0aadf, 0x0aae0,
    0x0abeb, 0x0abec,
    0x0fe50, 0x0fe53,
    0x0fe54, 0x0fe58,
    0x0ff01, 0x0ff02,
    0x0ff0c, 0x0ff0d,
    0x0ff0e, 0x0ff0f,
    0x0ff1a, 0x0ff1c,
    0x0ff1f, 0x0ff20,
    0x0ff61, 0x0ff62,
    0x0ff64, 0x0ff65,
    0x1039f, 0x103a0,
    0x103d0, 0x103d1,
    0x10857, 0x10858,
    0x1091f, 0x10920,
    0x10b3a, 0x10b40,
    0x11047, 0x1104e,
    0x110be, 0x110c2,
    0x12470, 0x12474,
]);

immutable(RleBitSet!uint) unicodePs = RleBitSet!uint([
    0x00028, 0x00029,
    0x0005b, 0x0005c,
    0x0007b, 0x0007c,
    0x00f3a, 0x00f3b,
    0x00f3c, 0x00f3d,
    0x0169b, 0x0169c,
    0x0201a, 0x0201b,
    0x0201e, 0x0201f,
    0x02045, 0x02046,
    0x0207d, 0x0207e,
    0x0208d, 0x0208e,
    0x02329, 0x0232a,
    0x02768, 0x02769,
    0x0276a, 0x0276b,
    0x0276c, 0x0276d,
    0x0276e, 0x0276f,
    0x02770, 0x02771,
    0x02772, 0x02773,
    0x02774, 0x02775,
    0x027c5, 0x027c6,
    0x027e6, 0x027e7,
    0x027e8, 0x027e9,
    0x027ea, 0x027eb,
    0x027ec, 0x027ed,
    0x027ee, 0x027ef,
    0x02983, 0x02984,
    0x02985, 0x02986,
    0x02987, 0x02988,
    0x02989, 0x0298a,
    0x0298b, 0x0298c,
    0x0298d, 0x0298e,
    0x0298f, 0x02990,
    0x02991, 0x02992,
    0x02993, 0x02994,
    0x02995, 0x02996,
    0x02997, 0x02998,
    0x029d8, 0x029d9,
    0x029da, 0x029db,
    0x029fc, 0x029fd,
    0x02e22, 0x02e23,
    0x02e24, 0x02e25,
    0x02e26, 0x02e27,
    0x02e28, 0x02e29,
    0x03008, 0x03009,
    0x0300a, 0x0300b,
    0x0300c, 0x0300d,
    0x0300e, 0x0300f,
    0x03010, 0x03011,
    0x03014, 0x03015,
    0x03016, 0x03017,
    0x03018, 0x03019,
    0x0301a, 0x0301b,
    0x0301d, 0x0301e,
    0x0fd3e, 0x0fd3f,
    0x0fe17, 0x0fe18,
    0x0fe35, 0x0fe36,
    0x0fe37, 0x0fe38,
    0x0fe39, 0x0fe3a,
    0x0fe3b, 0x0fe3c,
    0x0fe3d, 0x0fe3e,
    0x0fe3f, 0x0fe40,
    0x0fe41, 0x0fe42,
    0x0fe43, 0x0fe44,
    0x0fe47, 0x0fe48,
    0x0fe59, 0x0fe5a,
    0x0fe5b, 0x0fe5c,
    0x0fe5d, 0x0fe5e,
    0x0ff08, 0x0ff09,
    0x0ff3b, 0x0ff3c,
    0x0ff5b, 0x0ff5c,
    0x0ff5f, 0x0ff60,
    0x0ff62, 0x0ff63,
]);

immutable(RleBitSet!uint) unicodeInSpacing_Modifier_Letters = RleBitSet!uint([
    0x002b0, 0x00300,
]);

immutable(RleBitSet!uint) unicodeOther_Alphabetic = RleBitSet!uint([
    0x00345, 0x00346,
    0x005b0, 0x005be,
    0x005bf, 0x005c0,
    0x005c1, 0x005c3,
    0x005c4, 0x005c6,
    0x005c7, 0x005c8,
    0x00610, 0x0061b,
    0x0064b, 0x00658,
    0x00659, 0x00660,
    0x00670, 0x00671,
    0x006d6, 0x006dd,
    0x006e1, 0x006e5,
    0x006e7, 0x006e9,
    0x006ed, 0x006ee,
    0x00711, 0x00712,
    0x00730, 0x00740,
    0x007a6, 0x007b1,
    0x00816, 0x00818,
    0x0081b, 0x00824,
    0x00825, 0x00828,
    0x00829, 0x0082d,
    0x00900, 0x00904,
    0x0093a, 0x0093c,
    0x0093e, 0x0094d,
    0x0094e, 0x00950,
    0x00955, 0x00958,
    0x00962, 0x00964,
    0x00981, 0x00984,
    0x009be, 0x009c5,
    0x009c7, 0x009c9,
    0x009cb, 0x009cd,
    0x009d7, 0x009d8,
    0x009e2, 0x009e4,
    0x00a01, 0x00a04,
    0x00a3e, 0x00a43,
    0x00a47, 0x00a49,
    0x00a4b, 0x00a4d,
    0x00a51, 0x00a52,
    0x00a70, 0x00a72,
    0x00a75, 0x00a76,
    0x00a81, 0x00a84,
    0x00abe, 0x00ac6,
    0x00ac7, 0x00aca,
    0x00acb, 0x00acd,
    0x00ae2, 0x00ae4,
    0x00b01, 0x00b04,
    0x00b3e, 0x00b45,
    0x00b47, 0x00b49,
    0x00b4b, 0x00b4d,
    0x00b56, 0x00b58,
    0x00b62, 0x00b64,
    0x00b82, 0x00b83,
    0x00bbe, 0x00bc3,
    0x00bc6, 0x00bc9,
    0x00bca, 0x00bcd,
    0x00bd7, 0x00bd8,
    0x00c01, 0x00c04,
    0x00c3e, 0x00c45,
    0x00c46, 0x00c49,
    0x00c4a, 0x00c4d,
    0x00c55, 0x00c57,
    0x00c62, 0x00c64,
    0x00c82, 0x00c84,
    0x00cbe, 0x00cc5,
    0x00cc6, 0x00cc9,
    0x00cca, 0x00ccd,
    0x00cd5, 0x00cd7,
    0x00ce2, 0x00ce4,
    0x00d02, 0x00d04,
    0x00d3e, 0x00d45,
    0x00d46, 0x00d49,
    0x00d4a, 0x00d4d,
    0x00d57, 0x00d58,
    0x00d62, 0x00d64,
    0x00d82, 0x00d84,
    0x00dcf, 0x00dd5,
    0x00dd6, 0x00dd7,
    0x00dd8, 0x00de0,
    0x00df2, 0x00df4,
    0x00e31, 0x00e32,
    0x00e34, 0x00e3b,
    0x00e4d, 0x00e4e,
    0x00eb1, 0x00eb2,
    0x00eb4, 0x00eba,
    0x00ebb, 0x00ebd,
    0x00ecd, 0x00ece,
    0x00f71, 0x00f82,
    0x00f8d, 0x00f98,
    0x00f99, 0x00fbd,
    0x0102b, 0x01037,
    0x01038, 0x01039,
    0x0103b, 0x0103f,
    0x01056, 0x0105a,
    0x0105e, 0x01061,
    0x01062, 0x01063,
    0x01067, 0x01069,
    0x01071, 0x01075,
    0x01082, 0x01087,
    0x0109c, 0x0109e,
    0x0135f, 0x01360,
    0x01712, 0x01714,
    0x01732, 0x01734,
    0x01752, 0x01754,
    0x01772, 0x01774,
    0x017b6, 0x017c9,
    0x018a9, 0x018aa,
    0x01920, 0x0192c,
    0x01930, 0x01939,
    0x019b0, 0x019c1,
    0x019c8, 0x019ca,
    0x01a17, 0x01a1c,
    0x01a55, 0x01a5f,
    0x01a61, 0x01a75,
    0x01b00, 0x01b05,
    0x01b35, 0x01b44,
    0x01b80, 0x01b83,
    0x01ba1, 0x01baa,
    0x01be7, 0x01bf2,
    0x01c24, 0x01c36,
    0x01cf2, 0x01cf3,
    0x024b6, 0x024ea,
    0x02de0, 0x02e00,
    0x0a823, 0x0a828,
    0x0a880, 0x0a882,
    0x0a8b4, 0x0a8c4,
    0x0a926, 0x0a92b,
    0x0a947, 0x0a953,
    0x0a980, 0x0a984,
    0x0a9b4, 0x0a9c0,
    0x0aa29, 0x0aa37,
    0x0aa43, 0x0aa44,
    0x0aa4c, 0x0aa4e,
    0x0aab0, 0x0aab1,
    0x0aab2, 0x0aab5,
    0x0aab7, 0x0aab9,
    0x0aabe, 0x0aabf,
    0x0abe3, 0x0abeb,
    0x0fb1e, 0x0fb1f,
    0x10a01, 0x10a04,
    0x10a05, 0x10a07,
    0x10a0c, 0x10a10,
    0x11000, 0x11003,
    0x11038, 0x11046,
    0x11082, 0x11083,
    0x110b0, 0x110b9,
]);

immutable(RleBitSet!uint) unicodeLepcha = RleBitSet!uint([
    0x01c00, 0x01c38,
    0x01c3b, 0x01c4a,
    0x01c4d, 0x01c50,
]);

immutable(RleBitSet!uint) unicodeKayah_Li = RleBitSet!uint([
    0x0a900, 0x0a930,
]);

immutable(RleBitSet!uint) unicodeInCounting_Rod_Numerals = RleBitSet!uint([
    0x1d360, 0x1d380,
]);

immutable(RleBitSet!uint) unicodeNko = RleBitSet!uint([
    0x007c0, 0x007fb,
]);

immutable(RleBitSet!uint) unicodeInOld_Turkic = RleBitSet!uint([
    0x10c00, 0x10c50,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Symbols_And_Pictographs = RleBitSet!uint([
    0x1f300, 0x1f600,
]);

immutable(RleBitSet!uint) unicodeInLao = RleBitSet!uint([
    0x00e80, 0x00f00,
]);

immutable(RleBitSet!uint) unicodeInNKo = RleBitSet!uint([
    0x007c0, 0x00800,
]);

immutable(RleBitSet!uint) unicodeInGreek_and_Coptic = RleBitSet!uint([
    0x00370, 0x00400,
]);

immutable(RleBitSet!uint) unicodePhags_Pa = RleBitSet!uint([
    0x0a840, 0x0a878,
]);

immutable(RleBitSet!uint) unicodeCypriot = RleBitSet!uint([
    0x10800, 0x10806,
    0x10808, 0x10809,
    0x1080a, 0x10836,
    0x10837, 0x10839,
    0x1083c, 0x1083d,
    0x1083f, 0x10840,
]);

immutable(RleBitSet!uint) unicodeInModifier_Tone_Letters = RleBitSet!uint([
    0x0a700, 0x0a720,
]);

immutable(RleBitSet!uint) unicodeTamil = RleBitSet!uint([
    0x00b82, 0x00b84,
    0x00b85, 0x00b8b,
    0x00b8e, 0x00b91,
    0x00b92, 0x00b96,
    0x00b99, 0x00b9b,
    0x00b9c, 0x00b9d,
    0x00b9e, 0x00ba0,
    0x00ba3, 0x00ba5,
    0x00ba8, 0x00bab,
    0x00bae, 0x00bba,
    0x00bbe, 0x00bc3,
    0x00bc6, 0x00bc9,
    0x00bca, 0x00bce,
    0x00bd0, 0x00bd1,
    0x00bd7, 0x00bd8,
    0x00be6, 0x00bfb,
]);

immutable(RleBitSet!uint) unicodeMyanmar = RleBitSet!uint([
    0x01000, 0x010a0,
    0x0aa60, 0x0aa7c,
]);

immutable(RleBitSet!uint) unicodeSc = RleBitSet!uint([
    0x00024, 0x00025,
    0x000a2, 0x000a6,
    0x0060b, 0x0060c,
    0x009f2, 0x009f4,
    0x009fb, 0x009fc,
    0x00af1, 0x00af2,
    0x00bf9, 0x00bfa,
    0x00e3f, 0x00e40,
    0x017db, 0x017dc,
    0x020a0, 0x020ba,
    0x0a838, 0x0a839,
    0x0fdfc, 0x0fdfd,
    0x0fe69, 0x0fe6a,
    0x0ff04, 0x0ff05,
    0x0ffe0, 0x0ffe2,
    0x0ffe5, 0x0ffe7,
]);

immutable(RleBitSet!uint) unicodeInPrivate_Use_Area = RleBitSet!uint([
    0x0e000, 0x0f900,
]);

immutable(RleBitSet!uint) unicodeKannada = RleBitSet!uint([
    0x00c82, 0x00c84,
    0x00c85, 0x00c8d,
    0x00c8e, 0x00c91,
    0x00c92, 0x00ca9,
    0x00caa, 0x00cb4,
    0x00cb5, 0x00cba,
    0x00cbc, 0x00cc5,
    0x00cc6, 0x00cc9,
    0x00cca, 0x00cce,
    0x00cd5, 0x00cd7,
    0x00cde, 0x00cdf,
    0x00ce0, 0x00ce4,
    0x00ce6, 0x00cf0,
    0x00cf1, 0x00cf3,
]);

immutable(RleBitSet!uint) unicodeInPhonetic_Extensions = RleBitSet!uint([
    0x01d00, 0x01d80,
]);

immutable(RleBitSet!uint) unicodeInEgyptian_Hieroglyphs = RleBitSet!uint([
    0x13000, 0x13430,
]);

immutable(RleBitSet!uint) unicodeTelugu = RleBitSet!uint([
    0x00c01, 0x00c04,
    0x00c05, 0x00c0d,
    0x00c0e, 0x00c11,
    0x00c12, 0x00c29,
    0x00c2a, 0x00c34,
    0x00c35, 0x00c3a,
    0x00c3d, 0x00c45,
    0x00c46, 0x00c49,
    0x00c4a, 0x00c4e,
    0x00c55, 0x00c57,
    0x00c58, 0x00c5a,
    0x00c60, 0x00c64,
    0x00c66, 0x00c70,
    0x00c78, 0x00c80,
]);

immutable(RleBitSet!uint) unicodeInCombining_Diacritical_Marks = RleBitSet!uint([
    0x00300, 0x00370,
]);

immutable(RleBitSet!uint) unicodeCham = RleBitSet!uint([
    0x0aa00, 0x0aa37,
    0x0aa40, 0x0aa4e,
    0x0aa50, 0x0aa5a,
    0x0aa5c, 0x0aa60,
]);

immutable(RleBitSet!uint) unicodeInArabic_Supplement = RleBitSet!uint([
    0x00750, 0x00780,
]);

immutable(RleBitSet!uint) unicodeSk = RleBitSet!uint([
    0x0005e, 0x0005f,
    0x00060, 0x00061,
    0x000a8, 0x000a9,
    0x000af, 0x000b0,
    0x000b4, 0x000b5,
    0x000b8, 0x000b9,
    0x002c2, 0x002c6,
    0x002d2, 0x002e0,
    0x002e5, 0x002ec,
    0x002ed, 0x002ee,
    0x002ef, 0x00300,
    0x00375, 0x00376,
    0x00384, 0x00386,
    0x01fbd, 0x01fbe,
    0x01fbf, 0x01fc2,
    0x01fcd, 0x01fd0,
    0x01fdd, 0x01fe0,
    0x01fed, 0x01ff0,
    0x01ffd, 0x01fff,
    0x0309b, 0x0309d,
    0x0a700, 0x0a717,
    0x0a720, 0x0a722,
    0x0a789, 0x0a78b,
    0x0fbb2, 0x0fbc2,
    0x0ff3e, 0x0ff3f,
    0x0ff40, 0x0ff41,
    0x0ffe3, 0x0ffe4,
]);

immutable(RleBitSet!uint) unicodeImperial_Aramaic = RleBitSet!uint([
    0x10840, 0x10856,
    0x10857, 0x10860,
]);

immutable(RleBitSet!uint) unicodeSm = RleBitSet!uint([
    0x0002b, 0x0002c,
    0x0003c, 0x0003f,
    0x0007c, 0x0007d,
    0x0007e, 0x0007f,
    0x000ac, 0x000ad,
    0x000b1, 0x000b2,
    0x000d7, 0x000d8,
    0x000f7, 0x000f8,
    0x003f6, 0x003f7,
    0x00606, 0x00609,
    0x02044, 0x02045,
    0x02052, 0x02053,
    0x0207a, 0x0207d,
    0x0208a, 0x0208d,
    0x02118, 0x02119,
    0x02140, 0x02145,
    0x0214b, 0x0214c,
    0x02190, 0x02195,
    0x0219a, 0x0219c,
    0x021a0, 0x021a1,
    0x021a3, 0x021a4,
    0x021a6, 0x021a7,
    0x021ae, 0x021af,
    0x021ce, 0x021d0,
    0x021d2, 0x021d3,
    0x021d4, 0x021d5,
    0x021f4, 0x02300,
    0x02308, 0x0230c,
    0x02320, 0x02322,
    0x0237c, 0x0237d,
    0x0239b, 0x023b4,
    0x023dc, 0x023e2,
    0x025b7, 0x025b8,
    0x025c1, 0x025c2,
    0x025f8, 0x02600,
    0x0266f, 0x02670,
    0x027c0, 0x027c5,
    0x027c7, 0x027cb,
    0x027cc, 0x027cd,
    0x027ce, 0x027e6,
    0x027f0, 0x02800,
    0x02900, 0x02983,
    0x02999, 0x029d8,
    0x029dc, 0x029fc,
    0x029fe, 0x02b00,
    0x02b30, 0x02b45,
    0x02b47, 0x02b4d,
    0x0fb29, 0x0fb2a,
    0x0fe62, 0x0fe63,
    0x0fe64, 0x0fe67,
    0x0ff0b, 0x0ff0c,
    0x0ff1c, 0x0ff1f,
    0x0ff5c, 0x0ff5d,
    0x0ff5e, 0x0ff5f,
    0x0ffe2, 0x0ffe3,
    0x0ffe9, 0x0ffed,
    0x1d6c1, 0x1d6c2,
    0x1d6db, 0x1d6dc,
    0x1d6fb, 0x1d6fc,
    0x1d715, 0x1d716,
    0x1d735, 0x1d736,
    0x1d74f, 0x1d750,
    0x1d76f, 0x1d770,
    0x1d789, 0x1d78a,
    0x1d7a9, 0x1d7aa,
    0x1d7c3, 0x1d7c4,
]);

immutable(RleBitSet!uint) unicodeKharoshthi = RleBitSet!uint([
    0x10a00, 0x10a04,
    0x10a05, 0x10a07,
    0x10a0c, 0x10a14,
    0x10a15, 0x10a18,
    0x10a19, 0x10a34,
    0x10a38, 0x10a3b,
    0x10a3f, 0x10a48,
    0x10a50, 0x10a59,
]);

immutable(RleBitSet!uint) unicodeInLycian = RleBitSet!uint([
    0x10280, 0x102a0,
]);

immutable(RleBitSet!uint) unicodeInCombining_Half_Marks = RleBitSet!uint([
    0x0fe20, 0x0fe30,
]);

immutable(RleBitSet!uint) unicodeSo = RleBitSet!uint([
    0x000a6, 0x000a8,
    0x000a9, 0x000aa,
    0x000ae, 0x000af,
    0x000b0, 0x000b1,
    0x000b6, 0x000b7,
    0x00482, 0x00483,
    0x0060e, 0x00610,
    0x006de, 0x006df,
    0x006e9, 0x006ea,
    0x006fd, 0x006ff,
    0x007f6, 0x007f7,
    0x009fa, 0x009fb,
    0x00b70, 0x00b71,
    0x00bf3, 0x00bf9,
    0x00bfa, 0x00bfb,
    0x00c7f, 0x00c80,
    0x00d79, 0x00d7a,
    0x00f01, 0x00f04,
    0x00f13, 0x00f18,
    0x00f1a, 0x00f20,
    0x00f34, 0x00f35,
    0x00f36, 0x00f37,
    0x00f38, 0x00f39,
    0x00fbe, 0x00fc6,
    0x00fc7, 0x00fcd,
    0x00fce, 0x00fd0,
    0x00fd5, 0x00fd9,
    0x0109e, 0x010a0,
    0x01360, 0x01361,
    0x01390, 0x0139a,
    0x01940, 0x01941,
    0x019de, 0x01a00,
    0x01b61, 0x01b6b,
    0x01b74, 0x01b7d,
    0x02100, 0x02102,
    0x02103, 0x02107,
    0x02108, 0x0210a,
    0x02114, 0x02115,
    0x02116, 0x02118,
    0x0211e, 0x02124,
    0x02125, 0x02126,
    0x02127, 0x02128,
    0x02129, 0x0212a,
    0x0212e, 0x0212f,
    0x0213a, 0x0213c,
    0x0214a, 0x0214b,
    0x0214c, 0x0214e,
    0x0214f, 0x02150,
    0x02195, 0x0219a,
    0x0219c, 0x021a0,
    0x021a1, 0x021a3,
    0x021a4, 0x021a6,
    0x021a7, 0x021ae,
    0x021af, 0x021ce,
    0x021d0, 0x021d2,
    0x021d3, 0x021d4,
    0x021d5, 0x021f4,
    0x02300, 0x02308,
    0x0230c, 0x02320,
    0x02322, 0x02329,
    0x0232b, 0x0237c,
    0x0237d, 0x0239b,
    0x023b4, 0x023dc,
    0x023e2, 0x023f4,
    0x02400, 0x02427,
    0x02440, 0x0244b,
    0x0249c, 0x024ea,
    0x02500, 0x025b7,
    0x025b8, 0x025c1,
    0x025c2, 0x025f8,
    0x02600, 0x0266f,
    0x02670, 0x02700,
    0x02701, 0x02768,
    0x02794, 0x027c0,
    0x02800, 0x02900,
    0x02b00, 0x02b30,
    0x02b45, 0x02b47,
    0x02b50, 0x02b5a,
    0x02ce5, 0x02ceb,
    0x02e80, 0x02e9a,
    0x02e9b, 0x02ef4,
    0x02f00, 0x02fd6,
    0x02ff0, 0x02ffc,
    0x03004, 0x03005,
    0x03012, 0x03014,
    0x03020, 0x03021,
    0x03036, 0x03038,
    0x0303e, 0x03040,
    0x03190, 0x03192,
    0x03196, 0x031a0,
    0x031c0, 0x031e4,
    0x03200, 0x0321f,
    0x0322a, 0x03251,
    0x03260, 0x03280,
    0x0328a, 0x032b1,
    0x032c0, 0x032ff,
    0x03300, 0x03400,
    0x04dc0, 0x04e00,
    0x0a490, 0x0a4c7,
    0x0a828, 0x0a82c,
    0x0a836, 0x0a838,
    0x0a839, 0x0a83a,
    0x0aa77, 0x0aa7a,
    0x0fdfd, 0x0fdfe,
    0x0ffe4, 0x0ffe5,
    0x0ffe8, 0x0ffe9,
    0x0ffed, 0x0ffef,
    0x0fffc, 0x0fffe,
    0x10102, 0x10103,
    0x10137, 0x10140,
    0x10179, 0x1018a,
    0x10190, 0x1019c,
    0x101d0, 0x101fd,
    0x1d000, 0x1d0f6,
    0x1d100, 0x1d127,
    0x1d129, 0x1d165,
    0x1d16a, 0x1d16d,
    0x1d183, 0x1d185,
    0x1d18c, 0x1d1aa,
    0x1d1ae, 0x1d1de,
    0x1d200, 0x1d242,
    0x1d245, 0x1d246,
    0x1d300, 0x1d357,
    0x1f000, 0x1f02c,
    0x1f030, 0x1f094,
    0x1f0a0, 0x1f0af,
    0x1f0b1, 0x1f0bf,
    0x1f0c1, 0x1f0d0,
    0x1f0d1, 0x1f0e0,
    0x1f110, 0x1f12f,
    0x1f130, 0x1f16a,
    0x1f170, 0x1f19b,
    0x1f1e6, 0x1f203,
    0x1f210, 0x1f23b,
    0x1f240, 0x1f249,
    0x1f250, 0x1f252,
    0x1f300, 0x1f321,
    0x1f330, 0x1f336,
    0x1f337, 0x1f37d,
    0x1f380, 0x1f394,
    0x1f3a0, 0x1f3c5,
    0x1f3c6, 0x1f3cb,
    0x1f3e0, 0x1f3f1,
    0x1f400, 0x1f43f,
    0x1f440, 0x1f441,
    0x1f442, 0x1f4f8,
    0x1f4f9, 0x1f4fd,
    0x1f500, 0x1f53e,
    0x1f550, 0x1f568,
    0x1f5fb, 0x1f600,
    0x1f601, 0x1f611,
    0x1f612, 0x1f615,
    0x1f616, 0x1f617,
    0x1f618, 0x1f619,
    0x1f61a, 0x1f61b,
    0x1f61c, 0x1f61f,
    0x1f620, 0x1f626,
    0x1f628, 0x1f62c,
    0x1f62d, 0x1f62e,
    0x1f630, 0x1f634,
    0x1f635, 0x1f641,
    0x1f645, 0x1f650,
    0x1f680, 0x1f6c6,
    0x1f700, 0x1f774,
]);

immutable(RleBitSet!uint) unicodeInEnclosed_Alphanumeric_Supplement = RleBitSet!uint([
    0x1f100, 0x1f200,
]);

immutable(RleBitSet!uint) unicodeInTai_Le = RleBitSet!uint([
    0x01950, 0x01980,
]);

immutable(RleBitSet!uint) unicodeInMandaic = RleBitSet!uint([
    0x00840, 0x00860,
]);

immutable(RleBitSet!uint) unicodeInLepcha = RleBitSet!uint([
    0x01c00, 0x01c50,
]);

immutable(RleBitSet!uint) unicodeCanadian_Aboriginal = RleBitSet!uint([
    0x01400, 0x01680,
    0x018b0, 0x018f6,
]);

immutable(RleBitSet!uint) unicodeInGreek_Extended = RleBitSet!uint([
    0x01f00, 0x02000,
]);

immutable(RleBitSet!uint) unicodeInCJK_Unified_Ideographs = RleBitSet!uint([
    0x04e00, 0x0a000,
]);

immutable(RleBitSet!uint) unicodeIDS_Trinary_Operator = RleBitSet!uint([
    0x02ff2, 0x02ff4,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Symbols = RleBitSet!uint([
    0x02600, 0x02700,
]);

immutable(RleBitSet!uint) unicodeLao = RleBitSet!uint([
    0x00e81, 0x00e83,
    0x00e84, 0x00e85,
    0x00e87, 0x00e89,
    0x00e8a, 0x00e8b,
    0x00e8d, 0x00e8e,
    0x00e94, 0x00e98,
    0x00e99, 0x00ea0,
    0x00ea1, 0x00ea4,
    0x00ea5, 0x00ea6,
    0x00ea7, 0x00ea8,
    0x00eaa, 0x00eac,
    0x00ead, 0x00eba,
    0x00ebb, 0x00ebe,
    0x00ec0, 0x00ec5,
    0x00ec6, 0x00ec7,
    0x00ec8, 0x00ece,
    0x00ed0, 0x00eda,
    0x00edc, 0x00ede,
]);

immutable(RleBitSet!uint) unicodeInLatin_Extended_Additional = RleBitSet!uint([
    0x01e00, 0x01f00,
]);

immutable(RleBitSet!uint) unicodeRadical = RleBitSet!uint([
    0x02e80, 0x02e9a,
    0x02e9b, 0x02ef4,
    0x02f00, 0x02fd6,
]);

immutable(RleBitSet!uint) unicodeMongolian = RleBitSet!uint([
    0x01800, 0x01802,
    0x01804, 0x01805,
    0x01806, 0x0180f,
    0x01810, 0x0181a,
    0x01820, 0x01878,
    0x01880, 0x018ab,
]);

immutable(RleBitSet!uint) unicodeInVai = RleBitSet!uint([
    0x0a500, 0x0a640,
]);

immutable(RleBitSet!uint) unicodeBengali = RleBitSet!uint([
    0x00981, 0x00984,
    0x00985, 0x0098d,
    0x0098f, 0x00991,
    0x00993, 0x009a9,
    0x009aa, 0x009b1,
    0x009b2, 0x009b3,
    0x009b6, 0x009ba,
    0x009bc, 0x009c5,
    0x009c7, 0x009c9,
    0x009cb, 0x009cf,
    0x009d7, 0x009d8,
    0x009dc, 0x009de,
    0x009df, 0x009e4,
    0x009e6, 0x009fc,
]);

immutable(RleBitSet!uint) unicodeLatin = RleBitSet!uint([
    0x00041, 0x0005b,
    0x00061, 0x0007b,
    0x000aa, 0x000ab,
    0x000ba, 0x000bb,
    0x000c0, 0x000d7,
    0x000d8, 0x000f7,
    0x000f8, 0x002b9,
    0x002e0, 0x002e5,
    0x01d00, 0x01d26,
    0x01d2c, 0x01d5d,
    0x01d62, 0x01d66,
    0x01d6b, 0x01d78,
    0x01d79, 0x01dbf,
    0x01e00, 0x01f00,
    0x02071, 0x02072,
    0x0207f, 0x02080,
    0x02090, 0x0209d,
    0x0212a, 0x0212c,
    0x02132, 0x02133,
    0x0214e, 0x0214f,
    0x02160, 0x02189,
    0x02c60, 0x02c80,
    0x0a722, 0x0a788,
    0x0a78b, 0x0a78f,
    0x0a790, 0x0a792,
    0x0a7a0, 0x0a7aa,
    0x0a7fa, 0x0a800,
    0x0fb00, 0x0fb07,
    0x0ff21, 0x0ff3b,
    0x0ff41, 0x0ff5b,
]);

immutable(RleBitSet!uint) unicodeTagalog = RleBitSet!uint([
    0x01700, 0x0170d,
    0x0170e, 0x01715,
]);

immutable(RleBitSet!uint) unicodeRejang = RleBitSet!uint([
    0x0a930, 0x0a954,
    0x0a95f, 0x0a960,
]);

immutable(RleBitSet!uint) unicodeInCombining_Diacritical_Marks_for_Symbols = RleBitSet!uint([
    0x020d0, 0x02100,
]);

immutable(RleBitSet!uint) unicodeInSupplemental_Mathematical_Operators = RleBitSet!uint([
    0x02a00, 0x02b00,
]);

immutable(RleBitSet!uint) unicodeInCham = RleBitSet!uint([
    0x0aa00, 0x0aa60,
]);

immutable(RleBitSet!uint) unicodeAvestan = RleBitSet!uint([
    0x10b00, 0x10b36,
    0x10b39, 0x10b40,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Mathematical_Symbols_A = RleBitSet!uint([
    0x027c0, 0x027f0,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Mathematical_Symbols_B = RleBitSet!uint([
    0x02980, 0x02a00,
]);

immutable(RleBitSet!uint) unicodeInTelugu = RleBitSet!uint([
    0x00c00, 0x00c80,
]);

immutable(RleBitSet!uint) unicodeLimbu = RleBitSet!uint([
    0x01900, 0x0191d,
    0x01920, 0x0192c,
    0x01930, 0x0193c,
    0x01940, 0x01941,
    0x01944, 0x01950,
]);

immutable(RleBitSet!uint) unicodeInGeneral_Punctuation = RleBitSet!uint([
    0x02000, 0x02070,
]);

immutable(RleBitSet!uint) unicodeUnified_Ideograph = RleBitSet!uint([
    0x03400, 0x04db6,
    0x04e00, 0x09fcc,
    0x0fa0e, 0x0fa10,
    0x0fa11, 0x0fa12,
    0x0fa13, 0x0fa15,
    0x0fa1f, 0x0fa20,
    0x0fa21, 0x0fa22,
    0x0fa23, 0x0fa25,
    0x0fa27, 0x0fa2a,
    0x20000, 0x2a6d7,
    0x2a700, 0x2b735,
    0x2b740, 0x2b81e,
]);

immutable(RleBitSet!uint) unicodeInPhoenician = RleBitSet!uint([
    0x10900, 0x10920,
]);

immutable(RleBitSet!uint) unicodeOld_South_Arabian = RleBitSet!uint([
    0x10a60, 0x10a80,
]);

immutable(RleBitSet!uint) unicodeInBuhid = RleBitSet!uint([
    0x01740, 0x01760,
]);

immutable(RleBitSet!uint) unicodeKhmer = RleBitSet!uint([
    0x01780, 0x017de,
    0x017e0, 0x017ea,
    0x017f0, 0x017fa,
    0x019e0, 0x01a00,
]);

immutable(RleBitSet!uint) unicodeInLatin_Extended_A = RleBitSet!uint([
    0x00100, 0x00180,
]);

immutable(RleBitSet!uint) unicodeInLatin_Extended_B = RleBitSet!uint([
    0x00180, 0x00250,
]);

immutable(RleBitSet!uint) unicodeInLatin_Extended_C = RleBitSet!uint([
    0x02c60, 0x02c80,
]);

immutable(RleBitSet!uint) unicodeInLatin_Extended_D = RleBitSet!uint([
    0x0a720, 0x0a800,
]);

immutable(RleBitSet!uint) unicodeGurmukhi = RleBitSet!uint([
    0x00a01, 0x00a04,
    0x00a05, 0x00a0b,
    0x00a0f, 0x00a11,
    0x00a13, 0x00a29,
    0x00a2a, 0x00a31,
    0x00a32, 0x00a34,
    0x00a35, 0x00a37,
    0x00a38, 0x00a3a,
    0x00a3c, 0x00a3d,
    0x00a3e, 0x00a43,
    0x00a47, 0x00a49,
    0x00a4b, 0x00a4e,
    0x00a51, 0x00a52,
    0x00a59, 0x00a5d,
    0x00a5e, 0x00a5f,
    0x00a66, 0x00a76,
]);

immutable(RleBitSet!uint) unicodeInOsmanya = RleBitSet!uint([
    0x10480, 0x104b0,
]);

immutable(RleBitSet!uint) unicodeInCJK_Compatibility = RleBitSet!uint([
    0x03300, 0x03400,
]);

immutable(RleBitSet!uint) unicodeOriya = RleBitSet!uint([
    0x00b01, 0x00b04,
    0x00b05, 0x00b0d,
    0x00b0f, 0x00b11,
    0x00b13, 0x00b29,
    0x00b2a, 0x00b31,
    0x00b32, 0x00b34,
    0x00b35, 0x00b3a,
    0x00b3c, 0x00b45,
    0x00b47, 0x00b49,
    0x00b4b, 0x00b4e,
    0x00b56, 0x00b58,
    0x00b5c, 0x00b5e,
    0x00b5f, 0x00b64,
    0x00b66, 0x00b78,
]);

immutable(RleBitSet!uint) unicodeBuginese = RleBitSet!uint([
    0x01a00, 0x01a1c,
    0x01a1e, 0x01a20,
]);

immutable(RleBitSet!uint) unicodeInGeorgian_Supplement = RleBitSet!uint([
    0x02d00, 0x02d30,
]);

immutable(RleBitSet!uint) unicodeInCJK_Strokes = RleBitSet!uint([
    0x031c0, 0x031f0,
]);

immutable(RleBitSet!uint) unicodeVai = RleBitSet!uint([
    0x0a500, 0x0a62c,
]);

immutable(RleBitSet!uint) unicodeHangul = RleBitSet!uint([
    0x01100, 0x01200,
    0x0302e, 0x03030,
    0x03131, 0x0318f,
    0x03200, 0x0321f,
    0x03260, 0x0327f,
    0x0a960, 0x0a97d,
    0x0ac00, 0x0d7a4,
    0x0d7b0, 0x0d7c7,
    0x0d7cb, 0x0d7fc,
    0x0ffa0, 0x0ffbf,
    0x0ffc2, 0x0ffc8,
    0x0ffca, 0x0ffd0,
    0x0ffd2, 0x0ffd8,
    0x0ffda, 0x0ffdd,
]);

immutable(RleBitSet!uint) unicodeInRejang = RleBitSet!uint([
    0x0a930, 0x0a960,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Technical = RleBitSet!uint([
    0x02300, 0x02400,
]);

immutable(RleBitSet!uint) unicodeInTransport_And_Map_Symbols = RleBitSet!uint([
    0x1f680, 0x1f700,
]);

immutable(RleBitSet!uint) unicodeHyphen = RleBitSet!uint([
    0x0002d, 0x0002e,
    0x000ad, 0x000ae,
    0x0058a, 0x0058b,
    0x01806, 0x01807,
    0x02010, 0x02012,
    0x02e17, 0x02e18,
    0x030fb, 0x030fc,
    0x0fe63, 0x0fe64,
    0x0ff0d, 0x0ff0e,
    0x0ff65, 0x0ff66,
]);

immutable(RleBitSet!uint) unicodeThai = RleBitSet!uint([
    0x00e01, 0x00e3b,
    0x00e40, 0x00e5c,
]);

immutable(RleBitSet!uint) unicodeSundanese = RleBitSet!uint([
    0x01b80, 0x01bab,
    0x01bae, 0x01bba,
]);

immutable(RleBitSet!uint) unicodeInRunic = RleBitSet!uint([
    0x016a0, 0x01700,
]);

immutable(RleBitSet!uint) unicodeDiacritic = RleBitSet!uint([
    0x0005e, 0x0005f,
    0x00060, 0x00061,
    0x000a8, 0x000a9,
    0x000af, 0x000b0,
    0x000b4, 0x000b5,
    0x000b7, 0x000b9,
    0x002b0, 0x0034f,
    0x00350, 0x00358,
    0x0035d, 0x00363,
    0x00374, 0x00376,
    0x0037a, 0x0037b,
    0x00384, 0x00386,
    0x00483, 0x00488,
    0x00559, 0x0055a,
    0x00591, 0x005a2,
    0x005a3, 0x005be,
    0x005bf, 0x005c0,
    0x005c1, 0x005c3,
    0x005c4, 0x005c5,
    0x0064b, 0x00653,
    0x00657, 0x00659,
    0x006df, 0x006e1,
    0x006e5, 0x006e7,
    0x006ea, 0x006ed,
    0x00730, 0x0074b,
    0x007a6, 0x007b1,
    0x007eb, 0x007f6,
    0x00818, 0x0081a,
    0x0093c, 0x0093d,
    0x0094d, 0x0094e,
    0x00951, 0x00955,
    0x00971, 0x00972,
    0x009bc, 0x009bd,
    0x009cd, 0x009ce,
    0x00a3c, 0x00a3d,
    0x00a4d, 0x00a4e,
    0x00abc, 0x00abd,
    0x00acd, 0x00ace,
    0x00b3c, 0x00b3d,
    0x00b4d, 0x00b4e,
    0x00bcd, 0x00bce,
    0x00c4d, 0x00c4e,
    0x00cbc, 0x00cbd,
    0x00ccd, 0x00cce,
    0x00d4d, 0x00d4e,
    0x00dca, 0x00dcb,
    0x00e47, 0x00e4d,
    0x00e4e, 0x00e4f,
    0x00ec8, 0x00ecd,
    0x00f18, 0x00f1a,
    0x00f35, 0x00f36,
    0x00f37, 0x00f38,
    0x00f39, 0x00f3a,
    0x00f3e, 0x00f40,
    0x00f82, 0x00f85,
    0x00f86, 0x00f88,
    0x00fc6, 0x00fc7,
    0x01037, 0x01038,
    0x01039, 0x0103b,
    0x01087, 0x0108e,
    0x0108f, 0x01090,
    0x0109a, 0x0109c,
    0x017c9, 0x017d4,
    0x017dd, 0x017de,
    0x01939, 0x0193c,
    0x01a75, 0x01a7d,
    0x01a7f, 0x01a80,
    0x01b34, 0x01b35,
    0x01b44, 0x01b45,
    0x01b6b, 0x01b74,
    0x01baa, 0x01bab,
    0x01c36, 0x01c38,
    0x01c78, 0x01c7e,
    0x01cd0, 0x01ce9,
    0x01ced, 0x01cee,
    0x01d2c, 0x01d6b,
    0x01dc4, 0x01dd0,
    0x01dfd, 0x01e00,
    0x01fbd, 0x01fbe,
    0x01fbf, 0x01fc2,
    0x01fcd, 0x01fd0,
    0x01fdd, 0x01fe0,
    0x01fed, 0x01ff0,
    0x01ffd, 0x01fff,
    0x02cef, 0x02cf2,
    0x02e2f, 0x02e30,
    0x0302a, 0x03030,
    0x03099, 0x0309d,
    0x030fc, 0x030fd,
    0x0a66f, 0x0a670,
    0x0a67c, 0x0a67e,
    0x0a67f, 0x0a680,
    0x0a6f0, 0x0a6f2,
    0x0a717, 0x0a722,
    0x0a788, 0x0a789,
    0x0a8c4, 0x0a8c5,
    0x0a8e0, 0x0a8f2,
    0x0a92b, 0x0a92f,
    0x0a953, 0x0a954,
    0x0a9b3, 0x0a9b4,
    0x0a9c0, 0x0a9c1,
    0x0aa7b, 0x0aa7c,
    0x0aabf, 0x0aac3,
    0x0abec, 0x0abee,
    0x0fb1e, 0x0fb1f,
    0x0fe20, 0x0fe27,
    0x0ff3e, 0x0ff3f,
    0x0ff40, 0x0ff41,
    0x0ff70, 0x0ff71,
    0x0ff9e, 0x0ffa0,
    0x0ffe3, 0x0ffe4,
    0x110b9, 0x110bb,
    0x1d167, 0x1d16a,
    0x1d16d, 0x1d173,
    0x1d17b, 0x1d183,
    0x1d185, 0x1d18c,
    0x1d1aa, 0x1d1ae,
]);

immutable(RleBitSet!uint) unicodeYi = RleBitSet!uint([
    0x0a000, 0x0a48d,
    0x0a490, 0x0a4c7,
]);

immutable(RleBitSet!uint) unicodeInAlphabetic_Presentation_Forms = RleBitSet!uint([
    0x0fb00, 0x0fb50,
]);

immutable(RleBitSet!uint) unicodeExtender = RleBitSet!uint([
    0x000b7, 0x000b8,
    0x002d0, 0x002d2,
    0x00640, 0x00641,
    0x007fa, 0x007fb,
    0x00e46, 0x00e47,
    0x00ec6, 0x00ec7,
    0x01843, 0x01844,
    0x01aa7, 0x01aa8,
    0x01c36, 0x01c37,
    0x01c7b, 0x01c7c,
    0x03005, 0x03006,
    0x03031, 0x03036,
    0x0309d, 0x0309f,
    0x030fc, 0x030ff,
    0x0a015, 0x0a016,
    0x0a60c, 0x0a60d,
    0x0a9cf, 0x0a9d0,
    0x0aa70, 0x0aa71,
    0x0aadd, 0x0aade,
    0x0ff70, 0x0ff71,
]);

immutable(RleBitSet!uint) unicodeGlagolitic = RleBitSet!uint([
    0x02c00, 0x02c2f,
    0x02c30, 0x02c5f,
]);

immutable(RleBitSet!uint) unicodeInSuperscripts_and_Subscripts = RleBitSet!uint([
    0x02070, 0x020a0,
]);

immutable(RleBitSet!uint) unicodeInMalayalam = RleBitSet!uint([
    0x00d00, 0x00d80,
]);

immutable(RleBitSet!uint) unicodeJoin_Control = RleBitSet!uint([
    0x0200c, 0x0200e,
]);

immutable(RleBitSet!uint) unicodeInBatak = RleBitSet!uint([
    0x01bc0, 0x01c00,
]);

immutable(RleBitSet!uint) unicodeThaana = RleBitSet!uint([
    0x00780, 0x007b2,
]);

immutable(RleBitSet!uint) unicodeSoft_Dotted = RleBitSet!uint([
    0x00069, 0x0006b,
    0x0012f, 0x00130,
    0x00249, 0x0024a,
    0x00268, 0x00269,
    0x0029d, 0x0029e,
    0x002b2, 0x002b3,
    0x003f3, 0x003f4,
    0x00456, 0x00457,
    0x00458, 0x00459,
    0x01d62, 0x01d63,
    0x01d96, 0x01d97,
    0x01da4, 0x01da5,
    0x01da8, 0x01da9,
    0x01e2d, 0x01e2e,
    0x01ecb, 0x01ecc,
    0x02071, 0x02072,
    0x02148, 0x0214a,
    0x02c7c, 0x02c7d,
    0x1d422, 0x1d424,
    0x1d456, 0x1d458,
    0x1d48a, 0x1d48c,
    0x1d4be, 0x1d4c0,
    0x1d4f2, 0x1d4f4,
    0x1d526, 0x1d528,
    0x1d55a, 0x1d55c,
    0x1d58e, 0x1d590,
    0x1d5c2, 0x1d5c4,
    0x1d5f6, 0x1d5f8,
    0x1d62a, 0x1d62c,
    0x1d65e, 0x1d660,
    0x1d692, 0x1d694,
]);

immutable(RleBitSet!uint) unicodeBraille = RleBitSet!uint([
    0x02800, 0x02900,
]);

immutable(RleBitSet!uint) unicodeInGurmukhi = RleBitSet!uint([
    0x00a00, 0x00a80,
]);

immutable(RleBitSet!uint) unicodeMandaic = RleBitSet!uint([
    0x00840, 0x0085c,
    0x0085e, 0x0085f,
]);

immutable(RleBitSet!uint) unicodeInUnified_Canadian_Aboriginal_Syllabics = RleBitSet!uint([
    0x01400, 0x01680,
]);

immutable(RleBitSet!uint) unicodeInDingbats = RleBitSet!uint([
    0x02700, 0x027c0,
]);

immutable(RleBitSet!uint) unicodeZl = RleBitSet!uint([
    0x02028, 0x02029,
]);

immutable(RleBitSet!uint) unicodeInMusical_Symbols = RleBitSet!uint([
    0x1d100, 0x1d200,
]);

immutable(RleBitSet!uint) unicodeInBuginese = RleBitSet!uint([
    0x01a00, 0x01a20,
]);

immutable(RleBitSet!uint) unicodeInSaurashtra = RleBitSet!uint([
    0x0a880, 0x0a8e0,
]);

immutable(RleBitSet!uint) unicodeInCJK_Symbols_and_Punctuation = RleBitSet!uint([
    0x03000, 0x03040,
]);

immutable(RleBitSet!uint) unicodeInKangxi_Radicals = RleBitSet!uint([
    0x02f00, 0x02fe0,
]);

immutable(RleBitSet!uint) unicodeZp = RleBitSet!uint([
    0x02029, 0x0202a,
]);

immutable(RleBitSet!uint) unicodeHex_Digit = RleBitSet!uint([
    0x00030, 0x0003a,
    0x00041, 0x00047,
    0x00061, 0x00067,
    0x0ff10, 0x0ff1a,
    0x0ff21, 0x0ff27,
    0x0ff41, 0x0ff47,
]);

immutable(RleBitSet!uint) unicodeZs = RleBitSet!uint([
    0x00020, 0x00021,
    0x000a0, 0x000a1,
    0x01680, 0x01681,
    0x0180e, 0x0180f,
    0x02000, 0x0200b,
    0x0202f, 0x02030,
    0x0205f, 0x02060,
    0x03000, 0x03001,
]);

immutable(RleBitSet!uint) unicodeInThai = RleBitSet!uint([
    0x00e00, 0x00e80,
]);

immutable(RleBitSet!uint) unicodeDevanagari = RleBitSet!uint([
    0x00900, 0x00951,
    0x00953, 0x00964,
    0x00966, 0x00970,
    0x00971, 0x00978,
    0x00979, 0x00980,
    0x0a8e0, 0x0a8fc,
]);

immutable(RleBitSet!uint) unicodeInArrows = RleBitSet!uint([
    0x02190, 0x02200,
]);

immutable(RleBitSet!uint) unicodeEthiopic = RleBitSet!uint([
    0x01200, 0x01249,
    0x0124a, 0x0124e,
    0x01250, 0x01257,
    0x01258, 0x01259,
    0x0125a, 0x0125e,
    0x01260, 0x01289,
    0x0128a, 0x0128e,
    0x01290, 0x012b1,
    0x012b2, 0x012b6,
    0x012b8, 0x012bf,
    0x012c0, 0x012c1,
    0x012c2, 0x012c6,
    0x012c8, 0x012d7,
    0x012d8, 0x01311,
    0x01312, 0x01316,
    0x01318, 0x0135b,
    0x0135d, 0x0137d,
    0x01380, 0x0139a,
    0x02d80, 0x02d97,
    0x02da0, 0x02da7,
    0x02da8, 0x02daf,
    0x02db0, 0x02db7,
    0x02db8, 0x02dbf,
    0x02dc0, 0x02dc7,
    0x02dc8, 0x02dcf,
    0x02dd0, 0x02dd7,
    0x02dd8, 0x02ddf,
    0x0ab01, 0x0ab07,
    0x0ab09, 0x0ab0f,
    0x0ab11, 0x0ab17,
    0x0ab20, 0x0ab27,
    0x0ab28, 0x0ab2f,
]);

immutable(RleBitSet!uint) unicodeInCurrency_Symbols = RleBitSet!uint([
    0x020a0, 0x020d0,
]);

immutable(RleBitSet!uint) unicodeInOld_Persian = RleBitSet!uint([
    0x103a0, 0x103e0,
]);

immutable(RleBitSet!uint) unicodeInRumi_Numeral_Symbols = RleBitSet!uint([
    0x10e60, 0x10e80,
]);

immutable(RleBitSet!uint) unicodeInTags = RleBitSet!uint([
    0xe0000, 0xe0080,
]);

immutable(RleBitSet!uint) unicodeGreek = RleBitSet!uint([
    0x00370, 0x00374,
    0x00375, 0x00378,
    0x0037a, 0x0037e,
    0x00384, 0x00385,
    0x00386, 0x00387,
    0x00388, 0x0038b,
    0x0038c, 0x0038d,
    0x0038e, 0x003a2,
    0x003a3, 0x003e2,
    0x003f0, 0x00400,
    0x01d26, 0x01d2b,
    0x01d5d, 0x01d62,
    0x01d66, 0x01d6b,
    0x01dbf, 0x01dc0,
    0x01f00, 0x01f16,
    0x01f18, 0x01f1e,
    0x01f20, 0x01f46,
    0x01f48, 0x01f4e,
    0x01f50, 0x01f58,
    0x01f59, 0x01f5a,
    0x01f5b, 0x01f5c,
    0x01f5d, 0x01f5e,
    0x01f5f, 0x01f7e,
    0x01f80, 0x01fb5,
    0x01fb6, 0x01fc5,
    0x01fc6, 0x01fd4,
    0x01fd6, 0x01fdc,
    0x01fdd, 0x01ff0,
    0x01ff2, 0x01ff5,
    0x01ff6, 0x01fff,
    0x02126, 0x02127,
    0x10140, 0x1018b,
    0x1d200, 0x1d246,
]);

immutable(RleBitSet!uint) unicodeOl_Chiki = RleBitSet!uint([
    0x01c50, 0x01c80,
]);

immutable(RleBitSet!uint) unicodeTagbanwa = RleBitSet!uint([
    0x01760, 0x0176d,
    0x0176e, 0x01771,
    0x01772, 0x01774,
]);

immutable(RleBitSet!uint) unicodeOther_Uppercase = RleBitSet!uint([
    0x02160, 0x02170,
    0x024b6, 0x024d0,
]);

immutable(RleBitSet!uint) unicodeInOgham = RleBitSet!uint([
    0x01680, 0x016a0,
]);

immutable(RleBitSet!uint) unicodeInThaana = RleBitSet!uint([
    0x00780, 0x007c0,
]);

immutable(RleBitSet!uint) unicodeInEnclosed_Ideographic_Supplement = RleBitSet!uint([
    0x1f200, 0x1f300,
]);

immutable(RleBitSet!uint) unicodeInKhmer_Symbols = RleBitSet!uint([
    0x019e0, 0x01a00,
]);

immutable(RleBitSet!uint) unicodeCommon = RleBitSet!uint([
    0x00000, 0x00041,
    0x0005b, 0x00061,
    0x0007b, 0x000aa,
    0x000ab, 0x000ba,
    0x000bb, 0x000c0,
    0x000d7, 0x000d8,
    0x000f7, 0x000f8,
    0x002b9, 0x002e0,
    0x002e5, 0x002ea,
    0x002ec, 0x00300,
    0x00374, 0x00375,
    0x0037e, 0x0037f,
    0x00385, 0x00386,
    0x00387, 0x00388,
    0x00589, 0x0058a,
    0x0060c, 0x0060d,
    0x0061b, 0x0061c,
    0x0061f, 0x00620,
    0x00640, 0x00641,
    0x00660, 0x0066a,
    0x006dd, 0x006de,
    0x00964, 0x00966,
    0x00970, 0x00971,
    0x00e3f, 0x00e40,
    0x00fd5, 0x00fd9,
    0x010fb, 0x010fc,
    0x016eb, 0x016ee,
    0x01735, 0x01737,
    0x01802, 0x01804,
    0x01805, 0x01806,
    0x01cd3, 0x01cd4,
    0x01ce1, 0x01ce2,
    0x01ce9, 0x01ced,
    0x01cee, 0x01cf3,
    0x02000, 0x0200c,
    0x0200e, 0x02065,
    0x0206a, 0x02071,
    0x02074, 0x0207f,
    0x02080, 0x0208f,
    0x020a0, 0x020ba,
    0x02100, 0x02126,
    0x02127, 0x0212a,
    0x0212c, 0x02132,
    0x02133, 0x0214e,
    0x0214f, 0x02160,
    0x02189, 0x0218a,
    0x02190, 0x023f4,
    0x02400, 0x02427,
    0x02440, 0x0244b,
    0x02460, 0x02700,
    0x02701, 0x027cb,
    0x027cc, 0x027cd,
    0x027ce, 0x02800,
    0x02900, 0x02b4d,
    0x02b50, 0x02b5a,
    0x02e00, 0x02e32,
    0x02ff0, 0x02ffc,
    0x03000, 0x03005,
    0x03006, 0x03007,
    0x03008, 0x03021,
    0x03030, 0x03038,
    0x0303c, 0x03040,
    0x0309b, 0x0309d,
    0x030a0, 0x030a1,
    0x030fb, 0x030fd,
    0x03190, 0x031a0,
    0x031c0, 0x031e4,
    0x03220, 0x03260,
    0x0327f, 0x032d0,
    0x03358, 0x03400,
    0x04dc0, 0x04e00,
    0x0a700, 0x0a722,
    0x0a788, 0x0a78b,
    0x0a830, 0x0a83a,
    0x0fd3e, 0x0fd40,
    0x0fdfd, 0x0fdfe,
    0x0fe10, 0x0fe1a,
    0x0fe30, 0x0fe53,
    0x0fe54, 0x0fe67,
    0x0fe68, 0x0fe6c,
    0x0feff, 0x0ff00,
    0x0ff01, 0x0ff21,
    0x0ff3b, 0x0ff41,
    0x0ff5b, 0x0ff66,
    0x0ff70, 0x0ff71,
    0x0ff9e, 0x0ffa0,
    0x0ffe0, 0x0ffe7,
    0x0ffe8, 0x0ffef,
    0x0fff9, 0x0fffe,
    0x10100, 0x10103,
    0x10107, 0x10134,
    0x10137, 0x10140,
    0x10190, 0x1019c,
    0x101d0, 0x101fd,
    0x1d000, 0x1d0f6,
    0x1d100, 0x1d127,
    0x1d129, 0x1d167,
    0x1d16a, 0x1d17b,
    0x1d183, 0x1d185,
    0x1d18c, 0x1d1aa,
    0x1d1ae, 0x1d1de,
    0x1d300, 0x1d357,
    0x1d360, 0x1d372,
    0x1d400, 0x1d455,
    0x1d456, 0x1d49d,
    0x1d49e, 0x1d4a0,
    0x1d4a2, 0x1d4a3,
    0x1d4a5, 0x1d4a7,
    0x1d4a9, 0x1d4ad,
    0x1d4ae, 0x1d4ba,
    0x1d4bb, 0x1d4bc,
    0x1d4bd, 0x1d4c4,
    0x1d4c5, 0x1d506,
    0x1d507, 0x1d50b,
    0x1d50d, 0x1d515,
    0x1d516, 0x1d51d,
    0x1d51e, 0x1d53a,
    0x1d53b, 0x1d53f,
    0x1d540, 0x1d545,
    0x1d546, 0x1d547,
    0x1d54a, 0x1d551,
    0x1d552, 0x1d6a6,
    0x1d6a8, 0x1d7cc,
    0x1d7ce, 0x1d800,
    0x1f000, 0x1f02c,
    0x1f030, 0x1f094,
    0x1f0a0, 0x1f0af,
    0x1f0b1, 0x1f0bf,
    0x1f0c1, 0x1f0d0,
    0x1f0d1, 0x1f0e0,
    0x1f100, 0x1f10b,
    0x1f110, 0x1f12f,
    0x1f130, 0x1f16a,
    0x1f170, 0x1f19b,
    0x1f1e6, 0x1f200,
    0x1f201, 0x1f203,
    0x1f210, 0x1f23b,
    0x1f240, 0x1f249,
    0x1f250, 0x1f252,
    0x1f300, 0x1f321,
    0x1f330, 0x1f336,
    0x1f337, 0x1f37d,
    0x1f380, 0x1f394,
    0x1f3a0, 0x1f3c5,
    0x1f3c6, 0x1f3cb,
    0x1f3e0, 0x1f3f1,
    0x1f400, 0x1f43f,
    0x1f440, 0x1f441,
    0x1f442, 0x1f4f8,
    0x1f4f9, 0x1f4fd,
    0x1f500, 0x1f53e,
    0x1f550, 0x1f568,
    0x1f5fb, 0x1f600,
    0x1f601, 0x1f611,
    0x1f612, 0x1f615,
    0x1f616, 0x1f617,
    0x1f618, 0x1f619,
    0x1f61a, 0x1f61b,
    0x1f61c, 0x1f61f,
    0x1f620, 0x1f626,
    0x1f628, 0x1f62c,
    0x1f62d, 0x1f62e,
    0x1f630, 0x1f634,
    0x1f635, 0x1f641,
    0x1f645, 0x1f650,
    0x1f680, 0x1f6c6,
    0x1f700, 0x1f774,
    0xe0001, 0xe0002,
    0xe0020, 0xe0080,
]);

immutable(RleBitSet!uint) unicodeInCommon_Indic_Number_Forms = RleBitSet!uint([
    0x0a830, 0x0a840,
]);

immutable(RleBitSet!uint) unicodeInSmall_Form_Variants = RleBitSet!uint([
    0x0fe50, 0x0fe70,
]);

immutable(RleBitSet!uint) unicodeIdeographic = RleBitSet!uint([
    0x03006, 0x03008,
    0x03021, 0x0302a,
    0x03038, 0x0303b,
    0x03400, 0x04db6,
    0x04e00, 0x09fcc,
    0x0f900, 0x0fa2e,
    0x0fa30, 0x0fa6e,
    0x0fa70, 0x0fada,
    0x20000, 0x2a6d7,
    0x2a700, 0x2b735,
    0x2b740, 0x2b81e,
    0x2f800, 0x2fa1e,
]);

immutable(RleBitSet!uint) unicodeGeorgian = RleBitSet!uint([
    0x010a0, 0x010c6,
    0x010d0, 0x010fb,
    0x010fc, 0x010fd,
    0x02d00, 0x02d26,
]);

immutable(RleBitSet!uint) unicodeOsmanya = RleBitSet!uint([
    0x10480, 0x1049e,
    0x104a0, 0x104aa,
]);

immutable(RleBitSet!uint) unicodeInEthiopic = RleBitSet!uint([
    0x01200, 0x01380,
]);

immutable(RleBitSet!uint) unicodeInEnclosed_Alphanumerics = RleBitSet!uint([
    0x02460, 0x02500,
]);

immutable(RleBitSet!uint) unicodeCuneiform = RleBitSet!uint([
    0x12000, 0x1236f,
    0x12400, 0x12463,
    0x12470, 0x12474,
]);

immutable(RleBitSet!uint) unicodeSyriac = RleBitSet!uint([
    0x00700, 0x0070e,
    0x0070f, 0x0074b,
    0x0074d, 0x00750,
]);

immutable(RleBitSet!uint) unicodeInVertical_Forms = RleBitSet!uint([
    0x0fe10, 0x0fe20,
]);

immutable(RleBitSet!uint) unicodeInTai_Xuan_Jing_Symbols = RleBitSet!uint([
    0x1d300, 0x1d360,
]);

immutable(RleBitSet!uint) unicodeNoncharacter_Code_Point = RleBitSet!uint([
    0x0fdd0, 0x0fdf0,
    0x0fffe, 0x10000,
    0x1fffe, 0x20000,
    0x2fffe, 0x30000,
    0x3fffe, 0x40000,
    0x4fffe, 0x50000,
    0x5fffe, 0x60000,
    0x6fffe, 0x70000,
    0x7fffe, 0x80000,
    0x8fffe, 0x90000,
    0x9fffe, 0xa0000,
    0xafffe, 0xb0000,
    0xbfffe, 0xc0000,
    0xcfffe, 0xd0000,
    0xdfffe, 0xe0000,
    0xefffe, 0xf0000,
    0xffffe, 0x100000,
    0x10fffe, 0x110000,
]);

immutable(RleBitSet!uint) unicodeInMiscellaneous_Symbols_and_Arrows = RleBitSet!uint([
    0x02b00, 0x02c00,
]);

immutable(RleBitSet!uint) unicodeInOld_Italic = RleBitSet!uint([
    0x10300, 0x10330,
]);

immutable(RleBitSet!uint) unicodeMeetei_Mayek = RleBitSet!uint([
    0x0abc0, 0x0abee,
    0x0abf0, 0x0abfa,
]);

immutable(RleBitSet!uint) unicodeOther_Grapheme_Extend = RleBitSet!uint([
    0x009be, 0x009bf,
    0x009d7, 0x009d8,
    0x00b3e, 0x00b3f,
    0x00b57, 0x00b58,
    0x00bbe, 0x00bbf,
    0x00bd7, 0x00bd8,
    0x00cc2, 0x00cc3,
    0x00cd5, 0x00cd7,
    0x00d3e, 0x00d3f,
    0x00d57, 0x00d58,
    0x00dcf, 0x00dd0,
    0x00ddf, 0x00de0,
    0x0200c, 0x0200e,
    0x0ff9e, 0x0ffa0,
    0x1d165, 0x1d166,
    0x1d16e, 0x1d173,
]);

immutable(RleBitSet!uint) unicodeOther_Math = RleBitSet!uint([
    0x0005e, 0x0005f,
    0x003d0, 0x003d3,
    0x003d5, 0x003d6,
    0x003f0, 0x003f2,
    0x003f4, 0x003f6,
    0x02016, 0x02017,
    0x02032, 0x02035,
    0x02040, 0x02041,
    0x02061, 0x02065,
    0x0207d, 0x0207f,
    0x0208d, 0x0208f,
    0x020d0, 0x020dd,
    0x020e1, 0x020e2,
    0x020e5, 0x020e7,
    0x020eb, 0x020f0,
    0x02102, 0x02103,
    0x02107, 0x02108,
    0x0210a, 0x02114,
    0x02115, 0x02116,
    0x02119, 0x0211e,
    0x02124, 0x02125,
    0x02128, 0x0212a,
    0x0212c, 0x0212e,
    0x0212f, 0x02132,
    0x02133, 0x02139,
    0x0213c, 0x02140,
    0x02145, 0x0214a,
    0x02195, 0x0219a,
    0x0219c, 0x021a0,
    0x021a1, 0x021a3,
    0x021a4, 0x021a6,
    0x021a7, 0x021a8,
    0x021a9, 0x021ae,
    0x021b0, 0x021b2,
    0x021b6, 0x021b8,
    0x021bc, 0x021ce,
    0x021d0, 0x021d2,
    0x021d3, 0x021d4,
    0x021d5, 0x021dc,
    0x021dd, 0x021de,
    0x021e4, 0x021e6,
    0x023b4, 0x023b6,
    0x023b7, 0x023b8,
    0x023d0, 0x023d1,
    0x023e2, 0x023e3,
    0x025a0, 0x025a2,
    0x025ae, 0x025b7,
    0x025bc, 0x025c1,
    0x025c6, 0x025c8,
    0x025ca, 0x025cc,
    0x025cf, 0x025d4,
    0x025e2, 0x025e3,
    0x025e4, 0x025e5,
    0x025e7, 0x025ed,
    0x02605, 0x02607,
    0x02640, 0x02641,
    0x02642, 0x02643,
    0x02660, 0x02664,
    0x0266d, 0x0266f,
    0x027c5, 0x027c7,
    0x027e6, 0x027f0,
    0x02983, 0x02999,
    0x029d8, 0x029dc,
    0x029fc, 0x029fe,
    0x0fe61, 0x0fe62,
    0x0fe63, 0x0fe64,
    0x0fe68, 0x0fe69,
    0x0ff3c, 0x0ff3d,
    0x0ff3e, 0x0ff3f,
    0x1d400, 0x1d455,
    0x1d456, 0x1d49d,
    0x1d49e, 0x1d4a0,
    0x1d4a2, 0x1d4a3,
    0x1d4a5, 0x1d4a7,
    0x1d4a9, 0x1d4ad,
    0x1d4ae, 0x1d4ba,
    0x1d4bb, 0x1d4bc,
    0x1d4bd, 0x1d4c4,
    0x1d4c5, 0x1d506,
    0x1d507, 0x1d50b,
    0x1d50d, 0x1d515,
    0x1d516, 0x1d51d,
    0x1d51e, 0x1d53a,
    0x1d53b, 0x1d53f,
    0x1d540, 0x1d545,
    0x1d546, 0x1d547,
    0x1d54a, 0x1d551,
    0x1d552, 0x1d6a6,
    0x1d6a8, 0x1d6c1,
    0x1d6c2, 0x1d6db,
    0x1d6dc, 0x1d6fb,
    0x1d6fc, 0x1d715,
    0x1d716, 0x1d735,
    0x1d736, 0x1d74f,
    0x1d750, 0x1d76f,
    0x1d770, 0x1d789,
    0x1d78a, 0x1d7a9,
    0x1d7aa, 0x1d7c3,
    0x1d7c4, 0x1d7cc,
    0x1d7ce, 0x1d800,
]);

immutable(RleBitSet!uint) unicodeGujarati = RleBitSet!uint([
    0x00a81, 0x00a84,
    0x00a85, 0x00a8e,
    0x00a8f, 0x00a92,
    0x00a93, 0x00aa9,
    0x00aaa, 0x00ab1,
    0x00ab2, 0x00ab4,
    0x00ab5, 0x00aba,
    0x00abc, 0x00ac6,
    0x00ac7, 0x00aca,
    0x00acb, 0x00ace,
    0x00ad0, 0x00ad1,
    0x00ae0, 0x00ae4,
    0x00ae6, 0x00af0,
    0x00af1, 0x00af2,
]);

immutable(RleBitSet!uint) unicodeInBox_Drawing = RleBitSet!uint([
    0x02500, 0x02580,
]);

immutable(RleBitSet!uint) unicodeInTagbanwa = RleBitSet!uint([
    0x01760, 0x01780,
]);

immutable(RleBitSet!uint) unicodeInHangul_Syllables = RleBitSet!uint([
    0x0ac00, 0x0d7b0,
]);

immutable(RleBitSet!uint) unicodeLydian = RleBitSet!uint([
    0x10920, 0x1093a,
    0x1093f, 0x10940,
]);

immutable(RleBitSet!uint) unicodeInKatakana_Phonetic_Extensions = RleBitSet!uint([
    0x031f0, 0x03200,
]);

immutable(RleBitSet!uint) unicodeInTai_Tham = RleBitSet!uint([
    0x01a20, 0x01ab0,
]);

immutable(RleBitSet!uint) unicodeInNumber_Forms = RleBitSet!uint([
    0x02150, 0x02190,
]);

immutable(RleBitSet!uint) unicodeInBopomofo_Extended = RleBitSet!uint([
    0x031a0, 0x031c0,
]);

immutable(RleBitSet!uint) unicodeInherited = RleBitSet!uint([
    0x00300, 0x00370,
    0x00485, 0x00487,
    0x0064b, 0x00656,
    0x0065f, 0x00660,
    0x00670, 0x00671,
    0x00951, 0x00953,
    0x01cd0, 0x01cd3,
    0x01cd4, 0x01ce1,
    0x01ce2, 0x01ce9,
    0x01ced, 0x01cee,
    0x01dc0, 0x01de7,
    0x01dfc, 0x01e00,
    0x0200c, 0x0200e,
    0x020d0, 0x020f1,
    0x0302a, 0x0302e,
    0x03099, 0x0309b,
    0x0fe00, 0x0fe10,
    0x0fe20, 0x0fe27,
    0x101fd, 0x101fe,
    0x1d167, 0x1d16a,
    0x1d17b, 0x1d183,
    0x1d185, 0x1d18c,
    0x1d1aa, 0x1d1ae,
    0xe0100, 0xe01f0,
]);

immutable(RleBitSet!uint) unicodeInLetterlike_Symbols = RleBitSet!uint([
    0x02100, 0x02150,
]);

immutable(RleBitSet!uint) unicodeBuhid = RleBitSet!uint([
    0x01740, 0x01754,
]);

immutable(RleBitSet!uint) unicodeInCypriot_Syllabary = RleBitSet!uint([
    0x10800, 0x10840,
]);

immutable(RleBitSet!uint) unicodeInSyloti_Nagri = RleBitSet!uint([
    0x0a800, 0x0a830,
]);

immutable(RleBitSet!uint) unicodeInSamaritan = RleBitSet!uint([
    0x00800, 0x00840,
]);

immutable(RleBitSet!uint) unicodeInGeorgian = RleBitSet!uint([
    0x010a0, 0x01100,
]);

immutable(RleBitSet!uint) unicodeAlphabetic = RleBitSet!uint([
    0x00041, 0x0005b,
    0x00061, 0x0007b,
    0x000aa, 0x000ab,
    0x000b5, 0x000b6,
    0x000ba, 0x000bb,
    0x000c0, 0x000d7,
    0x000d8, 0x000f7,
    0x000f8, 0x002c2,
    0x002c6, 0x002d2,
    0x002e0, 0x002e5,
    0x002ec, 0x002ed,
    0x002ee, 0x002ef,
    0x00345, 0x00346,
    0x00370, 0x00375,
    0x00376, 0x00378,
    0x0037a, 0x0037e,
    0x00386, 0x00387,
    0x00388, 0x0038b,
    0x0038c, 0x0038d,
    0x0038e, 0x003a2,
    0x003a3, 0x003f6,
    0x003f7, 0x00482,
    0x0048a, 0x00528,
    0x00531, 0x00557,
    0x00559, 0x0055a,
    0x00561, 0x00588,
    0x005b0, 0x005be,
    0x005bf, 0x005c0,
    0x005c1, 0x005c3,
    0x005c4, 0x005c6,
    0x005c7, 0x005c8,
    0x005d0, 0x005eb,
    0x005f0, 0x005f3,
    0x00610, 0x0061b,
    0x00620, 0x00658,
    0x00659, 0x00660,
    0x0066e, 0x006d4,
    0x006d5, 0x006dd,
    0x006e1, 0x006e9,
    0x006ed, 0x006f0,
    0x006fa, 0x006fd,
    0x006ff, 0x00700,
    0x00710, 0x00740,
    0x0074d, 0x007b2,
    0x007ca, 0x007eb,
    0x007f4, 0x007f6,
    0x007fa, 0x007fb,
    0x00800, 0x00818,
    0x0081a, 0x0082d,
    0x00840, 0x00859,
    0x00900, 0x0093c,
    0x0093d, 0x0094d,
    0x0094e, 0x00951,
    0x00955, 0x00964,
    0x00971, 0x00978,
    0x00979, 0x00980,
    0x00981, 0x00984,
    0x00985, 0x0098d,
    0x0098f, 0x00991,
    0x00993, 0x009a9,
    0x009aa, 0x009b1,
    0x009b2, 0x009b3,
    0x009b6, 0x009ba,
    0x009bd, 0x009c5,
    0x009c7, 0x009c9,
    0x009cb, 0x009cd,
    0x009ce, 0x009cf,
    0x009d7, 0x009d8,
    0x009dc, 0x009de,
    0x009df, 0x009e4,
    0x009f0, 0x009f2,
    0x00a01, 0x00a04,
    0x00a05, 0x00a0b,
    0x00a0f, 0x00a11,
    0x00a13, 0x00a29,
    0x00a2a, 0x00a31,
    0x00a32, 0x00a34,
    0x00a35, 0x00a37,
    0x00a38, 0x00a3a,
    0x00a3e, 0x00a43,
    0x00a47, 0x00a49,
    0x00a4b, 0x00a4d,
    0x00a51, 0x00a52,
    0x00a59, 0x00a5d,
    0x00a5e, 0x00a5f,
    0x00a70, 0x00a76,
    0x00a81, 0x00a84,
    0x00a85, 0x00a8e,
    0x00a8f, 0x00a92,
    0x00a93, 0x00aa9,
    0x00aaa, 0x00ab1,
    0x00ab2, 0x00ab4,
    0x00ab5, 0x00aba,
    0x00abd, 0x00ac6,
    0x00ac7, 0x00aca,
    0x00acb, 0x00acd,
    0x00ad0, 0x00ad1,
    0x00ae0, 0x00ae4,
    0x00b01, 0x00b04,
    0x00b05, 0x00b0d,
    0x00b0f, 0x00b11,
    0x00b13, 0x00b29,
    0x00b2a, 0x00b31,
    0x00b32, 0x00b34,
    0x00b35, 0x00b3a,
    0x00b3d, 0x00b45,
    0x00b47, 0x00b49,
    0x00b4b, 0x00b4d,
    0x00b56, 0x00b58,
    0x00b5c, 0x00b5e,
    0x00b5f, 0x00b64,
    0x00b71, 0x00b72,
    0x00b82, 0x00b84,
    0x00b85, 0x00b8b,
    0x00b8e, 0x00b91,
    0x00b92, 0x00b96,
    0x00b99, 0x00b9b,
    0x00b9c, 0x00b9d,
    0x00b9e, 0x00ba0,
    0x00ba3, 0x00ba5,
    0x00ba8, 0x00bab,
    0x00bae, 0x00bba,
    0x00bbe, 0x00bc3,
    0x00bc6, 0x00bc9,
    0x00bca, 0x00bcd,
    0x00bd0, 0x00bd1,
    0x00bd7, 0x00bd8,
    0x00c01, 0x00c04,
    0x00c05, 0x00c0d,
    0x00c0e, 0x00c11,
    0x00c12, 0x00c29,
    0x00c2a, 0x00c34,
    0x00c35, 0x00c3a,
    0x00c3d, 0x00c45,
    0x00c46, 0x00c49,
    0x00c4a, 0x00c4d,
    0x00c55, 0x00c57,
    0x00c58, 0x00c5a,
    0x00c60, 0x00c64,
    0x00c82, 0x00c84,
    0x00c85, 0x00c8d,
    0x00c8e, 0x00c91,
    0x00c92, 0x00ca9,
    0x00caa, 0x00cb4,
    0x00cb5, 0x00cba,
    0x00cbd, 0x00cc5,
    0x00cc6, 0x00cc9,
    0x00cca, 0x00ccd,
    0x00cd5, 0x00cd7,
    0x00cde, 0x00cdf,
    0x00ce0, 0x00ce4,
    0x00cf1, 0x00cf3,
    0x00d02, 0x00d04,
    0x00d05, 0x00d0d,
    0x00d0e, 0x00d11,
    0x00d12, 0x00d3b,
    0x00d3d, 0x00d45,
    0x00d46, 0x00d49,
    0x00d4a, 0x00d4d,
    0x00d4e, 0x00d4f,
    0x00d57, 0x00d58,
    0x00d60, 0x00d64,
    0x00d7a, 0x00d80,
    0x00d82, 0x00d84,
    0x00d85, 0x00d97,
    0x00d9a, 0x00db2,
    0x00db3, 0x00dbc,
    0x00dbd, 0x00dbe,
    0x00dc0, 0x00dc7,
    0x00dcf, 0x00dd5,
    0x00dd6, 0x00dd7,
    0x00dd8, 0x00de0,
    0x00df2, 0x00df4,
    0x00e01, 0x00e3b,
    0x00e40, 0x00e47,
    0x00e4d, 0x00e4e,
    0x00e81, 0x00e83,
    0x00e84, 0x00e85,
    0x00e87, 0x00e89,
    0x00e8a, 0x00e8b,
    0x00e8d, 0x00e8e,
    0x00e94, 0x00e98,
    0x00e99, 0x00ea0,
    0x00ea1, 0x00ea4,
    0x00ea5, 0x00ea6,
    0x00ea7, 0x00ea8,
    0x00eaa, 0x00eac,
    0x00ead, 0x00eba,
    0x00ebb, 0x00ebe,
    0x00ec0, 0x00ec5,
    0x00ec6, 0x00ec7,
    0x00ecd, 0x00ece,
    0x00edc, 0x00ede,
    0x00f00, 0x00f01,
    0x00f40, 0x00f48,
    0x00f49, 0x00f6d,
    0x00f71, 0x00f82,
    0x00f88, 0x00f98,
    0x00f99, 0x00fbd,
    0x01000, 0x01037,
    0x01038, 0x01039,
    0x0103b, 0x01040,
    0x01050, 0x01063,
    0x01065, 0x01069,
    0x0106e, 0x01087,
    0x0108e, 0x0108f,
    0x0109c, 0x0109e,
    0x010a0, 0x010c6,
    0x010d0, 0x010fb,
    0x010fc, 0x010fd,
    0x01100, 0x01249,
    0x0124a, 0x0124e,
    0x01250, 0x01257,
    0x01258, 0x01259,
    0x0125a, 0x0125e,
    0x01260, 0x01289,
    0x0128a, 0x0128e,
    0x01290, 0x012b1,
    0x012b2, 0x012b6,
    0x012b8, 0x012bf,
    0x012c0, 0x012c1,
    0x012c2, 0x012c6,
    0x012c8, 0x012d7,
    0x012d8, 0x01311,
    0x01312, 0x01316,
    0x01318, 0x0135b,
    0x0135f, 0x01360,
    0x01380, 0x01390,
    0x013a0, 0x013f5,
    0x01401, 0x0166d,
    0x0166f, 0x01680,
    0x01681, 0x0169b,
    0x016a0, 0x016eb,
    0x016ee, 0x016f1,
    0x01700, 0x0170d,
    0x0170e, 0x01714,
    0x01720, 0x01734,
    0x01740, 0x01754,
    0x01760, 0x0176d,
    0x0176e, 0x01771,
    0x01772, 0x01774,
    0x01780, 0x017b4,
    0x017b6, 0x017c9,
    0x017d7, 0x017d8,
    0x017dc, 0x017dd,
    0x01820, 0x01878,
    0x01880, 0x018ab,
    0x018b0, 0x018f6,
    0x01900, 0x0191d,
    0x01920, 0x0192c,
    0x01930, 0x01939,
    0x01950, 0x0196e,
    0x01970, 0x01975,
    0x01980, 0x019ac,
    0x019b0, 0x019ca,
    0x01a00, 0x01a1c,
    0x01a20, 0x01a5f,
    0x01a61, 0x01a75,
    0x01aa7, 0x01aa8,
    0x01b00, 0x01b34,
    0x01b35, 0x01b44,
    0x01b45, 0x01b4c,
    0x01b80, 0x01baa,
    0x01bae, 0x01bb0,
    0x01bc0, 0x01be6,
    0x01be7, 0x01bf2,
    0x01c00, 0x01c36,
    0x01c4d, 0x01c50,
    0x01c5a, 0x01c7e,
    0x01ce9, 0x01ced,
    0x01cee, 0x01cf3,
    0x01d00, 0x01dc0,
    0x01e00, 0x01f16,
    0x01f18, 0x01f1e,
    0x01f20, 0x01f46,
    0x01f48, 0x01f4e,
    0x01f50, 0x01f58,
    0x01f59, 0x01f5a,
    0x01f5b, 0x01f5c,
    0x01f5d, 0x01f5e,
    0x01f5f, 0x01f7e,
    0x01f80, 0x01fb5,
    0x01fb6, 0x01fbd,
    0x01fbe, 0x01fbf,
    0x01fc2, 0x01fc5,
    0x01fc6, 0x01fcd,
    0x01fd0, 0x01fd4,
    0x01fd6, 0x01fdc,
    0x01fe0, 0x01fed,
    0x01ff2, 0x01ff5,
    0x01ff6, 0x01ffd,
    0x02071, 0x02072,
    0x0207f, 0x02080,
    0x02090, 0x0209d,
    0x02102, 0x02103,
    0x02107, 0x02108,
    0x0210a, 0x02114,
    0x02115, 0x02116,
    0x02119, 0x0211e,
    0x02124, 0x02125,
    0x02126, 0x02127,
    0x02128, 0x02129,
    0x0212a, 0x0212e,
    0x0212f, 0x0213a,
    0x0213c, 0x02140,
    0x02145, 0x0214a,
    0x0214e, 0x0214f,
    0x02160, 0x02189,
    0x024b6, 0x024ea,
    0x02c00, 0x02c2f,
    0x02c30, 0x02c5f,
    0x02c60, 0x02ce5,
    0x02ceb, 0x02cef,
    0x02d00, 0x02d26,
    0x02d30, 0x02d66,
    0x02d6f, 0x02d70,
    0x02d80, 0x02d97,
    0x02da0, 0x02da7,
    0x02da8, 0x02daf,
    0x02db0, 0x02db7,
    0x02db8, 0x02dbf,
    0x02dc0, 0x02dc7,
    0x02dc8, 0x02dcf,
    0x02dd0, 0x02dd7,
    0x02dd8, 0x02ddf,
    0x02de0, 0x02e00,
    0x02e2f, 0x02e30,
    0x03005, 0x03008,
    0x03021, 0x0302a,
    0x03031, 0x03036,
    0x03038, 0x0303d,
    0x03041, 0x03097,
    0x0309d, 0x030a0,
    0x030a1, 0x030fb,
    0x030fc, 0x03100,
    0x03105, 0x0312e,
    0x03131, 0x0318f,
    0x031a0, 0x031bb,
    0x031f0, 0x03200,
    0x03400, 0x04db6,
    0x04e00, 0x09fcc,
    0x0a000, 0x0a48d,
    0x0a4d0, 0x0a4fe,
    0x0a500, 0x0a60d,
    0x0a610, 0x0a620,
    0x0a62a, 0x0a62c,
    0x0a640, 0x0a66f,
    0x0a67f, 0x0a698,
    0x0a6a0, 0x0a6f0,
    0x0a717, 0x0a720,
    0x0a722, 0x0a789,
    0x0a78b, 0x0a78f,
    0x0a790, 0x0a792,
    0x0a7a0, 0x0a7aa,
    0x0a7fa, 0x0a802,
    0x0a803, 0x0a806,
    0x0a807, 0x0a80b,
    0x0a80c, 0x0a828,
    0x0a840, 0x0a874,
    0x0a880, 0x0a8c4,
    0x0a8f2, 0x0a8f8,
    0x0a8fb, 0x0a8fc,
    0x0a90a, 0x0a92b,
    0x0a930, 0x0a953,
    0x0a960, 0x0a97d,
    0x0a980, 0x0a9b3,
    0x0a9b4, 0x0a9c0,
    0x0a9cf, 0x0a9d0,
    0x0aa00, 0x0aa37,
    0x0aa40, 0x0aa4e,
    0x0aa60, 0x0aa77,
    0x0aa7a, 0x0aa7b,
    0x0aa80, 0x0aabf,
    0x0aac0, 0x0aac1,
    0x0aac2, 0x0aac3,
    0x0aadb, 0x0aade,
    0x0ab01, 0x0ab07,
    0x0ab09, 0x0ab0f,
    0x0ab11, 0x0ab17,
    0x0ab20, 0x0ab27,
    0x0ab28, 0x0ab2f,
    0x0abc0, 0x0abeb,
    0x0ac00, 0x0d7a4,
    0x0d7b0, 0x0d7c7,
    0x0d7cb, 0x0d7fc,
    0x0f900, 0x0fa2e,
    0x0fa30, 0x0fa6e,
    0x0fa70, 0x0fada,
    0x0fb00, 0x0fb07,
    0x0fb13, 0x0fb18,
    0x0fb1d, 0x0fb29,
    0x0fb2a, 0x0fb37,
    0x0fb38, 0x0fb3d,
    0x0fb3e, 0x0fb3f,
    0x0fb40, 0x0fb42,
    0x0fb43, 0x0fb45,
    0x0fb46, 0x0fbb2,
    0x0fbd3, 0x0fd3e,
    0x0fd50, 0x0fd90,
    0x0fd92, 0x0fdc8,
    0x0fdf0, 0x0fdfc,
    0x0fe70, 0x0fe75,
    0x0fe76, 0x0fefd,
    0x0ff21, 0x0ff3b,
    0x0ff41, 0x0ff5b,
    0x0ff66, 0x0ffbf,
    0x0ffc2, 0x0ffc8,
    0x0ffca, 0x0ffd0,
    0x0ffd2, 0x0ffd8,
    0x0ffda, 0x0ffdd,
    0x10000, 0x1000c,
    0x1000d, 0x10027,
    0x10028, 0x1003b,
    0x1003c, 0x1003e,
    0x1003f, 0x1004e,
    0x10050, 0x1005e,
    0x10080, 0x100fb,
    0x10140, 0x10175,
    0x10280, 0x1029d,
    0x102a0, 0x102d1,
    0x10300, 0x1031f,
    0x10330, 0x1034b,
    0x10380, 0x1039e,
    0x103a0, 0x103c4,
    0x103c8, 0x103d0,
    0x103d1, 0x103d6,
    0x10400, 0x1049e,
    0x10800, 0x10806,
    0x10808, 0x10809,
    0x1080a, 0x10836,
    0x10837, 0x10839,
    0x1083c, 0x1083d,
    0x1083f, 0x10856,
    0x10900, 0x10916,
    0x10920, 0x1093a,
    0x10a00, 0x10a04,
    0x10a05, 0x10a07,
    0x10a0c, 0x10a14,
    0x10a15, 0x10a18,
    0x10a19, 0x10a34,
    0x10a60, 0x10a7d,
    0x10b00, 0x10b36,
    0x10b40, 0x10b56,
    0x10b60, 0x10b73,
    0x10c00, 0x10c49,
    0x11000, 0x11046,
    0x11082, 0x110b9,
    0x12000, 0x1236f,
    0x12400, 0x12463,
    0x13000, 0x1342f,
    0x16800, 0x16a39,
    0x1b000, 0x1b002,
    0x1d400, 0x1d455,
    0x1d456, 0x1d49d,
    0x1d49e, 0x1d4a0,
    0x1d4a2, 0x1d4a3,
    0x1d4a5, 0x1d4a7,
    0x1d4a9, 0x1d4ad,
    0x1d4ae, 0x1d4ba,
    0x1d4bb, 0x1d4bc,
    0x1d4bd, 0x1d4c4,
    0x1d4c5, 0x1d506,
    0x1d507, 0x1d50b,
    0x1d50d, 0x1d515,
    0x1d516, 0x1d51d,
    0x1d51e, 0x1d53a,
    0x1d53b, 0x1d53f,
    0x1d540, 0x1d545,
    0x1d546, 0x1d547,
    0x1d54a, 0x1d551,
    0x1d552, 0x1d6a6,
    0x1d6a8, 0x1d6c1,
    0x1d6c2, 0x1d6db,
    0x1d6dc, 0x1d6fb,
    0x1d6fc, 0x1d715,
    0x1d716, 0x1d735,
    0x1d736, 0x1d74f,
    0x1d750, 0x1d76f,
    0x1d770, 0x1d789,
    0x1d78a, 0x1d7a9,
    0x1d7aa, 0x1d7c3,
    0x1d7c4, 0x1d7cc,
    0x20000, 0x2a6d7,
    0x2a700, 0x2b735,
    0x2b740, 0x2b81e,
    0x2f800, 0x2fa1e,
]);

immutable(RleBitSet!uint) unicodeBalinese = RleBitSet!uint([
    0x01b00, 0x01b4c,
    0x01b50, 0x01b7d,
]);

immutable UnicodeProperty[] unicodeProperties = [
	immutable(UnicodeProperty)("Alphabetic", unicodeAlphabetic),
	immutable(UnicodeProperty)("Arabic", unicodeArabic),
	immutable(UnicodeProperty)("Armenian", unicodeArmenian),
	immutable(UnicodeProperty)("ASCII_Hex_Digit", unicodeASCII_Hex_Digit),
	immutable(UnicodeProperty)("Avestan", unicodeAvestan),
	immutable(UnicodeProperty)("Balinese", unicodeBalinese),
	immutable(UnicodeProperty)("Bamum", unicodeBamum),
	immutable(UnicodeProperty)("Batak", unicodeBatak),
	immutable(UnicodeProperty)("Bengali", unicodeBengali),
	immutable(UnicodeProperty)("Bidi_Control", unicodeBidi_Control),
	immutable(UnicodeProperty)("Bopomofo", unicodeBopomofo),
	immutable(UnicodeProperty)("Brahmi", unicodeBrahmi),
	immutable(UnicodeProperty)("Braille", unicodeBraille),
	immutable(UnicodeProperty)("Buginese", unicodeBuginese),
	immutable(UnicodeProperty)("Buhid", unicodeBuhid),
	immutable(UnicodeProperty)("Canadian_Aboriginal", unicodeCanadian_Aboriginal),
	immutable(UnicodeProperty)("Carian", unicodeCarian),
	immutable(UnicodeProperty)("Cc", unicodeCc),
	immutable(UnicodeProperty)("Cf", unicodeCf),
	immutable(UnicodeProperty)("Cham", unicodeCham),
	immutable(UnicodeProperty)("Cherokee", unicodeCherokee),
	immutable(UnicodeProperty)("Close_Punctuation", unicodePe),
	immutable(UnicodeProperty)("Cn", unicodeCn),
	immutable(UnicodeProperty)("Co", unicodeCo),
	immutable(UnicodeProperty)("Common", unicodeCommon),
	immutable(UnicodeProperty)("Connector_Punctuation", unicodePc),
	immutable(UnicodeProperty)("Control", unicodeCc),
	immutable(UnicodeProperty)("Coptic", unicodeCoptic),
	immutable(UnicodeProperty)("Cs", unicodeCs),
	immutable(UnicodeProperty)("Cuneiform", unicodeCuneiform),
	immutable(UnicodeProperty)("Currency_Symbol", unicodeSc),
	immutable(UnicodeProperty)("Cypriot", unicodeCypriot),
	immutable(UnicodeProperty)("Cyrillic", unicodeCyrillic),
	immutable(UnicodeProperty)("Dash", unicodeDash),
	immutable(UnicodeProperty)("Dash_Punctuation", unicodePd),
	immutable(UnicodeProperty)("Decimal_Number", unicodeNd),
	immutable(UnicodeProperty)("Deprecated", unicodeDeprecated),
	immutable(UnicodeProperty)("Deseret", unicodeDeseret),
	immutable(UnicodeProperty)("Devanagari", unicodeDevanagari),
	immutable(UnicodeProperty)("Diacritic", unicodeDiacritic),
	immutable(UnicodeProperty)("Egyptian_Hieroglyphs", unicodeEgyptian_Hieroglyphs),
	immutable(UnicodeProperty)("Enclosing_Mark", unicodeMe),
	immutable(UnicodeProperty)("Ethiopic", unicodeEthiopic),
	immutable(UnicodeProperty)("Extender", unicodeExtender),
	immutable(UnicodeProperty)("Final_Punctuation", unicodePf),
	immutable(UnicodeProperty)("Format", unicodeCf),
	immutable(UnicodeProperty)("Georgian", unicodeGeorgian),
	immutable(UnicodeProperty)("Glagolitic", unicodeGlagolitic),
	immutable(UnicodeProperty)("Gothic", unicodeGothic),
	immutable(UnicodeProperty)("Greek", unicodeGreek),
	immutable(UnicodeProperty)("Gujarati", unicodeGujarati),
	immutable(UnicodeProperty)("Gurmukhi", unicodeGurmukhi),
	immutable(UnicodeProperty)("Han", unicodeHan),
	immutable(UnicodeProperty)("Hangul", unicodeHangul),
	immutable(UnicodeProperty)("Hanunoo", unicodeHanunoo),
	immutable(UnicodeProperty)("Hebrew", unicodeHebrew),
	immutable(UnicodeProperty)("Hex_Digit", unicodeHex_Digit),
	immutable(UnicodeProperty)("Hiragana", unicodeHiragana),
	immutable(UnicodeProperty)("Hyphen", unicodeHyphen),
	immutable(UnicodeProperty)("Ideographic", unicodeIdeographic),
	immutable(UnicodeProperty)("IDS_Binary_Operator", unicodeIDS_Binary_Operator),
	immutable(UnicodeProperty)("IDS_Trinary_Operator", unicodeIDS_Trinary_Operator),
	immutable(UnicodeProperty)("Imperial_Aramaic", unicodeImperial_Aramaic),
	immutable(UnicodeProperty)("InAegean Numbers", unicodeInAegean_Numbers),
	immutable(UnicodeProperty)("InAlchemical Symbols", unicodeInAlchemical_Symbols),
	immutable(UnicodeProperty)("InAlphabetic Presentation Forms", unicodeInAlphabetic_Presentation_Forms),
	immutable(UnicodeProperty)("InAncient Greek Musical Notation", unicodeInAncient_Greek_Musical_Notation),
	immutable(UnicodeProperty)("InAncient Greek Numbers", unicodeInAncient_Greek_Numbers),
	immutable(UnicodeProperty)("InAncient Symbols", unicodeInAncient_Symbols),
	immutable(UnicodeProperty)("InArabic", unicodeInArabic),
	immutable(UnicodeProperty)("InArabic Presentation Forms-A", unicodeInArabic_Presentation_Forms_A),
	immutable(UnicodeProperty)("InArabic Presentation Forms-B", unicodeInArabic_Presentation_Forms_B),
	immutable(UnicodeProperty)("InArabic Supplement", unicodeInArabic_Supplement),
	immutable(UnicodeProperty)("InArmenian", unicodeInArmenian),
	immutable(UnicodeProperty)("InArrows", unicodeInArrows),
	immutable(UnicodeProperty)("InAvestan", unicodeInAvestan),
	immutable(UnicodeProperty)("InBalinese", unicodeInBalinese),
	immutable(UnicodeProperty)("InBamum", unicodeInBamum),
	immutable(UnicodeProperty)("InBamum Supplement", unicodeInBamum_Supplement),
	immutable(UnicodeProperty)("InBasic Latin", unicodeInBasic_Latin),
	immutable(UnicodeProperty)("InBatak", unicodeInBatak),
	immutable(UnicodeProperty)("InBengali", unicodeInBengali),
	immutable(UnicodeProperty)("InBlock Elements", unicodeInBlock_Elements),
	immutable(UnicodeProperty)("InBopomofo", unicodeInBopomofo),
	immutable(UnicodeProperty)("InBopomofo Extended", unicodeInBopomofo_Extended),
	immutable(UnicodeProperty)("InBox Drawing", unicodeInBox_Drawing),
	immutable(UnicodeProperty)("InBrahmi", unicodeInBrahmi),
	immutable(UnicodeProperty)("InBraille Patterns", unicodeInBraille_Patterns),
	immutable(UnicodeProperty)("InBuginese", unicodeInBuginese),
	immutable(UnicodeProperty)("InBuhid", unicodeInBuhid),
	immutable(UnicodeProperty)("InByzantine Musical Symbols", unicodeInByzantine_Musical_Symbols),
	immutable(UnicodeProperty)("InCarian", unicodeInCarian),
	immutable(UnicodeProperty)("InCham", unicodeInCham),
	immutable(UnicodeProperty)("InCherokee", unicodeInCherokee),
	immutable(UnicodeProperty)("InCJK Compatibility", unicodeInCJK_Compatibility),
	immutable(UnicodeProperty)("InCJK Compatibility Forms", unicodeInCJK_Compatibility_Forms),
	immutable(UnicodeProperty)("InCJK Compatibility Ideographs", unicodeInCJK_Compatibility_Ideographs),
	immutable(UnicodeProperty)("InCJK Compatibility Ideographs Supplement", unicodeInCJK_Compatibility_Ideographs_Supplement),
	immutable(UnicodeProperty)("InCJK Radicals Supplement", unicodeInCJK_Radicals_Supplement),
	immutable(UnicodeProperty)("InCJK Strokes", unicodeInCJK_Strokes),
	immutable(UnicodeProperty)("InCJK Symbols and Punctuation", unicodeInCJK_Symbols_and_Punctuation),
	immutable(UnicodeProperty)("InCJK Unified Ideographs", unicodeInCJK_Unified_Ideographs),
	immutable(UnicodeProperty)("InCJK Unified Ideographs Extension A", unicodeInCJK_Unified_Ideographs_Extension_A),
	immutable(UnicodeProperty)("InCJK Unified Ideographs Extension B", unicodeInCJK_Unified_Ideographs_Extension_B),
	immutable(UnicodeProperty)("InCJK Unified Ideographs Extension C", unicodeInCJK_Unified_Ideographs_Extension_C),
	immutable(UnicodeProperty)("InCJK Unified Ideographs Extension D", unicodeInCJK_Unified_Ideographs_Extension_D),
	immutable(UnicodeProperty)("InCombining Diacritical Marks", unicodeInCombining_Diacritical_Marks),
	immutable(UnicodeProperty)("InCombining Diacritical Marks for Symbols", unicodeInCombining_Diacritical_Marks_for_Symbols),
	immutable(UnicodeProperty)("InCombining Diacritical Marks Supplement", unicodeInCombining_Diacritical_Marks_Supplement),
	immutable(UnicodeProperty)("InCombining Half Marks", unicodeInCombining_Half_Marks),
	immutable(UnicodeProperty)("InCommon Indic Number Forms", unicodeInCommon_Indic_Number_Forms),
	immutable(UnicodeProperty)("InControl Pictures", unicodeInControl_Pictures),
	immutable(UnicodeProperty)("InCoptic", unicodeInCoptic),
	immutable(UnicodeProperty)("InCounting Rod Numerals", unicodeInCounting_Rod_Numerals),
	immutable(UnicodeProperty)("InCuneiform", unicodeInCuneiform),
	immutable(UnicodeProperty)("InCuneiform Numbers and Punctuation", unicodeInCuneiform_Numbers_and_Punctuation),
	immutable(UnicodeProperty)("InCurrency Symbols", unicodeInCurrency_Symbols),
	immutable(UnicodeProperty)("InCypriot Syllabary", unicodeInCypriot_Syllabary),
	immutable(UnicodeProperty)("InCyrillic", unicodeInCyrillic),
	immutable(UnicodeProperty)("InCyrillic Extended-A", unicodeInCyrillic_Extended_A),
	immutable(UnicodeProperty)("InCyrillic Extended-B", unicodeInCyrillic_Extended_B),
	immutable(UnicodeProperty)("InCyrillic Supplement", unicodeInCyrillic_Supplement),
	immutable(UnicodeProperty)("InDeseret", unicodeInDeseret),
	immutable(UnicodeProperty)("InDevanagari", unicodeInDevanagari),
	immutable(UnicodeProperty)("InDevanagari Extended", unicodeInDevanagari_Extended),
	immutable(UnicodeProperty)("InDingbats", unicodeInDingbats),
	immutable(UnicodeProperty)("InDomino Tiles", unicodeInDomino_Tiles),
	immutable(UnicodeProperty)("InEgyptian Hieroglyphs", unicodeInEgyptian_Hieroglyphs),
	immutable(UnicodeProperty)("InEmoticons", unicodeInEmoticons),
	immutable(UnicodeProperty)("InEnclosed Alphanumerics", unicodeInEnclosed_Alphanumerics),
	immutable(UnicodeProperty)("InEnclosed Alphanumeric Supplement", unicodeInEnclosed_Alphanumeric_Supplement),
	immutable(UnicodeProperty)("InEnclosed CJK Letters and Months", unicodeInEnclosed_CJK_Letters_and_Months),
	immutable(UnicodeProperty)("InEnclosed Ideographic Supplement", unicodeInEnclosed_Ideographic_Supplement),
	immutable(UnicodeProperty)("InEthiopic", unicodeInEthiopic),
	immutable(UnicodeProperty)("InEthiopic Extended", unicodeInEthiopic_Extended),
	immutable(UnicodeProperty)("InEthiopic Extended-A", unicodeInEthiopic_Extended_A),
	immutable(UnicodeProperty)("InEthiopic Supplement", unicodeInEthiopic_Supplement),
	immutable(UnicodeProperty)("InGeneral Punctuation", unicodeInGeneral_Punctuation),
	immutable(UnicodeProperty)("InGeometric Shapes", unicodeInGeometric_Shapes),
	immutable(UnicodeProperty)("InGeorgian", unicodeInGeorgian),
	immutable(UnicodeProperty)("InGeorgian Supplement", unicodeInGeorgian_Supplement),
	immutable(UnicodeProperty)("InGlagolitic", unicodeInGlagolitic),
	immutable(UnicodeProperty)("InGothic", unicodeInGothic),
	immutable(UnicodeProperty)("InGreek and Coptic", unicodeInGreek_and_Coptic),
	immutable(UnicodeProperty)("InGreek Extended", unicodeInGreek_Extended),
	immutable(UnicodeProperty)("InGujarati", unicodeInGujarati),
	immutable(UnicodeProperty)("InGurmukhi", unicodeInGurmukhi),
	immutable(UnicodeProperty)("InHalfwidth and Fullwidth Forms", unicodeInHalfwidth_and_Fullwidth_Forms),
	immutable(UnicodeProperty)("InHangul Compatibility Jamo", unicodeInHangul_Compatibility_Jamo),
	immutable(UnicodeProperty)("InHangul Jamo", unicodeInHangul_Jamo),
	immutable(UnicodeProperty)("InHangul Jamo Extended-A", unicodeInHangul_Jamo_Extended_A),
	immutable(UnicodeProperty)("InHangul Jamo Extended-B", unicodeInHangul_Jamo_Extended_B),
	immutable(UnicodeProperty)("InHangul Syllables", unicodeInHangul_Syllables),
	immutable(UnicodeProperty)("InHanunoo", unicodeInHanunoo),
	immutable(UnicodeProperty)("InHebrew", unicodeInHebrew),
	immutable(UnicodeProperty)("Inherited", unicodeInherited),
	immutable(UnicodeProperty)("InHigh Private Use Surrogates", unicodeInHigh_Private_Use_Surrogates),
	immutable(UnicodeProperty)("InHigh Surrogates", unicodeInHigh_Surrogates),
	immutable(UnicodeProperty)("InHiragana", unicodeInHiragana),
	immutable(UnicodeProperty)("InIdeographic Description Characters", unicodeInIdeographic_Description_Characters),
	immutable(UnicodeProperty)("InImperial Aramaic", unicodeInImperial_Aramaic),
	immutable(UnicodeProperty)("InInscriptional Pahlavi", unicodeInInscriptional_Pahlavi),
	immutable(UnicodeProperty)("InInscriptional Parthian", unicodeInInscriptional_Parthian),
	immutable(UnicodeProperty)("InIPA Extensions", unicodeInIPA_Extensions),
	immutable(UnicodeProperty)("Initial_Punctuation", unicodePi),
	immutable(UnicodeProperty)("InJavanese", unicodeInJavanese),
	immutable(UnicodeProperty)("InKaithi", unicodeInKaithi),
	immutable(UnicodeProperty)("InKana Supplement", unicodeInKana_Supplement),
	immutable(UnicodeProperty)("InKanbun", unicodeInKanbun),
	immutable(UnicodeProperty)("InKangxi Radicals", unicodeInKangxi_Radicals),
	immutable(UnicodeProperty)("InKannada", unicodeInKannada),
	immutable(UnicodeProperty)("InKatakana", unicodeInKatakana),
	immutable(UnicodeProperty)("InKatakana Phonetic Extensions", unicodeInKatakana_Phonetic_Extensions),
	immutable(UnicodeProperty)("InKayah Li", unicodeInKayah_Li),
	immutable(UnicodeProperty)("InKharoshthi", unicodeInKharoshthi),
	immutable(UnicodeProperty)("InKhmer", unicodeInKhmer),
	immutable(UnicodeProperty)("InKhmer Symbols", unicodeInKhmer_Symbols),
	immutable(UnicodeProperty)("InLao", unicodeInLao),
	immutable(UnicodeProperty)("InLatin-1 Supplement", unicodeInLatin_1_Supplement),
	immutable(UnicodeProperty)("InLatin Extended-A", unicodeInLatin_Extended_A),
	immutable(UnicodeProperty)("InLatin Extended Additional", unicodeInLatin_Extended_Additional),
	immutable(UnicodeProperty)("InLatin Extended-B", unicodeInLatin_Extended_B),
	immutable(UnicodeProperty)("InLatin Extended-C", unicodeInLatin_Extended_C),
	immutable(UnicodeProperty)("InLatin Extended-D", unicodeInLatin_Extended_D),
	immutable(UnicodeProperty)("InLepcha", unicodeInLepcha),
	immutable(UnicodeProperty)("InLetterlike Symbols", unicodeInLetterlike_Symbols),
	immutable(UnicodeProperty)("InLimbu", unicodeInLimbu),
	immutable(UnicodeProperty)("InLinear B Ideograms", unicodeInLinear_B_Ideograms),
	immutable(UnicodeProperty)("InLinear B Syllabary", unicodeInLinear_B_Syllabary),
	immutable(UnicodeProperty)("InLisu", unicodeInLisu),
	immutable(UnicodeProperty)("InLow Surrogates", unicodeInLow_Surrogates),
	immutable(UnicodeProperty)("InLycian", unicodeInLycian),
	immutable(UnicodeProperty)("InLydian", unicodeInLydian),
	immutable(UnicodeProperty)("InMahjong Tiles", unicodeInMahjong_Tiles),
	immutable(UnicodeProperty)("InMalayalam", unicodeInMalayalam),
	immutable(UnicodeProperty)("InMandaic", unicodeInMandaic),
	immutable(UnicodeProperty)("InMathematical Alphanumeric Symbols", unicodeInMathematical_Alphanumeric_Symbols),
	immutable(UnicodeProperty)("InMathematical Operators", unicodeInMathematical_Operators),
	immutable(UnicodeProperty)("InMeetei Mayek", unicodeInMeetei_Mayek),
	immutable(UnicodeProperty)("InMiscellaneous Mathematical Symbols-A", unicodeInMiscellaneous_Mathematical_Symbols_A),
	immutable(UnicodeProperty)("InMiscellaneous Mathematical Symbols-B", unicodeInMiscellaneous_Mathematical_Symbols_B),
	immutable(UnicodeProperty)("InMiscellaneous Symbols", unicodeInMiscellaneous_Symbols),
	immutable(UnicodeProperty)("InMiscellaneous Symbols and Arrows", unicodeInMiscellaneous_Symbols_and_Arrows),
	immutable(UnicodeProperty)("InMiscellaneous Symbols And Pictographs", unicodeInMiscellaneous_Symbols_And_Pictographs),
	immutable(UnicodeProperty)("InMiscellaneous Technical", unicodeInMiscellaneous_Technical),
	immutable(UnicodeProperty)("InModifier Tone Letters", unicodeInModifier_Tone_Letters),
	immutable(UnicodeProperty)("InMongolian", unicodeInMongolian),
	immutable(UnicodeProperty)("InMusical Symbols", unicodeInMusical_Symbols),
	immutable(UnicodeProperty)("InMyanmar", unicodeInMyanmar),
	immutable(UnicodeProperty)("InMyanmar Extended-A", unicodeInMyanmar_Extended_A),
	immutable(UnicodeProperty)("InNew Tai Lue", unicodeInNew_Tai_Lue),
	immutable(UnicodeProperty)("InNKo", unicodeInNKo),
	immutable(UnicodeProperty)("InNumber Forms", unicodeInNumber_Forms),
	immutable(UnicodeProperty)("InOgham", unicodeInOgham),
	immutable(UnicodeProperty)("InOl Chiki", unicodeInOl_Chiki),
	immutable(UnicodeProperty)("InOld Italic", unicodeInOld_Italic),
	immutable(UnicodeProperty)("InOld Persian", unicodeInOld_Persian),
	immutable(UnicodeProperty)("InOld South Arabian", unicodeInOld_South_Arabian),
	immutable(UnicodeProperty)("InOld Turkic", unicodeInOld_Turkic),
	immutable(UnicodeProperty)("InOptical Character Recognition", unicodeInOptical_Character_Recognition),
	immutable(UnicodeProperty)("InOriya", unicodeInOriya),
	immutable(UnicodeProperty)("InOsmanya", unicodeInOsmanya),
	immutable(UnicodeProperty)("InPhags-pa", unicodeInPhags_pa),
	immutable(UnicodeProperty)("InPhaistos Disc", unicodeInPhaistos_Disc),
	immutable(UnicodeProperty)("InPhoenician", unicodeInPhoenician),
	immutable(UnicodeProperty)("InPhonetic Extensions", unicodeInPhonetic_Extensions),
	immutable(UnicodeProperty)("InPhonetic Extensions Supplement", unicodeInPhonetic_Extensions_Supplement),
	immutable(UnicodeProperty)("InPlaying Cards", unicodeInPlaying_Cards),
	immutable(UnicodeProperty)("InPrivate Use Area", unicodeInPrivate_Use_Area),
	immutable(UnicodeProperty)("InRejang", unicodeInRejang),
	immutable(UnicodeProperty)("InRumi Numeral Symbols", unicodeInRumi_Numeral_Symbols),
	immutable(UnicodeProperty)("InRunic", unicodeInRunic),
	immutable(UnicodeProperty)("InSamaritan", unicodeInSamaritan),
	immutable(UnicodeProperty)("InSaurashtra", unicodeInSaurashtra),
	immutable(UnicodeProperty)("Inscriptional_Pahlavi", unicodeInscriptional_Pahlavi),
	immutable(UnicodeProperty)("Inscriptional_Parthian", unicodeInscriptional_Parthian),
	immutable(UnicodeProperty)("InShavian", unicodeInShavian),
	immutable(UnicodeProperty)("InSinhala", unicodeInSinhala),
	immutable(UnicodeProperty)("InSmall Form Variants", unicodeInSmall_Form_Variants),
	immutable(UnicodeProperty)("InSpacing Modifier Letters", unicodeInSpacing_Modifier_Letters),
	immutable(UnicodeProperty)("InSpecials", unicodeInSpecials),
	immutable(UnicodeProperty)("InSundanese", unicodeInSundanese),
	immutable(UnicodeProperty)("InSuperscripts and Subscripts", unicodeInSuperscripts_and_Subscripts),
	immutable(UnicodeProperty)("InSupplemental Arrows-A", unicodeInSupplemental_Arrows_A),
	immutable(UnicodeProperty)("InSupplemental Arrows-B", unicodeInSupplemental_Arrows_B),
	immutable(UnicodeProperty)("InSupplemental Mathematical Operators", unicodeInSupplemental_Mathematical_Operators),
	immutable(UnicodeProperty)("InSupplemental Punctuation", unicodeInSupplemental_Punctuation),
	immutable(UnicodeProperty)("InSupplementary Private Use Area-A", unicodeInSupplementary_Private_Use_Area_A),
	immutable(UnicodeProperty)("InSupplementary Private Use Area-B", unicodeInSupplementary_Private_Use_Area_B),
	immutable(UnicodeProperty)("InSyloti Nagri", unicodeInSyloti_Nagri),
	immutable(UnicodeProperty)("InSyriac", unicodeInSyriac),
	immutable(UnicodeProperty)("InTagalog", unicodeInTagalog),
	immutable(UnicodeProperty)("InTagbanwa", unicodeInTagbanwa),
	immutable(UnicodeProperty)("InTags", unicodeInTags),
	immutable(UnicodeProperty)("InTai Le", unicodeInTai_Le),
	immutable(UnicodeProperty)("InTai Tham", unicodeInTai_Tham),
	immutable(UnicodeProperty)("InTai Viet", unicodeInTai_Viet),
	immutable(UnicodeProperty)("InTai Xuan Jing Symbols", unicodeInTai_Xuan_Jing_Symbols),
	immutable(UnicodeProperty)("InTamil", unicodeInTamil),
	immutable(UnicodeProperty)("InTelugu", unicodeInTelugu),
	immutable(UnicodeProperty)("InThaana", unicodeInThaana),
	immutable(UnicodeProperty)("InThai", unicodeInThai),
	immutable(UnicodeProperty)("InTibetan", unicodeInTibetan),
	immutable(UnicodeProperty)("InTifinagh", unicodeInTifinagh),
	immutable(UnicodeProperty)("InTransport And Map Symbols", unicodeInTransport_And_Map_Symbols),
	immutable(UnicodeProperty)("InUgaritic", unicodeInUgaritic),
	immutable(UnicodeProperty)("InUnified Canadian Aboriginal Syllabics", unicodeInUnified_Canadian_Aboriginal_Syllabics),
	immutable(UnicodeProperty)("InUnified Canadian Aboriginal Syllabics Extended", unicodeInUnified_Canadian_Aboriginal_Syllabics_Extended),
	immutable(UnicodeProperty)("InVai", unicodeInVai),
	immutable(UnicodeProperty)("InVariation Selectors", unicodeInVariation_Selectors),
	immutable(UnicodeProperty)("InVariation Selectors Supplement", unicodeInVariation_Selectors_Supplement),
	immutable(UnicodeProperty)("InVedic Extensions", unicodeInVedic_Extensions),
	immutable(UnicodeProperty)("InVertical Forms", unicodeInVertical_Forms),
	immutable(UnicodeProperty)("InYijing Hexagram Symbols", unicodeInYijing_Hexagram_Symbols),
	immutable(UnicodeProperty)("InYi Radicals", unicodeInYi_Radicals),
	immutable(UnicodeProperty)("InYi Syllables", unicodeInYi_Syllables),
	immutable(UnicodeProperty)("Javanese", unicodeJavanese),
	immutable(UnicodeProperty)("Join_Control", unicodeJoin_Control),
	immutable(UnicodeProperty)("Kaithi", unicodeKaithi),
	immutable(UnicodeProperty)("Kannada", unicodeKannada),
	immutable(UnicodeProperty)("Katakana", unicodeKatakana),
	immutable(UnicodeProperty)("Kayah_Li", unicodeKayah_Li),
	immutable(UnicodeProperty)("Kharoshthi", unicodeKharoshthi),
	immutable(UnicodeProperty)("Khmer", unicodeKhmer),
	immutable(UnicodeProperty)("Lao", unicodeLao),
	immutable(UnicodeProperty)("Latin", unicodeLatin),
	immutable(UnicodeProperty)("Lepcha", unicodeLepcha),
	immutable(UnicodeProperty)("Letter_Number", unicodeNl),
	immutable(UnicodeProperty)("Limbu", unicodeLimbu),
	immutable(UnicodeProperty)("Linear_B", unicodeLinear_B),
	immutable(UnicodeProperty)("Line_Separator", unicodeZl),
	immutable(UnicodeProperty)("Lisu", unicodeLisu),
	immutable(UnicodeProperty)("Ll", unicodeLl),
	immutable(UnicodeProperty)("Lm", unicodeLm),
	immutable(UnicodeProperty)("Lo", unicodeLo),
	immutable(UnicodeProperty)("Logical_Order_Exception", unicodeLogical_Order_Exception),
	immutable(UnicodeProperty)("Lowercase_Letter", unicodeLl),
	immutable(UnicodeProperty)("Lt", unicodeLt),
	immutable(UnicodeProperty)("Lu", unicodeLu),
	immutable(UnicodeProperty)("Lycian", unicodeLycian),
	immutable(UnicodeProperty)("Lydian", unicodeLydian),
	immutable(UnicodeProperty)("Malayalam", unicodeMalayalam),
	immutable(UnicodeProperty)("Mandaic", unicodeMandaic),
	immutable(UnicodeProperty)("Math", unicodeMath),
	immutable(UnicodeProperty)("Math_Symbol", unicodeSm),
	immutable(UnicodeProperty)("Mc", unicodeMc),
	immutable(UnicodeProperty)("Me", unicodeMe),
	immutable(UnicodeProperty)("Meetei_Mayek", unicodeMeetei_Mayek),
	immutable(UnicodeProperty)("Mn", unicodeMn),
	immutable(UnicodeProperty)("Modifier_Letter", unicodeLm),
	immutable(UnicodeProperty)("Modifier_Symbol", unicodeSk),
	immutable(UnicodeProperty)("Mongolian", unicodeMongolian),
	immutable(UnicodeProperty)("Myanmar", unicodeMyanmar),
	immutable(UnicodeProperty)("Nd", unicodeNd),
	immutable(UnicodeProperty)("New_Tai_Lue", unicodeNew_Tai_Lue),
	immutable(UnicodeProperty)("Nko", unicodeNko),
	immutable(UnicodeProperty)("Nl", unicodeNl),
	immutable(UnicodeProperty)("No", unicodeNo),
	immutable(UnicodeProperty)("Noncharacter_Code_Point", unicodeNoncharacter_Code_Point),
	immutable(UnicodeProperty)("Nonspacing_Mark", unicodeMn),
	immutable(UnicodeProperty)("Ogham", unicodeOgham),
	immutable(UnicodeProperty)("Ol_Chiki", unicodeOl_Chiki),
	immutable(UnicodeProperty)("Old_Italic", unicodeOld_Italic),
	immutable(UnicodeProperty)("Old_Persian", unicodeOld_Persian),
	immutable(UnicodeProperty)("Old_South_Arabian", unicodeOld_South_Arabian),
	immutable(UnicodeProperty)("Old_Turkic", unicodeOld_Turkic),
	immutable(UnicodeProperty)("Open_Punctuation", unicodePs),
	immutable(UnicodeProperty)("Oriya", unicodeOriya),
	immutable(UnicodeProperty)("Osmanya", unicodeOsmanya),
	immutable(UnicodeProperty)("Other_Alphabetic", unicodeOther_Alphabetic),
	immutable(UnicodeProperty)("Other_Default_Ignorable_Code_Point", unicodeOther_Default_Ignorable_Code_Point),
	immutable(UnicodeProperty)("Other_Grapheme_Extend", unicodeOther_Grapheme_Extend),
	immutable(UnicodeProperty)("Other_ID_Continue", unicodeOther_ID_Continue),
	immutable(UnicodeProperty)("Other_ID_Start", unicodeOther_ID_Start),
	immutable(UnicodeProperty)("Other_Letter", unicodeLo),
	immutable(UnicodeProperty)("Other_Lowercase", unicodeOther_Lowercase),
	immutable(UnicodeProperty)("Other_Math", unicodeOther_Math),
	immutable(UnicodeProperty)("Other_Number", unicodeNo),
	immutable(UnicodeProperty)("Other_Punctuation", unicodePo),
	immutable(UnicodeProperty)("Other_Symbol", unicodeSo),
	immutable(UnicodeProperty)("Other_Uppercase", unicodeOther_Uppercase),
	immutable(UnicodeProperty)("Paragraph_Separator", unicodeZp),
	immutable(UnicodeProperty)("Pattern_Syntax", unicodePattern_Syntax),
	immutable(UnicodeProperty)("Pattern_White_Space", unicodePattern_White_Space),
	immutable(UnicodeProperty)("Pc", unicodePc),
	immutable(UnicodeProperty)("Pd", unicodePd),
	immutable(UnicodeProperty)("Pe", unicodePe),
	immutable(UnicodeProperty)("Pf", unicodePf),
	immutable(UnicodeProperty)("Phags_Pa", unicodePhags_Pa),
	immutable(UnicodeProperty)("Phoenician", unicodePhoenician),
	immutable(UnicodeProperty)("Pi", unicodePi),
	immutable(UnicodeProperty)("Po", unicodePo),
	immutable(UnicodeProperty)("Private_Use", unicodeCo),
	immutable(UnicodeProperty)("Ps", unicodePs),
	immutable(UnicodeProperty)("Quotation_Mark", unicodeQuotation_Mark),
	immutable(UnicodeProperty)("Radical", unicodeRadical),
	immutable(UnicodeProperty)("Rejang", unicodeRejang),
	immutable(UnicodeProperty)("Runic", unicodeRunic),
	immutable(UnicodeProperty)("Samaritan", unicodeSamaritan),
	immutable(UnicodeProperty)("Saurashtra", unicodeSaurashtra),
	immutable(UnicodeProperty)("Sc", unicodeSc),
	immutable(UnicodeProperty)("Shavian", unicodeShavian),
	immutable(UnicodeProperty)("Sinhala", unicodeSinhala),
	immutable(UnicodeProperty)("Sk", unicodeSk),
	immutable(UnicodeProperty)("Sm", unicodeSm),
	immutable(UnicodeProperty)("So", unicodeSo),
	immutable(UnicodeProperty)("Soft_Dotted", unicodeSoft_Dotted),
	immutable(UnicodeProperty)("Space_Separator", unicodeZs),
	immutable(UnicodeProperty)("Spacing_Mark", unicodeMc),
	immutable(UnicodeProperty)("STerm", unicodeSTerm),
	immutable(UnicodeProperty)("Sundanese", unicodeSundanese),
	immutable(UnicodeProperty)("Surrogate", unicodeCs),
	immutable(UnicodeProperty)("Syloti_Nagri", unicodeSyloti_Nagri),
	immutable(UnicodeProperty)("Syriac", unicodeSyriac),
	immutable(UnicodeProperty)("Tagalog", unicodeTagalog),
	immutable(UnicodeProperty)("Tagbanwa", unicodeTagbanwa),
	immutable(UnicodeProperty)("Tai_Le", unicodeTai_Le),
	immutable(UnicodeProperty)("Tai_Tham", unicodeTai_Tham),
	immutable(UnicodeProperty)("Tai_Viet", unicodeTai_Viet),
	immutable(UnicodeProperty)("Tamil", unicodeTamil),
	immutable(UnicodeProperty)("Telugu", unicodeTelugu),
	immutable(UnicodeProperty)("Terminal_Punctuation", unicodeTerminal_Punctuation),
	immutable(UnicodeProperty)("Thaana", unicodeThaana),
	immutable(UnicodeProperty)("Thai", unicodeThai),
	immutable(UnicodeProperty)("Tibetan", unicodeTibetan),
	immutable(UnicodeProperty)("Tifinagh", unicodeTifinagh),
	immutable(UnicodeProperty)("Titlecase_Letter", unicodeLt),
	immutable(UnicodeProperty)("Ugaritic", unicodeUgaritic),
	immutable(UnicodeProperty)("Unassigned", unicodeCn),
	immutable(UnicodeProperty)("Unified_Ideograph", unicodeUnified_Ideograph),
	immutable(UnicodeProperty)("Uppercase_Letter", unicodeLu),
	immutable(UnicodeProperty)("Vai", unicodeVai),
	immutable(UnicodeProperty)("Variation_Selector", unicodeVariation_Selector),
	immutable(UnicodeProperty)("White_Space", unicodeWhite_Space),
	immutable(UnicodeProperty)("Yi", unicodeYi),
	immutable(UnicodeProperty)("Zl", unicodeZl),
	immutable(UnicodeProperty)("Zp", unicodeZp),
	immutable(UnicodeProperty)("Zs", unicodeZs),
];

immutable NFC_QCN = RleBitSet!uint([
    0x00340, 0x00342,
    0x00343, 0x00345,
    0x00374, 0x00375,
    0x0037e, 0x0037f,
    0x00387, 0x00388,
    0x00958, 0x00960,
    0x009dc, 0x009de,
    0x009df, 0x009e0,
    0x00a33, 0x00a34,
    0x00a36, 0x00a37,
    0x00a59, 0x00a5c,
    0x00a5e, 0x00a5f,
    0x00b5c, 0x00b5e,
    0x00f43, 0x00f44,
    0x00f4d, 0x00f4e,
    0x00f52, 0x00f53,
    0x00f57, 0x00f58,
    0x00f5c, 0x00f5d,
    0x00f69, 0x00f6a,
    0x00f73, 0x00f74,
    0x00f75, 0x00f77,
    0x00f78, 0x00f79,
    0x00f81, 0x00f82,
    0x00f93, 0x00f94,
    0x00f9d, 0x00f9e,
    0x00fa2, 0x00fa3,
    0x00fa7, 0x00fa8,
    0x00fac, 0x00fad,
    0x00fb9, 0x00fba,
    0x01f71, 0x01f72,
    0x01f73, 0x01f74,
    0x01f75, 0x01f76,
    0x01f77, 0x01f78,
    0x01f79, 0x01f7a,
    0x01f7b, 0x01f7c,
    0x01f7d, 0x01f7e,
    0x01fbb, 0x01fbc,
    0x01fbe, 0x01fbf,
    0x01fc9, 0x01fca,
    0x01fcb, 0x01fcc,
    0x01fd3, 0x01fd4,
    0x01fdb, 0x01fdc,
    0x01fe3, 0x01fe4,
    0x01feb, 0x01fec,
    0x01fee, 0x01ff0,
    0x01ff9, 0x01ffa,
    0x01ffb, 0x01ffc,
    0x01ffd, 0x01ffe,
    0x02000, 0x02002,
    0x02126, 0x02127,
    0x0212a, 0x0212c,
    0x02329, 0x0232b,
    0x02adc, 0x02add,
    0x0f900, 0x0fa0e,
    0x0fa10, 0x0fa11,
    0x0fa12, 0x0fa13,
    0x0fa15, 0x0fa1f,
    0x0fa20, 0x0fa21,
    0x0fa22, 0x0fa23,
    0x0fa25, 0x0fa27,
    0x0fa2a, 0x0fa2e,
    0x0fa30, 0x0fa6e,
    0x0fa70, 0x0fada,
    0x0fb1d, 0x0fb1e,
    0x0fb1f, 0x0fb20,
    0x0fb2a, 0x0fb37,
    0x0fb38, 0x0fb3d,
    0x0fb3e, 0x0fb3f,
    0x0fb40, 0x0fb42,
    0x0fb43, 0x0fb45,
    0x0fb46, 0x0fb4f,
    0x1d15e, 0x1d165,
    0x1d1bb, 0x1d1c1,
    0x2f800, 0x2fa1e,
]);

immutable NFC_QCM = RleBitSet!uint([
    0x00300, 0x00305,
    0x00306, 0x0030d,
    0x0030f, 0x00310,
    0x00311, 0x00312,
    0x00313, 0x00315,
    0x0031b, 0x0031c,
    0x00323, 0x00329,
    0x0032d, 0x0032f,
    0x00330, 0x00332,
    0x00338, 0x00339,
    0x00342, 0x00343,
    0x00345, 0x00346,
    0x00653, 0x00656,
    0x0093c, 0x0093d,
    0x009be, 0x009bf,
    0x009d7, 0x009d8,
    0x00b3e, 0x00b3f,
    0x00b56, 0x00b58,
    0x00bbe, 0x00bbf,
    0x00bd7, 0x00bd8,
    0x00c56, 0x00c57,
    0x00cc2, 0x00cc3,
    0x00cd5, 0x00cd7,
    0x00d3e, 0x00d3f,
    0x00d57, 0x00d58,
    0x00dca, 0x00dcb,
    0x00dcf, 0x00dd0,
    0x00ddf, 0x00de0,
    0x0102e, 0x0102f,
    0x01161, 0x01176,
    0x011a8, 0x011c3,
    0x01b35, 0x01b36,
    0x03099, 0x0309b,
    0x110ba, 0x110bb,
]);

