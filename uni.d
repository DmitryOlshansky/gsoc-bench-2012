// This code has been taken from std.uni by Dmitry Olshansky at
// https://raw.github.com/blackwhale/phobos/gsoc-2/std/uni.d

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
//            writeln("Before scaling:", delta);
            delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
            sz[n] += delta;
//            writeln("After scaling:", delta);
            storage.length +=  delta;//extend space at end
			
            //raw_slice!x must follow resize as it could be moved!
            //next 3 stmts move all data past this array, last-one-goes-first
            static if(n != dim-1)
            {
                auto start = raw_ptr!(n+1);
				//len includes delta
                size_t len = (storage.ptr+storage.length-start);

                copy(retro(start[0..len-delta])
                    , retro(start[delta..len]));
				
                start[0..delta] = 0;
                //writeln("OFFSETS before:", offsets);
                //offsets are used for raw_slice, ptr etc.
                foreach(i; n+1..dim)
                    offsets[i] += delta;
                //writeln("OFFSETS after:", offsets);
            }
        }
        else if(new_size < sz[n])
        {//shrink
            size_t delta = (sz[n] - new_size);
            delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
            sz[n] -= delta;
            writeln("Shrinking");
            //move all data past this array, forward direction
            static if(n != dim-1)
            {
                auto start = raw_ptr!(n+1);
                size_t len = storage.length;
                copy(start[delta..len]
                 , start[0..len-delta]);
                storage.length -= delta;

                //adjust offsets last, they affect raw_slice
                foreach(i; n+1..dim)
                    offsets[i] -= delta;
            }
        }
        //else - NOP
    }
private:
    @property auto raw_ptr(size_t n)()inout
    {
        static if(n == 0)
            return storage.ptr;//[0..sz[0]];
        else
        {
            return storage.ptr+offsets[n];//..offsets[n]+sz[n]];
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
        writefln("A: %(%x %)", m.slice!(k)[]);
    }

    static void fillB(size_t k, T)(ref T m, int n)
    {
        foreach(i; 0..n)
            m.slice!(k)[i] = force!ubyte(n-i);
        writefln("B: %(%x %)", m.slice!(k)[]);
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

//hope to see simillar stuff in public interface... once Allocators are out
//@@@BUG moveFront and friends? dunno, for now it's POD-only
size_t genericReplace(Policy=void, T, Range)
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
public struct GcPolicy
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
struct ReallocPolicy
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
    alias typeof(this) This;
    /**
        $(P $(D RleBitSet)s support natural syntax for set algebra, namely:)
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
                return this.addInterval(rhs, rhs+1);
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
    @property auto byChar()
    {
        static struct CharRange
        {
            this(This set)
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

private:
    ref add(uint a, uint b)
    {
        addInterval(a, b);
        return this;
    }

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

    enum isSet = true;
};

public struct RleBitSet(T, SP=GcPolicy)
    if(isUnsigned!T)
{
public:
    /*this(C)(in C[] regexSet)
        if(is(C : dchar))
    {
        assert(0);
    }
*/
    this()(uint[] intervals...)
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

    bool opIndex(uint val)
    {
        foreach(i; byInterval)
            if(val < i.b)
                return val >= i.a;
        return false;
    }

    mixin BasicSetOps;
private:
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
                    replacePad(data, start_idx, data.length, cast(T[])[]);
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
public struct InversionList(SP=GcPolicy)
{
    this(C)(in C[] regexSet)
        if(is(C : dchar))
    {
        assert(0);
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
        return assumeSorted(data[]).lowerBound(val).length & 1;
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
struct Uint24Array(SP=GcPolicy)
{
    this(Range)(Range range)
        if(isInputRange!Range && hasLength!Range)
    {
        length = range.length;
        copy(range, this[]);
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

unittest//Uint24 tests
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

unittest//core set primitives test
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

unittest
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

unittest// set operations and integer overflow ;)
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

unittest// ditto
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

unittest//even more set operations with BIG intervals
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

template WrapBinary(alias filter, alias bFn)
{
    auto WrapBinary(A)(A a, A b)
    {
        return bFn(filter(a), filter(b));
    }
}

template MapWrap(alias Fn, Ts...)
{
    static if(Ts.length > 0)
        alias TypeTuple!(WrapBinary!(Fn, Ts[0]), MapWrap!(Fn, Ts[1..$])) MapPipe;
    else
        alias TypeTuple!() MapPipe;
}

struct Trie(Value, Key, Prefix...)
    if(Prefix.length >= 1)
{
    enum TrieType{ Value, Set, Map };
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
                multiSort!(Comps, SwapStrategy.unstable)
                    (keys);
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

            for(int i=0;i<r.length; i++)
            {
                //writeln(i, " - ", r[i]);
                static if(type == TrieType.Map)
                    size_t keyIdx = getIndex(r[i][1]);
                else
                    size_t keyIdx = getIndex(r[i]);
                if(keyIdx != prevKeyIdx)
                {
                    static if(type == TrieType.Set
                              || type == TrieType.Map)
                    {
                        addValue!last(idxs, r.front.init, keyIdx - j);
                        addValue!last(idxs, r[i]);
                    }
                    else
                    {
                        addValue!last(idxs, false, keyIdx - j);
                        addValue!last(idxs, true);
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
        }
    }

    ///Construct boolean Trie from set.
    this(Set)(Set set, Key maxKey=Key.max)
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
            /*for(; i<maxKey; i++){
                writeln("### i:", i);
                foreach(j, p; Prefix)
                    writef("%1d-LVL: %d; ", j, p.entity(i));
                writeln();
                addValue!last(idxs, false);
                
            }*/
        }
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
        static if(n == size_t.max)
            return table.storage.length*size_t.sizeof;
        else
            return table.length!n * typeof(table.slice!n[0]).sizeof;
    }

    @property size_t pages(size_t n=size_t.max)() const
    {
        return (bytes!n+2^^(Prefix[$-1].bitSize-1))
                /2^^Prefix[$-1].bitSize;
    }

    version(none) static bool cmpKey(Key a, Key b)//untested, possibly bogus
    {
        foreach(p; Prefix)
        {
            if(p.entity(a) < p.entity(b))
               return true;
        }
        return false;
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
            //writeln(ptr[]);
            //writeln(indices);
            if(numVals == 1)
            {
                static if(level == Prefix.length-1 && type != TrieType.Value)
                    putValue(ptr[indices[level]], val);
                else// can incurr narrowing conversion
                    ptr[indices[level]] = force!(typeof(ptr[indices[level]]))(val);
                //writeln(indices);
                indices[level]++;
                //writeln(table.slice!level);
                numVals = 0;
            }
            else
            {
                //writeln("slice assign!");
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
                static if(level == Prefix.length-1 && type != TrieType.Value)
                {
                    for(int i=0;i<n; i++)
                        putValue(ptr[j++], val);
                }
                else
                {
                    //writeln("slice assign!");
                    ptr[j..j+n]  = val;
                    j += n;
                    //writeln(j);
                }
                indices[level] = j;

            }
            static if(level != 0)//last level has 1 "page"
            {
                alias typeof(table.slice!(level-1)[0]) NextIdx;
                NextIdx next_lvl_index;
                if(indices[level] % pageSize == 0)
                {
                    auto last = indices[level]-pageSize;
                    auto slice = ptr[indices[level] - pageSize..indices[level]];
                    //writeln(last, "   base: ", base, " ptr:", ptr);
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
    MultiArray!(idxTypes!(Key, size_t.max, true, Prefix[0..$-1]), V) table;
    pragma(msg, typeof(table));
}

/**
    Wrapping T by SetAsSlot indicates that T should be considered
    as a set of values.
    When SetAsSlot!T is used as $(D Value) type, Trie will internally
    translate assignments/test to insert & 'in' operator repspectively.
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
        writeln("INDEX (excluding value level):");
        foreach(i; Sequence!(0, t.table.dim-1) )
            writeln(t.table.slice!(i)[0..t.table.length!i]);
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
//TODO: benchmark for Trie vs InversionList vs RleBitSet vs std.bitmanip.BitArray

//
T msb(T)(T value)
    if(isUnsigned!T)
{
    size_t mask = cast(size_t)(1UL<<(T.sizeof*8-1)), i;
    for(i=T.sizeof*8-1; i<T.sizeof*8; i--, mask >>= 1)//count on overflow
    {
        if(mask & value)
            return i+1;
    }
    return 1;
}

template idxTypes(Key, size_t maxKeyIdx, bool pack, Prefix...)
{
    static if(Prefix.length == 0)
    {
        alias TypeTuple!() idxTypes;
    }
    else
    {
        //Important bitpacking note:
        //- each level has to hold enough of bits to address the next one
        static if(Prefix.length > 1)
        {
            alias TypeTuple!(BitPacked!(Prefix[1].bitSize+1, typeof(Prefix[0].entity(Key.init)))
                             , idxTypes!(Key, maxKeyIdx, pack, Prefix[1..$])) idxTypes;
        }
        else
        {//and the last one should be able to hold
            static if(pack)
                alias TypeTuple!(BitPacked!(msb(maxKeyIdx), typeof(Prefix[0].entity(Key.init)))) idxTypes;
            else
                alias TypeTuple!(typeof(Prefix[0].entity(Key.init))) idxTypes;
        }
    }
}

template bitSizeOf(T)
{
    static if(is(typeof(T.bitSize)))
        enum bitSizeOf = T.bitSize;
    else
        enum bitSizeOf = T.sizeof*8;
}

public: //Public API continues
/++
    Whether or not $(D c) is a Unicode whitespace character.
    (general Unicode category: Part of C0(tab, vertical tab, form feed,
    carriage return, and linefeed characters), Zs, Zl, Zp, and NEL(U+0085))
  +/
bool isWhite(dchar c) @safe pure nothrow
{
    return std.ascii.isWhite(c) ||
           c == lineSep || c == paraSep ||
           c == '\u0085' || c == '\u00A0' || c == '\u1680' || c == '\u180E' ||
           (c >= '\u2000' && c <= '\u200A') ||
           c == '\u202F' || c == '\u205F' || c == '\u3000';
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
private:

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
