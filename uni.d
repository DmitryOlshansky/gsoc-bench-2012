// Written in the D programming language.

/++
    Implementation of fundamental data structures and algorithms for Unicode.

    All functions in this module operate on Unicode characters and/or sets of characters. 
    For functions which operate on ASCII characters and ignore Unicode
    characters, see $(LINK2 std_ascii.html, std.ascii).

    (Short introduction to come)

    Synopsis:
    ---
    void main()
    {
        //intialize codepoint sets using regex notation
        //$(D set) contains codepoints from both scripts.
        auto set = unicode("Cyrillic") | unicode("Armenian");
        auto ascii = unicode("ASCII");
        auto currency = unicode("Currency_Symbol");

        //easy set ops
        auto a = set & ascii;
        assert(a.empty); //as it has no intersection with ascii
        a = set | ascii;
        auto b = currency - a; //subtract all ASCII, cyrilic and armenian

        //some properties of codepoint sets
        assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
        //testing presense of a codepoint in a set
        //is just fine, it is O(logN)
        assert(!b['$']); 
        assert(!b['\u058F']); // armenian dram sign
        assert(b['¥']); 

        //building fast lookup tables, these guarantee O(1) complexity
        //1-level Trie lookup table
        auto oneTrie = buildTrie!1(b);
        //2-level more compact but typically slighlty slower
        auto twoTrie = buildTrie!2(b);
        //3-level even smaller, and a bit slower yet
        auto threeTrie = buildTrie!3(b);
        assert(oneTrie['£']);
        assert(twoTrie['£']);
        assert(threeTrie['£']);
        
        //build the trie with the most sensible trie level 
        //and bind it as a functor
        auto cyrilicOrArmenian = buildLookup(set);
        auto balance = find!(cyrilicOrArmenian)("Hello ընկեր!");
        assert(balance == "ընկեր!");
        //compatible with bool delegate(dchar)
        bool delegate(dchar) bindIt = cyrilicOrArmenian;

        //Normalization
        string s = "Plain ascii (and not only), is always normalized!";
        assert(s is normalize(s));//is the same string

        string nonS = "A\u0308ffin"; //A ligature
        auto nS = normalize(nonS); //to NFC, the W3C endorsed standard
        assert(nS == "Äffin");
        assert(nS != nonS);
        string composed = "Äffin";
        
        assert(normalize!NFD(composed) == "A\u0308ffin");
        //to NFKD, compatibility decomposition useful for fuzzy matching/searching
        assert(normalize!NFKD("2¹⁰") == "210");
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
    Authors:   Dmitry Olshansky
    Source:    $(PHOBOSSRC std/_uni.d)
    Standards: $(WEB www.unicode.org/versions/Unicode6.1.0/, Unicode v6.1)
  +/
module uni;

static import std.ascii;
import std.traits, std.range, std.algorithm, std.typecons,
    std.format, std.conv, std.typetuple, std.exception, core.stdc.stdlib;
import std.array; //@@BUG UFCS doesn't work with 'local' imports
import core.bitop;
import unicode_tables;

version(X86)
    version = std_uni_unaligned_reads;
version(X86_64)
    version = std_uni_unaligned_reads;
//update to reflect all major CPUs supporting unaligned reads

enum dchar lineSep = '\u2028'; /// UTF line separator
enum dchar paraSep = '\u2029'; /// UTF paragraph separator

//test the intro example
unittest
{
    //intialize codepoint sets using regex notation
    //$(D set) contains codepoints from both scripts.
    auto set = unicode("Cyrillic") | unicode("Armenian");
    auto ascii = unicode("ASCII");
    auto currency = unicode("Currency_Symbol");

    //easy set ops
    auto a = set & ascii;
    assert(a.empty); //as it has no intersection with ascii
    a = set | ascii;
    auto b = currency - a; //subtract all ASCII, cyrilic and armenian

    //some properties of codepoint sets
    assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
    //testing presense of a codepoint in a set
    //is just fine, it is O(logN)
    assert(!b['$']); 
    assert(!b['\u058F']); // armenian dram sign
    assert(b['¥']); 

    //building fast lookup tables, these guarantee O(1) complexity
    //1-level Trie lookup table
    auto oneTrie = buildTrie!1(b);
    //2-level more compact but typically slighlty slower
    auto twoTrie = buildTrie!2(b);
    //3-level even smaller, and a bit slower yet
    auto threeTrie = buildTrie!3(b);
    assert(oneTrie['£']);
    assert(twoTrie['£']);
    assert(threeTrie['£']);
    
    //build the trie with the most sensible trie level 
    //and bind it as a functor
    auto cyrilicOrArmenian = buildLookup(set);
    auto balance = find!(cyrilicOrArmenian)("Hello ընկեր!");
    assert(balance == "ընկեր!");
    //compatible with bool delegate(dchar)
    bool delegate(dchar) bindIt = cyrilicOrArmenian;

    //Normalization
    string s = "Plain ascii (and not only), is always normalized!";
    assert(s is normalize(s));//is the same string

    string nonS = "A\u0308ffin"; //A ligature
    auto nS = normalize(nonS); //to NFC, the W3C endorsed standard
    assert(nS == "Äffin");
    assert(nS != nonS);
    string composed = "Äffin";
    
    assert(normalize!NFD(composed) == "A\u0308ffin");
    //to NFKD, compatibility decomposition useful for fuzzy matching/searching
    assert(normalize!NFKD("2¹⁰") == "210");
}

//debug = std_uni;

debug(std_uni) import std.stdio;


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

//multiple arrays squashed into one memory block
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

    this(const(size_t)[] raw_offsets, 
        const(size_t)[] raw_sizes, const(size_t)[] data)const
    {
        offsets = raw_offsets;
        sz = raw_sizes;
        storage = data;
    }

    @property auto slice(size_t n)()inout
    {
        auto ptr = raw_ptr!n;
        //size_t len = raw_length!n;
        size_t len = spaceFor!(bitSizeOf!(Types[n]))(sz[n]);
        assert(ptr + len <= storage.ptr+storage.length);
        return packedArrayView!(Unpack!(Types[n]), bitSizeOf!(Types[n]))(ptr[0..len]);
    }

    template length(size_t n)
    {
        @property size_t length()const{ return sz[n]; }

        @property void length(size_t new_size)
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

    void store(OutputRange)(OutputRange sink)
        if(isOutputRange!(OutputRange, ubyte))         
    {
        formattedWrite(sink, "[%( 0x%x, %)]", offsets[]);
        formattedWrite(sink, ", [%( 0x%x, %)]", sz[]);
        formattedWrite(sink, ", [%( 0x%x, %)]", storage);
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
        return (new_len+factor-1)/factor;
    }
}

//============================================================================
// Fast integer divison by constant 
// Useful to not rely on compiler optimizations and have speed in debug builds
// Currently GDC does this optimization, DMD doesn't
//============================================================================
struct Magic
{
    uint mul;    
    uint shift;
    bool add;
}

struct QR{
    uint q;
    uint r;
}

Magic magicNumbers(uint d)
{
    static double dumbPow2(int n)
    {
        double x = 1.0;
        for(int i=0; i<n; i++)
            x *= 2;
        return x;
    }
    import core.bitop;
    Magic m;
    assert(d > 1);
    uint r = 32 + bsr(d);
    double f = dumbPow2(r) / d;    
    double frac = f - cast(long)f;
    if(frac < 0.5)
    {
        m.mul = cast(uint)f;
        m.add = true;
    }
    else
    {
        m.mul = cast(uint)f + 1;
        m.add = false;
    }
    m.shift = r - 32;
    return m;
}

//creates q & r in local scope
static string genFastModDiv(uint div)
{
    import std.string;
    Magic m = magicNumbers(div);
    if(m.mul == 0)
        return format("auto q = (n >> %s);\nauto r = n & 0x%X;\n", 
            m.shift,  (1U<<m.shift)-1);
    else
        return format("auto q = cast(uint)(((n%s) * 0x%XUL)>>%s);\n", 
             m.add ? "+1" : "", m.mul, m.shift+32)
        ~ format("auto r = n - q * %s;\n", div);
}

auto fastModDiv(uint d)(uint n)
{
    mixin(genFastModDiv(d));
    return QR(q, r);
}

unittest
{
    //exhastive test is not feasible as unittest, uses 2M random numbers instead
    import std.random;
    uint seed = unpredictableSeed();
    Xorshift rng = Xorshift(seed);
    uint[] data = array(take(rng, 2*1000*1000));
    foreach(divisor; TypeTuple!(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15))
    {
        foreach(v; data)
        {
            auto rq = fastModDiv!divisor(v);
            assert(v / divisor == rq.q && v % divisor == rq.r, 
                text("fastDiv!", divisor," failed v=", v, " q=", rq.q,
                    " rng seed=", seed));
        }
    }
}
//============================================================================

//only per word packing, speed is more important
//doesn't own memory, only provides access
struct PackedArrayView(T, size_t bits)
    if(isIntegral!T || is(T == bool) || isSomeChar!T)
{
    import core.bitop;    
    version(std_uni_unaligned_reads)
        enum hasUnaligned = true;    

    this(inout(size_t)[] arr)inout
    {
        original = arr;
    }

    static if(factor == bytesPerWord// can pack by byte
         || ((factor == bytesPerWord/2 || factor == bytesPerWord/4) && hasUnaligned))
    {   
        //pragma(msg, text("direct for ", factor));
        static if(factor == bytesPerWord)
            alias U = ubyte;
        else static if(factor == bytesPerWord/2)
            alias U = ushort;
        else static if(factor == bytesPerWord/4)
            alias U = uint;

        T opIndex(size_t idx) inout
        {
            return cast(inout(T))(cast(U*)original.ptr)[idx];
        }

        void opIndexAssign(T val, size_t idx)
        {
            (cast(U*)original.ptr)[idx] = cast(U)val;
        }
    }
    else
    {
        //pragma(msg, text("computed for ", factor));
        T opIndex(size_t n) inout
        in
        {
            assert(n/factor < original.length, text(n/factor, " vs ", original.length));
        }
        body
        {            
            // genFastModDiv is proven to be as fast as properly
            // optimized div and faster then current DMD's one, v2.061            
            static if(factor == bytesPerWord*8)
            {   //can re-write so that there is less data dependency
                mixin(genFastModDiv(factor));
                return original[q] & (mask<<r) ? 1 : 0;
            }
            else
            {
                mixin(genFastModDiv(factor));
                return cast(T)((original[q] >> bits*r) & mask);
            }
        }

        void opIndexAssign(T val, size_t n)
        in
        {
            static if(isIntegral!T)
                assert(val <= mask, 
                    text("mask: ",mask, " bits: ", bits
                        , "value:", val, " > ", mask));
        }
        body
        {
            mixin(genFastModDiv(factor));
            size_t tgt_shift = bits*r;
            size_t word = original[q];
            original[q] = (word & ~(mask<<tgt_shift)) 
                | (cast(size_t)val << tgt_shift);
        }
    }


    void opSliceAssign(T val, size_t start, size_t end)
    {
        //rounded to factor granuarity
        //TODO: re-test and implement
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
        return sliceOverIndexed(from, to, &this);
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
    enum bytesPerWord =  size_t.sizeof;
    size_t[] original;
}


private struct SliceOverIndexed(T)
{
    enum assignableIndex = is(typeof((){ T.init[0] = Item.init; }));
    enum assignableSlice = is(typeof((){ T.init[0..0] = Item.init; }));

    auto opIndex(size_t idx)const
    in
    {
        assert(idx < to - from);
    }
    body
    {
        return arr.opIndex(from+idx);
    }

    static if(assignableIndex)
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
        return typeof(this)(from+a, from+b, arr);
    }

    //static if(assignableSlice)
    void opSliceAssign(T)(T val, size_t start, size_t end)
    {
        return (*arr)[start+from, end+from] = val;
    }

    auto opSlice()
    {
        return opSlice(from, to);
    }

    @property size_t length()const { return to-from;}

    auto opDollar()const { return length; }

    @property bool empty()const { return from == to; }

    @property auto front()const { return (*arr)[from]; }

    static if(assignableIndex)
    @property void front(Item val) { (*arr)[from] = val; }

    @property auto back()const { return (*arr)[to-1]; }

    static if(assignableIndex)
    @property void back(Item val) { (*arr)[to-1] = val; }

    @property auto save() inout { return this; }

    void popFront() {   from++; }

    void popBack() {    to--; }

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

//BUG? forward reference to return type of sliceOverIndexed!Grapheme
SliceOverIndexed!(const(T)) sliceOverIndexed(T)(size_t a, size_t b, const(T)* x)
    if(is(Unqual!T == T))
{
    return SliceOverIndexed!(const(T))(a, b, x);
}

//BUG? inout is out of reach 
//...SliceOverIndexed.arr only parameters or stack based variables can be inout
SliceOverIndexed!T sliceOverIndexed(T)(size_t a, size_t b, T* x)
    if(is(Unqual!T == T))
{
    return SliceOverIndexed!T(a, b, x);
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

//============================================================================
// Partially unrolled binary search using Shar's method
//============================================================================

string genUnrolledSwitchSearch(size_t size)
{
    assert(isPowerOf2(size));
    string code = `auto power = bsr(m)+1;
    switch(power){`;
    size_t i = bsr(size);
    foreach(v; iota(0, bsr(size)).retro.map!"2^^a")
    {
        code ~= `
        case pow:
            if(pred(range[idx+m], needle))
                idx +=  m;
            goto case;
        `.replace("m", to!string(v))
        .replace("pow", to!string(i));   
        i--;
    }
    code ~= `
        case 0:
            if(pred(range[idx], needle))
                idx += 1;
            goto default;
        `;  
    code ~= `
        default:
    }`;
    return code;
}

bool isPowerOf2(size_t sz)
{
    return (sz & (sz-1)) == 0;
}

size_t uniformLowerBound(alias pred, Range, T)(Range range, T needle)
    if(is(T : ElementType!Range))
{
    assert(isPowerOf2(range.length));
    size_t idx = 0, m = range.length/2;        
    while(m != 0)
    {
        if(pred(range[idx+m], needle))
            idx += m;
        m /= 2;        
    }
    if(pred(range[idx], needle))
        idx += 1;
    return idx;
}

size_t switchUniformLowerBound(alias pred, Range, T)(Range range, T needle)
    if(is(T : ElementType!Range))
{
    assert(isPowerOf2(range.length));
    size_t idx = 0, m = range.length/2;
    enum max = 1<<10;
    while(m >= max)
    {
        if(pred(range[idx+m], needle))
            idx += m;
        m /= 2;
    }
    mixin(genUnrolledSwitchSearch(max));
    return idx;
}
   

size_t prevPowerOf2(size_t arg)
{
    assert(arg > 1); //else bsr is undefined
    return 1<<bsr(arg-1);
}

size_t nextPowerOf2(size_t arg)
{
    assert(arg > 1); //else bsr is undefined
    return 1<<bsr(arg-1)+1;
}

template sharMethod(alias uniLowerBound)
{
    size_t sharMethod(alias _pred="a<b", Range, T)(Range range, T needle)
        if(is(T : ElementType!Range))
    {
        import std.functional;
        alias binaryFun!_pred pred;
        if(range.length == 0)
            return 0;
        if(isPowerOf2(range.length))
            return uniLowerBound!pred(range, needle);
        size_t n = prevPowerOf2(range.length);
        if(pred(range[n-1], needle))
        {//search in another 2^^k area that fully covers the tail of range
            size_t k = nextPowerOf2(range.length - n + 1);
            return range.length - k + uniLowerBound!pred(range[$-k..$], needle);
        }
        else
            return uniLowerBound!pred(range[0..n], needle);
    }
}

alias sharMethod!uniformLowerBound sharLowerBound;
alias sharMethod!switchUniformLowerBound sharSwitchLowerBound;

unittest
{
    auto stdLowerBound(T)(T[] range, T needle)
    {
        return assumeSorted(range).lowerBound(needle).length;
    }
    immutable MAX = 5*1173;
    auto arr = array(iota(5, MAX, 5));
    assert(arr.length == MAX/5-1);
    foreach(i; 0..MAX+5)
    {
        auto std = stdLowerBound(arr, i);
        assert(std == sharLowerBound(arr, i));
        assert(std == sharSwitchLowerBound(arr, i));
    }
    arr = [];
    auto std = stdLowerBound(arr, 33);
    assert(std == sharLowerBound(arr, 33));
    assert(std == sharSwitchLowerBound(arr, 33));
}
//============================================================================

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
//TODO: stop working around bugs, report them!
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
        debug 
        {
            arr[] = cast(typeof(T.init[0]))(0xdead_beef); 
        }
    }

    static void destroy(T)(ref T arr)
        if(isDynamicArray!T && !is(Unqual!T == T))
    { /*NOP*/ }
}

//ditto
@trusted struct ReallocPolicy
{    
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
        if(arr.ptr)
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

/**
    Checks if T is some kind a set of codepoints. Intended for template constraints.
    TODO: decribe operations provided by any codepoint set.
*/
public template isCodepointSet(T)
{
    enum isCodepointSet = is(typeof(T.init.isSet));
}

/**
    Checks if $(D T) is a pair of integers that implicitly convert to $(D V).
    The following code must compile for any pair $(D T):
    ---
    (T x){ V a = x[0]; V b = x[1];}    
    ---
    The following must not compile:
     ---
    (T x){ V c = x[2];}    
    ---
*/
public template isIntegralPair(T, V=uint)
{
    enum isIntegralPair = is(typeof((T x){ V a = x[0]; V b = x[1];}))
        && !is(typeof((T x){ V c = x[2]; }));
}

//bootstrap full set operations from 4 primitives:
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
        if(isCodepointSet!U || is(U:dchar))
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
        if(isCodepointSet!U || is(U:dchar))
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
                r = set.byInterval;
                if(!r.empty)                    
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
    /**
        Add an interval [a, b) to this set.
    */
    ref add()(uint a, uint b)
    {
        addInterval(a, b);
        return this;
    }
    enum isSet = true;
private:

    ref intersect(U)(in U rhs)
        if(isCodepointSet!U)
    {
        Marker mark;
        foreach( i; rhs.byInterval)
        {
            mark = this.dropUpTo(i.a, mark);
            mark = this.skipUpTo(i.b, mark);
        }
        this.dropUpTo(uint.max, mark);
        return this;
    }

    ref intersect()(dchar ch)
    {
        foreach(i; byInterval)
            if(i.a >= ch && ch < i.b)
                return this = This.init.add(ch, ch+1);
        this = This.init;
        return this;
    }

    ref sub()(dchar ch)
    {
        //workaround a BUG, somehow overload below doesn't survive if base class has sub(dchar)
        return subChar(ch);
    }

    //same as the above except that skip & drop parts are swapped
    ref sub(U)(in U rhs)
        if(isCodepointSet!U)
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

    ref add(U)(in U rhs)
        if(isCodepointSet!U)
    {
        Marker start;
        foreach(i; rhs.byInterval)
        {
            start = addInterval(i.a, i.b, start);
        }
        return this;
    }

}



///The recommended default type for set of codepoints.
public alias InversionList!GcPolicy CodepointSet;

/**
    The recommended type of $(XREF std._typecons, Tuple)
    to represent [a, b) intervals of codepoints.
*/
public alias Tuple!(uint, "a", uint, "b") CodepointInterval;

/**
    $(D InversionList) is a packed data structure for a set of codepoints.
    Memory usage is 6 bytes per each contigous interval in a set.
*/
@trusted public struct InversionList(SP=GcPolicy)
{
public:
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

    this(Range)(Range intervals)
        if(isInputRange!Range && isIntegralPair!(ElementType!Range))
    {
        auto flattened = roundRobin(intervals.save.map!"a[0]", intervals.save.map!"a[1]");
        data = Uint24Array!(SP)(flattened);
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
    {//TODO: COW?
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
                uint a = read24(slice.ptr, 0);
                uint b = read24(slice.ptr, 1);
                return CodepointInterval(a, b);
            }

            @property auto back()const
            {
                uint a = read24(slice.ptr, slice.length/3 - 2);
                uint b = read24(slice.ptr, slice.length/3 - 1);
                return CodepointInterval(a, b);
            }

            void popFront()
            {
                slice = slice[6..$];
            }

            void popBack()
            {
                slice = slice[0..$-6];
            }

            @property bool empty()const { return slice.empty; }

            @property auto save(){ return this; }
        private:
            const(ubyte)[] slice;
        }
        return Intervals(data.data);
    }

    bool opIndex(uint val) const
    {
        // the <= ensures that searching in  interval of [a, b) for 'a' you get .length == 1
        //return assumeSorted!((a,b) => a<=b)(data[]).lowerBound(val).length & 1;
        return sharSwitchLowerBound!"a<=b"(data[], val) & 1;
    }

    ///Number of characters in this set
    @property size_t length() const
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

    /**
        Generates string with D source code of function that tests if the codepoint
        belongs to this set or not. The result is to be used with string mixin.
        The inteded usage area is agressive optimization via meta programming 
        in parsers generators and the like.

        $(I Notes): to be used with care for relatively small or regular sets. It
        could be end up being slower then just using multi-staged tables.
        Example:
        ---
        TODO: add an example
        ---
    */    
    string toSourceCode(string funcName="")
    {
        import std.string;        
        enum maxBinary = 3;
        static string linearScope(R)(R ivals, string indent)
        {
            string result = indent~"{\n";
            string deeper = indent~"    ";
            foreach(ival; ivals)
            {
                auto span = ival[1] - ival[0];
                assert(span != 0);
                if(span == 1)
                {
                    result ~= format("%sif(ch == %s) return true;\n", deeper, ival[0]);
                }
                else if(span == 2)
                {
                    result ~= format("%sif(ch == %s || ch == %s) return true;\n", 
                        deeper, ival[0], ival[0]+1);
                }
                else
                {
                    if(ival[0] != 0) //dchar is unsigned and  < 0 is useless
                        result ~= format("%sif(ch < %s) return false;\n", deeper, ival[0]);
                    result ~= format("%sif(ch < %s) return true;\n", deeper, ival[1]);
                }
            }
            result ~= format("%sreturn false;\n%s}\n", deeper, indent); //including empty range of intervals
            return result;
        }

        static string binaryScope(R)(R ivals, string indent)
        { 
            //time to do unrolled comparisons?
            if(ivals.length < maxBinary)
                return linearScope(ivals, indent);
            else
                return bisect(ivals, ivals.length/2, indent);
        }

        //not used yet if/elsebinary search is far better with DMD  as of 2.061
        //and GDC is doing fine job either way
        static string switchScope(R)(R ivals, string indent)
        {
            string result = indent~"switch(ch){\n";
            string deeper = indent~"    ";
            foreach(ival; ivals)
            {
                if(ival[0]+1 == ival[1])
                {
                    result ~= format("%scase %s: return true;\n", 
                        deeper, ival[0]);
                }
                else
                {
                    result ~= format("%scase %s: .. case %s: return true;\n",
                         deeper, ival[0], ival[1]-1);
                } 
            }
            result ~= deeper~"default: return false;\n"~indent~"}\n";
            return result;
        }

        static string bisect(R)(R range, size_t idx, string indent)
        {
            string deeper = indent ~ "    ";
            //bisect on one [a, b) interval at idx
            string result = indent~"{\n";
            //less branch, < a
            result ~= format("%sif(ch < %s)\n%s", 
                deeper, range[idx][0], binaryScope(range[0..idx], deeper));            
            //middle point,  >= a && < b
            result ~= format("%selse if (ch < %s) return true;\n", 
                deeper, range[idx][1]);
            //greater or equal branch,  >= b
            result ~= format("%selse\n%s", 
                deeper, binaryScope(range[idx+1..$], deeper));
            return result~indent~"}\n";
        }

        string code = format("bool %s(dchar ch)\n", funcName.empty ? "function" : funcName);
        auto range = byInterval.array;
        //special case first bisection to be on ASCII vs beyond
        auto tillAscii = countUntil!"a[0] > 0x80"(range);
        if(tillAscii <= 0) //everything is ASCII or nothing is ascii (-1 & 0)
            code ~= binaryScope(range, "");
        else
            code ~= bisect(range, tillAscii, "");
        return code;
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
        assert(a <= b, text(a, " > ", b));
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

//pedantic version for ctfe, and aligned-access only architectures
@trusted uint safeRead24(const ubyte* ptr, size_t idx)
{
    idx *= 3;
    version(LittleEndian)
        return ptr[idx] + (cast(uint)ptr[idx+1]<<8)
             + (cast(uint)ptr[idx+2]<<16);
    else
        return (cast(uint)ptr[idx]<<16) + (cast(uint)ptr[idx+1]<<8)
             + ptr[idx+2];
}

//ditto
@trusted void safeWrite24(ubyte* ptr, uint val, size_t idx)
{
    idx *= 3;
    version(LittleEndian)
    {
        ptr[idx] = val & 0xFF;
        ptr[idx+1] = (val>>8) & 0xFF;
        ptr[idx+2] = (val>>16) & 0xFF;
    }
    else
    {
        ptr[idx] = (val>>16) & 0xFF;
        ptr[idx+1] = (val>>8) & 0xFF;
        ptr[idx+2] = val & 0xFF;
    }
}

//unaligned x86-like read/write functions
@trusted uint unalignedRead24(const ubyte* ptr, size_t idx)
{
    uint* src = cast(uint*)(ptr+3*idx);
    version(LittleEndian)
        return *src & 0xFF_FFFF;
    else
        return *src >> 8;
}

//ditto
@trusted void unalignedWrite24(ubyte* ptr, uint val, size_t idx)
{
    uint* dest = cast(uint*)(cast(ubyte*)ptr + 3*idx);
    version(LittleEndian)
        *dest = val | (*dest & 0xFF00_0000);
    else
        *dest = (val<<8) | (*dest & 0xFF);
}

uint read24(const ubyte* ptr, size_t idx)
{
    if(__ctfe)
        return safeRead24(ptr, idx);
    version(std_uni_unaligned_reads)
        return unalignedRead24(ptr, idx);
    else
        return safeRead24(ptr, idx);
}

void write24(ubyte* ptr, uint val, size_t idx)
{
    if(__ctfe)
        return safeWrite24(ptr, val, idx);
    version(std_uni_unaligned_reads)
        return unalignedWrite24(ptr, val, idx);
    else
        return safeWrite24(ptr, val, idx);    
}

//Packed array of 24-bit integers.
@trusted struct Uint24Array(SP=GcPolicy)
{
    this(Range)(Range range)
        if(isInputRange!Range && hasLength!Range)
    {
        length = range.length;
        copy(range, this[]);
    }

    this(Range)(Range range)
        if(isInputRange!Range && !hasLength!Range)
    {
        auto a = array(range); //TODO: use better things like appending to Uint24Array
        this(a);
    }

    this(this)
    {
        data = SP.dup(data);
    }

    static if(!is(SP :GcPolicy))
    ~this()
    {
        SP.destroy(data);
    }

    @property size_t length()const { return data.length/3; }

    @property void length(size_t len)
    {
        data = SP.realloc(data, len*3);
    }

    ///Read 24-bit packed integer
    uint opIndex(size_t idx)const
    {
        return read24(data.ptr, idx);
    }

    ///Write 24-bit packed integer
    void opIndexAssign(uint val, size_t idx)
    in
    {
        assert(val <= 0xFF_FFFF);
    }
    body
    {
        write24(data.ptr, val, idx);        
    }

    //
    auto opSlice(size_t from, size_t to)
    {
        return sliceOverIndexed(from, to, &this);
    }

    auto opSlice(size_t from, size_t to)const
    {
        return sliceOverIndexed(from, to, &this);
    }

    //
    auto opSlice()
    {
        return opSlice(0, length);
    }

    auto opSlice() const
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
        if(isInputRange!Range && hasLength!Range && is(ElementType!Range : uint))
    {
        size_t nl = length + range.length;
        length = nl;
        copy(range, this[nl-range.length..nl]);
    }

    bool opEquals(const ref Uint24Array rhs)const
    {
        return data[0..data.length]
            == rhs.data[0..rhs.data.length];
    }
private:

    ubyte[] data;
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

private alias TypeTuple!(InversionList!GcPolicy, InversionList!ReallocPolicy) AllSets;

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
        assert(a == CodeList(10, 60), text(a));

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
        assert(c == CodeList(20, 200), text(CodeList.stringof," ", c));
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


@system:
unittest// vs single dchar
{
    CodepointSet a = CodepointSet(10, 100, 120, 200);
    assert(a - 'A' == CodepointSet(10, 65, 66, 100, 120, 200), text(a - 'A'));
    assert((a & 'B') == CodepointSet(66, 67));
}

unittest//iteration & opIndex
{
    import std.typecons;
    foreach(CodeList; AllSets)
    {
        auto arr = "ABCDEFGHIJKLMabcdefghijklm"d;
        auto a = CodeList('A','N','a', 'n');
        assert(equal(a.byInterval, 
                [tuple(cast(uint)'A', cast(uint)'N'), tuple(cast(uint)'a', cast(uint)'n')]
            ), text(a.byInterval));
        assert(equal(retro(a.byInterval), 
                [tuple(cast(uint)'a', cast(uint)'n'), tuple(cast(uint)'A', cast(uint)'N')]
            ), text(retro(a.byInterval)));
        assert(equal(a.byChar, arr), text(a.byChar));
        foreach(ch; a.byChar)
            assert(a[ch]);
        auto x = CodeList(100, 500, 600, 900, 1200, 1500);
        assert(equal(x.byInterval, [ tuple(100, 500), tuple(600, 900), tuple(1200, 1500)]), text(x.byInterval));
        foreach(ch; x.byChar)
            assert(x[ch]);
        static if(is(CodeList == CodepointSet))
        {
            auto y = CodeList(x.byInterval);
            assert(equal(x.byInterval, y.byInterval));
        }
        assert(equal(CodepointSet.init.byInterval, cast(Tuple!(uint, uint)[])[]));
        assert(equal(CodepointSet.init.byChar, cast(dchar[])[]));
    }
}

@trusted public struct Trie(Value, Key, Prefix...)
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
        alias V Item;
        enum type = TrieType.Value;
        static putValue(ref V x, V val)
        {
            x = val;
        }
    }

    private size_t bootstrap(size_t[Prefix.length] idxs)
    {
        enum pageBits=Prefix[$-1].bitSize, pageSize = 1<<pageBits;
        size_t maxIdx = 1;
        foreach(v; Prefix)
            maxIdx *= 2^^v.bitSize;

        table = typeof(table)(idxs);
        //one page per level is bootstrap minimum
        foreach(i; Sequence!(0, Prefix.length))
            table.length!i = (1<<Prefix[i].bitSize);
        return maxIdx;
    }

    static if(type == TrieType.Value)
    this()(Value[Key] hash, Value defvalue=Value.init)
    {
        ConstructState[Prefix.length] emptyFull;//empty page index, full page index
        size_t[Prefix.length] idxs;
        enum last = Prefix.length-1;
        size_t maxIdx = bootstrap(idxs);

        auto r = array(zip(hash.values, hash.keys));
        alias GetComparators!(Prefix.length, cmpK0) Comps;
        multiSort!Comps(r);

        size_t j = 0;
        for(size_t i=0;i<r.length; i++)
        {

            size_t keyIdx = getIndex(r[i][1]);
            //writeln(keyIdx);
            addValue!last(idxs, defvalue, emptyFull[], keyIdx - j);
            //writeln("~~~~~~~~~~~~~~");
            addValue!last(idxs, r[i][0], emptyFull[]);
            //writeln("~~~~~~~~~~~~~~");
            j = keyIdx+1;
        }
        addValue!last(idxs, defvalue, emptyFull[], maxIdx-j);
    }

    static if(type == TrieType.Map)
    this()(Item[Key] hash)
    {
        ConstructState[Prefix.length] emptyFull;//empty page index, full page index
        size_t[Prefix.length] idxs;
        enum last = Prefix.length-1;
        size_t maxIdx = bootstrap(idxs);

        auto r = array(zip(hash.byValue, hash.byKey));
        alias GetComparators!(Prefix.length, cmpK0) Comps;
        multiSort!Comps(r);

        size_t j = 0;
        size_t prevKeyIdx = size_t.max;
        for(size_t i=0;i<r.length; i++)
        {
            size_t keyIdx = getIndex(r[i][1]);
            if(keyIdx != prevKeyIdx)
            {
                addValue!last(idxs, r.front.init, emptyFull[], keyIdx - j);
                addValue!last(idxs, r[i], emptyFull[]);                
                j = keyIdx+1;
                prevKeyIdx = keyIdx;
            }
            else
            {//duplicate keyIdx, quite possible with MapAsSlot
                idxs[last]--;
                addValue!last(idxs, r[i], emptyFull[]);
            }
        }
        addValue!last(idxs, r.front.init, emptyFull[], maxIdx-j);
    }

    ///Construct Trie from array of keys
    ///fills all possible keys with zeros in index
    this(Keys)(Keys keys)
        if(!is(typeof(Keys.init.isSet)) && !isAssociativeArray!Keys)
    {
        ConstructState[Prefix.length] emptyFull; //empty page index, full page index
        enum last = Prefix.length-1;
        enum pageBits=Prefix[$-1].bitSize, pageSize = 1<<pageBits;
        size_t maxIdx = 1;
        //maximum index is sizes of each level multiplied
        foreach(v; Prefix)
            maxIdx *= 2^^v.bitSize;

        size_t[Prefix.length] idxs;
        table = typeof(table)(idxs);
        //one page per level is bootstrap minimum
        foreach(i; Sequence!(0, Prefix.length))
            table.length!i = (1<<Prefix[i].bitSize);

        {//don't pollute the ctor scope
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
            else
                static assert(false, "unsupported constructor for "~Keys.stringof);

            for(size_t i=0;i<r.length; i++)
            {
                size_t keyIdx = getIndex(r[i]);
                if(keyIdx != prevKeyIdx)
                {
                    static if(type == TrieType.Value && is(V == bool))
                    {
                        addValue!last(idxs, false, emptyFull[], keyIdx - j);
                        addValue!last(idxs, true, emptyFull[]);
                    }
                    else
                    {
                        addValue!last(idxs, r.front.init, emptyFull[], keyIdx - j);
                        addValue!last(idxs, r[i], emptyFull[]);
                    }
                    prevKeyIdx = keyIdx;
                    j = keyIdx+1;
                }
                else
                {//Set or map version can have "duplicate" slot keys
                     static if(type == TrieType.Set)
                     {
                        idxs[last]--;
                        addValue!last(idxs, r[i], emptyFull[]);
                     }
                }

            }
            static if(type == TrieType.Set)
                addValue!last(idxs, Key.init, emptyFull[], maxIdx-j);
            else
                addValue!last(idxs, false, emptyFull[], maxIdx-j);
        }
    }

    ///Construct boolean Trie from set.
    this(Set)(in Set set, Key maxKey=Key.max)
        if(is(typeof(Set.init.isSet)))
    {
        ConstructState[Prefix.length] emptyFull; //empty page index, full page index
        foreach(ref v; emptyFull)
            v = ConstructState(true, true, uint.max, uint.max);
        enum last = Prefix.length-1;
        enum pageBits=Prefix[$-1].bitSize, pageSize = 1<<pageBits;
        maxKey =((maxKey + pageSize-1)>>pageBits)<<pageBits;

        auto ivals = set.byInterval;
        size_t[Prefix.length] idxs;


        table = typeof(table)(idxs);
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

                addValue!last(idxs, false, emptyFull[], a - i);
                i = a;
                assert(i <= maxKey, text("set has keys > maxKey in Trie c-tor: ", i, " vs ", maxKey));
                addValue!last(idxs, true, emptyFull[], b - i);
                i = b;

                ivals.popFront();
            }
            addValue!last(idxs, false, emptyFull[], maxKey - i);
        }
    }

    //only for constant Tries constructed from preconstructed tables
    private this()(const(size_t)[] offsets, const(size_t)[] sizes, 
        const(size_t)[] data) const
    {
        table = typeof(table)(offsets, sizes, data);
    }

    inout(V) opIndex(Key key) inout
    {
        size_t idx;
        alias Prefix p;
        idx = cast(size_t)p[0].entity(key);
        foreach(i, v; p[0..$-1])
            idx = cast(size_t)((table.slice!i[idx]<<p[i+1].bitSize) + p[i+1].entity(key));
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
    static if(type == TrieType.Map || type==TrieType.Value)
    static bool cmpK0(size_t i)
        (const ref Tuple!(Item,Key) a, const ref Tuple!(Item, Key) b)
    {
        return Prefix[i].entity(a[1]) < Prefix[i].entity(b[1]);
    }

    void store(OutputRange)(scope OutputRange sink)
        if(isOutputRange!(OutputRange, ubyte))
    {
        table.store(sink);
    }

private:
    struct ConstructState//used during creation of Trie
    {
        bool empty, full; //current page is empty? full?
        uint idx_empty, idx_full;
    }
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
            //writeln(i, ": ", cast(size_t) p[i].entity(key));
            idx |= p[i].entity(key);
            idx <<= p[i+1].bitSize;
        }
        //writeln(p.length-1, ": ", cast(size_t) p[$-1].entity(key));
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
    void addValue(size_t level, T)(size_t[] indices, T val, ConstructState[] emptyFull, size_t numVals=1)
    {
        enum pageSize = 1<<Prefix[level].bitSize;
        if(numVals == 0)
            return;
        do
        {
            //need to take pointer again, memory block  may move on resize
            auto ptr = table.slice!(level);
            static if(is(T : bool))
            {
                if(val)
                    emptyFull[level].empty = false;
                else
                    emptyFull[level].full = false;
            }
            if(numVals == 1)
            {
                static if(level == Prefix.length-1 && type != TrieType.Value)
                    putValue(ptr[indices[level]], val);
                else{// can incurr narrowing conversion
                    assert(indices[level] < ptr.length);
                    ptr[indices[level]] = force!(typeof(ptr[indices[level]]))(val);
                }
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
                    static if(is(T : bool))
                    {
                        if(emptyFull[level].empty)
                        {
                            if(emptyFull[level].idx_empty == uint.max)
                            {
                                emptyFull[level].idx_empty = cast(uint)(indices[level]/pageSize - 1);
                                goto L_allocate_page;
                            }
                            else
                            {
                                next_lvl_index = cast(NextIdx)emptyFull[level].idx_empty;
                                indices[level] -= pageSize;//it is a duplicate
                                goto L_know_index;
                            }
                        }                        
                    }
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
                    L_allocate_page:    
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
                    L_know_index:
                    static if(is(T : bool))
                    {
                        emptyFull[level].empty = true;
                        emptyFull[level].full = true;
                    }

                    addValue!(level-1)(indices, next_lvl_index, emptyFull);
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

template GetBitSlicing(size_t Top, Sizes...)
{
    static if(Sizes.length > 0)
        alias TypeTuple!(sliceBits!(Top - Sizes[0], Top)
            , GetBitSlicing!(Top - Sizes[0], Sizes[1..$])) GetBitSlicing;
    else
        alias TypeTuple!()  GetBitSlicing;
}

/**
    Wrapper for generic Trie template to simplify mapping unicode codepoints
    to bool. As the name suggests it could be treated as a set of characters
    packed into multi-stage table to provide fast lookup.
    
    Example:

    ---
    {
        import std.stdio;
        auto set = unicode("Number");
        auto trie = CodepointSetTrie!(8, 5, 8)(set);
        foreach(line; stdin.byLine)
        {
            int count=0;
            foreach(dchar ch; line)
                if(trie[ch])//is number
                    count++;
            writefln("Contains %d number characters.", count);
        }
    }
    ---
*/
public template CodepointSetTrie(Sizes...)
{
    alias Trie!(bool, dchar, GetBitSlicing!(21, Sizes)) CodepointSetTrie;
}

/**
    A more general wrapper for generic Trie template. Specifically it's allows
    creating mappings of codepoints to an arbritrary type.
    Keep in mind that CodepointSets will naturally convert only to bool mappings.
*/
public template CodepointTrie(T, Sizes...)
{
    alias Trie!(T, dchar, GetBitSlicing!(21, Sizes)) CodepointTrie;
}

/++
    Convinience function to construct optimal configurations for CodepointTrie 
    of 1, 2, 3 or 4 levels. 

    Level 1 indicates a plain bitset and uses the most space.
    Level 2 & 3 add 1 or 2 levels of indices that greately save on required
    space but typically a bit slower to lookup.
+/
public auto buildTrie(size_t level, Set)(in Set set)
    if(isCodepointSet!Set)
{
    static if(level == 1)
        return CodepointSetTrie!(21)(set);
    else static if(level == 2)
        return CodepointSetTrie!(10, 11)(set);
    else static if(level == 3)
        return CodepointSetTrie!(8, 5, 8)(set);
    else static if(level == 4)
         return CodepointSetTrie!(6, 4, 4, 7)(set);
    else
        static assert(false, "Sorry, buildTrie doesn't support level > 4, use CodepointSetTrie directly");
}

/++
    Builds Trie with typically optimal space-time tradeoff and wraps into delegate of the form:
    delegate bool (dchar ch);

    Effectively this creates a 'tester' object suitable for algorithms like std.algorithm.find
    that take unary predicates.
+/
public auto buildLookup(Set)(in Set set)
    if(isCodepointSet!Set)
{
    auto t = buildTrie!2(set);// revise as small sets typically better packed with 2 level trie
    return (dchar ch) => t[ch];
}

/**
    Wrapping T by SetAsSlot indicates that T should be considered
    as a set of values.
    When SetAsSlot!T is used as $(D Value) type, $(D Trie) template will internally
    translate assignments/tests to insert & 'in' operator repspectively.
*/
public struct SetAsSlot(T){}

 /**
    Wrapping T by MapAsSlot indicates that T should be considered
    as a map Key -> Value.
    When MapAsSlot!T is used as $(D Value) type, $(D Trie) template will internally
    translate assignments/tests to insert & 'in' operator repspectively.
*/
public struct MapAsSlot(T, Value, Key){}

/**
    Wrapper, used in definition of custom data structures from $(D Trie) template.
    Use it on a lambda function to indicate that returned value always
     fits within $(D bits) of bits.
*/
public template assumeSize(size_t bits, alias Fn)
{
    enum bitSize = bits;
    alias Fn entity;
}

//indicates MultiArray to apply bit packing to this field
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

/++
    A helper for defining lambda function that yileds a slice 
    of sertain bits from integer value.
    The resulting lambda is wrapped in assumeSize and can be used directly 
    with $(D Trie) template.
+/
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
        debug(std_uni)
        {
            writeln("---TRIE FOOTPRINT STATS---");
            foreach(i; Sequence!(0, t.table.dim) )
            {
                writefln("lvl%s = %s bytes;  %s pages"
                         , i, t.bytes!i, t.pages!i);
            }
            writefln("TOTAL: %s bytes", t.bytes);
            version(none)
            {
                writeln("INDEX (excluding value level):");
                foreach(i; Sequence!(0, t.table.dim-1) )
                    writeln(t.table.slice!(i)[0..t.table.length!i]);
            }
            writeln("---------------------------");
        }
    }
    //@@@BUG link failure, lambdas not found by linker somehow (in case of trie2)
    //alias assumeSize!(8, function (uint x) { return x&0xFF; }) lo8;
    //alias assumeSize!(7, function (uint x) { return (x&0x7F00)>>8; }) next8;
    alias CodepointSet Set;
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

    auto redundant2 = Set(
        1, 18, 256+2, 256+111, 512+1, 512+18, 768+2, 768+111);
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
        //thus it's size in pages is full_bit_width - size_of_last_prefix
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


int comparePropertyName(Char1, Char2)(const(Char1)[] a, const(Char2)[] b)
{
    for(;;)
    {
        while(!a.empty && (isWhite(a.front) || a.front == '-' || a.front =='_'))
        {
            a.popFront();
        }
        while(!b.empty && (isWhite(b.front) || b.front == '-' || b.front =='_'))
        {
            b.popFront();
        }
        if(a.empty)
            return b.empty ? 0 : -1;
        if(b.empty)
            return 1;
        auto ca = std.ascii.toLower(a.front), cb = std.ascii.toLower(b.front);
        if(ca > cb)
            return 1;
        else if( ca < cb)
            return -1;
        a.popFront();
        b.popFront();
    }
}

bool propertyNameLess(Char1, Char2)(const(Char1)[] a, const(Char2)[] b)
{
    return comparePropertyName(a, b) < 0;
}

//============================================================================
//Utilities for compression of unicode character sets


void compressTo(uint val, ref ubyte[] arr) pure nothrow
{
    //not optimized as usually done 1 time (and not public interface)
    if(val < 128)
        arr ~= cast(ubyte)val;
    else if(val < (1<<13))
    {
        arr ~= (0b1_00<<5) | cast(ubyte)(val>>8);
        arr ~= val & 0xFF;
    }
    else        
    {
        assert(val < (1<<21));
        arr ~= (0b1_01<<5) | cast(ubyte)(val>>16);
        arr ~= (val >> 8) & 0xFF;
        arr ~= val  & 0xFF;
    }
}

uint decompressFrom(const(ubyte)[] arr, ref size_t idx) pure
{
    uint first = arr[idx++];
    if(!(first & 0x80)) //no top bit -> [0..127]
        return first;
    uint extra = ((first>>5) & 1) + 1; // [1, 2]
    uint val = (first & 0x1F);
    enforce(idx + extra <= arr.length, "bad codepoint interval encoding");
    foreach(j; 0..extra)
        val = (val<<8) | arr[idx+j];
    idx += extra;
    return val;
}

//compres
public ubyte[] compressIntervals(Range)(Range intervals)
    if(isInputRange!Range && isIntegralPair!(ElementType!Range))
{
    ubyte[] storage;
    uint base = 0;
    //RLE encode
    foreach(val; intervals)
    {        
        compressTo(val[0]-base, storage);
        base = val[0];
        if(val[1] != lastDchar+1) //till the end of domain so don't store it
        {
            compressTo(val[1]-base, storage);
            base = val[1];
        }
    }
    return storage;
}

unittest
{
    auto run = [tuple(80, 127), tuple(128, (1<<10)+128)];
    ubyte[] enc = [cast(ubyte)80, 47, 1, (0b1_00<<5) | (1<<2), 0];
    assert(compressIntervals(run) == enc);
    auto run2 = [tuple(0, (1<<20)+512+1), tuple((1<<20)+512+4, lastDchar+1)];
    ubyte[] enc2 = [cast(ubyte)0, (0b1_01<<5) | (1<<4), 2, 1, 3]; //odd length-ed
    assert(compressIntervals(run2) == enc2);
    size_t  idx = 0;
    assert(decompressFrom(enc, idx) == 80);
    assert(decompressFrom(enc, idx) == 47);
    assert(decompressFrom(enc, idx) == 1);
    assert(decompressFrom(enc, idx) == (1<<10));
    idx = 0;
    assert(decompressFrom(enc2, idx) == 0);
    assert(decompressFrom(enc2, idx) == (1<<20)+512+1);
    assert(equal(decompressIntervals(compressIntervals(run)), run));
    assert(equal(decompressIntervals(compressIntervals(run2)), run2));
}

///Creates a range of $(D CodepointInterval) that lazily decodes compressed data.
//TODO: make it package
public auto decompressIntervals(const(ubyte)[] data)
{
    return DecompressedIntervals(data);
}

struct DecompressedIntervals
{
    const(ubyte)[] _stream;
    size_t _idx;
    CodepointInterval _front;

    this(const(ubyte)[] stream)
    {
        _stream = stream;
        popFront();
    }

    @property CodepointInterval front()
    {
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        if(_idx == _stream.length)
        {
            _idx = size_t.max;
            return;
        }
        uint base = _front[1];                
        _front[0] = base + decompressFrom(_stream, _idx);
        if(_idx == _stream.length)//odd length ---> till the end
            _front[1] = lastDchar+1;
        else
        {
            base = _front[0];
            _front[1] = base + decompressFrom(_stream, _idx);
        }
    }

    @property bool empty()
    {
        return _idx == size_t.max;
    }

    DecompressedIntervals save() const { return this; } 
}



version(std_uni_bootstrap){}

else
{

//helper for static codepoint set tables
ptrdiff_t findUnicodeSet(C)(in C[] name)
{
    auto range = assumeSorted!((a,b) => propertyNameLess(a,b))(unicodeProps.map!"a.name");    
   
    size_t idx = range.lowerBound(name).length;

    if(idx < range.length && comparePropertyName(range[idx], name) == 0){
        return idx;
    }
    return -1;
}

//another one that loads it
bool loadUnicodeSet(Set, C)(in C[] name, ref Set dest)
{
    auto idx = findUnicodeSet(name);
    if(idx >= 0)
    {
        dest = Set(asSet(unicodeProps[idx].compressed));
        return true;
    }
    return false;
}

/**
    A single entry point to lookup unicode character sets by name or alias of 
    block, script or general category.

    Uses well defined standrd rules including fuzzy matching of names, e.g.
    White_Space, white-SpAce and whitespace are all considered equals 
    and yield the same set of white space characters.
*/
public struct unicode
{
    /**
        Performs the lookup with compile-time correctness checking.
        Example:
        ---            
        auto ascii = unicode.ASCII;
        assert(ascii['A']);
        assert(ascii['~']);
        assert(!ascii['\u00e0']);
        ---
    */
    
    static auto opDispatch(string name)()
    {
        static if(findSetName(name))
            return pickSet(name);
        else
            static assert(false, "No unicode set by name "~name~" is found.");
    }
    
    /**
        The same lookup but performed at run-time suitable for cases 
        where $(D name) is not known beforehand.
    */
    static auto opCall(C)(in C[] name)
        if(is(C : dchar))
    {
        return pickSet(name);       
    }

    //CTFE-only, not optimized
    private static bool findSetName(C)(in C[] name)
    {
        auto names = [ 
            "L", "Letters", 
            "LC", "Cased Letter", 
            "M", "Mark",
            "N", "Number",
            "P", "Punctuation",
            "S", "Symbol",
            "Z", "Separator"
            "Graphical",
            "any",
            "ascii"
        ];
        auto idx = names.countUntil!(x => comparePropertyName(x, name) == 0)();
        if(idx >= 0)
            return true;
        if(findUnicodeSet(name) >= 0)
            return true;
        return false;
    }

    static auto pickSet(Set=CodepointSet, C)(in C[] name)
    {        
        Set result;
        alias comparePropertyName ucmp;

        //unicode property
        //helper: direct access with a sanity check
        if(ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
        {
            result |= asSet(unicodeLu);
            result |= asSet(unicodeLl);
            result |= asSet(unicodeLt);
            result |= asSet(unicodeLo);
            result |= asSet(unicodeLm);
        }
        else if(ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter")==0)
        {
            result |= asSet(unicodeLl);
            result |= asSet(unicodeLu);
            result |= asSet(unicodeLt);//Title case
        }
        else if(ucmp(name, "M") == 0 || ucmp(name, "Mark") == 0)
        {
            result |= asSet(unicodeMn);
            result |= asSet(unicodeMc);
            result |= asSet(unicodeMe);
        }
        else if(ucmp(name, "N") == 0 || ucmp(name, "Number") == 0)
        {
            result |= asSet(unicodeNd);
            result |= asSet(unicodeNl);
            result |= asSet(unicodeNo);
        }
        else if(ucmp(name, "P") == 0 || ucmp(name, "Punctuation") == 0)
        {
            result |= asSet(unicodePc);
            result |= asSet(unicodePd);
            result |= asSet(unicodePs);
            result |= asSet(unicodePe);
            result |= asSet(unicodePi);
            result |= asSet(unicodePf);
            result |= asSet(unicodePo);
        }
        else if(ucmp(name, "S") == 0 || ucmp(name, "Symbol") == 0)
        {
            result |= asSet(unicodeSm);
            result |= asSet(unicodeSc);
            result |= asSet(unicodeSk);
            result |= asSet(unicodeSo);
        }
        else if(ucmp(name, "Z") == 0 || ucmp(name, "Separator") == 0)
        {
            result |= asSet(unicodeZs);
            result |= asSet(unicodeZl);
            result |= asSet(unicodeZp);
        }
        else if(ucmp(name, "C") == 0 || ucmp(name, "Other") == 0)
        {
            result |= asSet(unicodeCo);
            result |= asSet(unicodeLo);
            result |= asSet(unicodeNo);
            result |= asSet(unicodeSo);
            result |= asSet(unicodePo);
        }
        else if(ucmp(name, "graphical") == 0){
            result |= asSet(unicodeAlphabetic);

            result |= asSet(unicodeMn);
            result |= asSet(unicodeMc);
            result |= asSet(unicodeMe);

            result |= asSet(unicodeNd);
            result |= asSet(unicodeNl);
            result |= asSet(unicodeNo);

            result |= asSet(unicodePc);
            result |= asSet(unicodePd);
            result |= asSet(unicodePs);
            result |= asSet(unicodePe);
            result |= asSet(unicodePi);
            result |= asSet(unicodePf);
            result |= asSet(unicodePo);

            result |= asSet(unicodeZs);

            result |= asSet(unicodeSm);
            result |= asSet(unicodeSc);
            result |= asSet(unicodeSk);
            result |= asSet(unicodeSo);
        }
        else if(ucmp(name, "any") == 0)
            result = Set(0,0x110000);
        else if(ucmp(name, "ascii") == 0)
            result = Set(0,0x80);
        else
        {
            if(loadUnicodeSet(name, result))                    
                return result;
            else
                throw new Exception("no unicode set by name of " 
                    ~ to!string(name));
        }
        return result;
    }
    /// Disabled to prevent the mistake of creating instances of this pseudo-struct.
    //@disable ~this();
}

unittest
{
    auto ascii = unicode.ASCII;
    assert(ascii['A']);
    assert(ascii['~']);
    assert(!ascii['à']);
    auto latin = unicode.inlatin1Supplement;
    assert(latin['à']);
    assert(!latin['$']);
}

unittest
{
    assert(unicode("InHebrew") == asSet(unicodeInHebrew));
    assert(unicode("separator") == (asSet(unicodeZs) | asSet(unicodeZl) | asSet(unicodeZp)));
    assert(unicode("In-Kharoshthi") == asSet(unicodeInKharoshthi));
}

enum EMPTY_CASE_TRIE = ushort.max;//from what gen_uni uses internally

enum hangul_L = `
    case '\u1100': .. case '\u115E':
    case '\uA960': .. case '\uA97C':
    case '\u115F':
`;

enum hangul_LV = `
    case '\uAC00': case '\uAC1C': case '\uAC38': case '\uAC54': case '\uAC70': case '\uAC8C': case '\uACA8': case '\uACC4': case '\uACE0': case '\uACFC': case '\uAD18': case '\uAD34': case '\uAD50': case '\uAD6C': case '\uAD88': case '\uADA4': case '\uADC0': case '\uADDC': case '\uADF8': case '\uAE14': case '\uAE30': case '\uAE4C': case '\uAE68': case '\uAE84': case '\uAEA0': case '\uAEBC': case '\uAED8': case '\uAEF4': case '\uAF10': case '\uAF2C': case '\uAF48': case '\uAF64': case '\uAF80': case '\uAF9C': case '\uAFB8': case '\uAFD4': case '\uAFF0': case '\uB00C': case '\uB028': case '\uB044': case '\uB060': case '\uB07C': case '\uB098': case '\uB0B4': case '\uB0D0': case '\uB0EC': case '\uB108': case '\uB124': case '\uB140': case '\uB15C': case '\uB178': case '\uB194': case '\uB1B0': case '\uB1CC': case '\uB1E8': case '\uB204': case '\uB220': case '\uB23C': case '\uB258': case '\uB274': case '\uB290': case '\uB2AC': case '\uB2C8': case '\uB2E4': case '\uB300': case '\uB31C': case '\uB338': case '\uB354': case '\uB370': case '\uB38C': case '\uB3A8': case '\uB3C4': case '\uB3E0': case '\uB3FC': case '\uB418': case '\uB434': case '\uB450': case '\uB46C': case '\uB488': case '\uB4A4': case '\uB4C0': case '\uB4DC': case '\uB4F8': case '\uB514': case '\uB530': case '\uB54C': case '\uB568': case '\uB584': case '\uB5A0': case '\uB5BC': case '\uB5D8': case '\uB5F4': case '\uB610': case '\uB62C': case '\uB648': case '\uB664': case '\uB680': case '\uB69C': case '\uB6B8': case '\uB6D4': case '\uB6F0': case '\uB70C': case '\uB728': case '\uB744': case '\uB760': case '\uB77C': case '\uB798': case '\uB7B4': case '\uB7D0': case '\uB7EC': case '\uB808': case '\uB824': case '\uB840': case '\uB85C': case '\uB878': case '\uB894': case '\uB8B0': case '\uB8CC': case '\uB8E8': case '\uB904': case '\uB920': case '\uB93C': case '\uB958': case '\uB974': case '\uB990': case '\uB9AC': case '\uB9C8': case '\uB9E4': case '\uBA00': case '\uBA1C': case '\uBA38': case '\uBA54': case '\uBA70': case '\uBA8C': case '\uBAA8': case '\uBAC4': case '\uBAE0': case '\uBAFC': case '\uBB18': case '\uBB34': case '\uBB50': case '\uBB6C': case '\uBB88': case '\uBBA4': case '\uBBC0': case '\uBBDC': case '\uBBF8': case '\uBC14': case '\uBC30': case '\uBC4C': case '\uBC68': case '\uBC84': case '\uBCA0': case '\uBCBC': case '\uBCD8': case '\uBCF4': case '\uBD10': case '\uBD2C': case '\uBD48': case '\uBD64': case '\uBD80': case '\uBD9C': case '\uBDB8': case '\uBDD4': case '\uBDF0': case '\uBE0C': case '\uBE28': case '\uBE44': case '\uBE60': case '\uBE7C': case '\uBE98': case '\uBEB4': case '\uBED0': case '\uBEEC': case '\uBF08': case '\uBF24': case '\uBF40': case '\uBF5C': case '\uBF78': case '\uBF94': case '\uBFB0': case '\uBFCC': case '\uBFE8': case '\uC004': case '\uC020': case '\uC03C': case '\uC058': case '\uC074': case '\uC090': case '\uC0AC': case '\uC0C8': case '\uC0E4': case '\uC100': case '\uC11C': case '\uC138': case '\uC154': case '\uC170': case '\uC18C': case '\uC1A8': case '\uC1C4': case '\uC1E0': case '\uC1FC': case '\uC218': case '\uC234': case '\uC250': case '\uC26C': case '\uC288': case '\uC2A4': case '\uC2C0': case '\uC2DC': case '\uC2F8': case '\uC314': case '\uC330': case '\uC34C': case '\uC368': case '\uC384': case '\uC3A0': case '\uC3BC': case '\uC3D8': case '\uC3F4': case '\uC410': case '\uC42C': case '\uC448': case '\uC464': case '\uC480': case '\uC49C': case '\uC4B8': case '\uC4D4': case '\uC4F0': case '\uC50C': case '\uC528': case '\uC544': case '\uC560': case '\uC57C': case '\uC598': case '\uC5B4': case '\uC5D0': case '\uC5EC': case '\uC608': case '\uC624': case '\uC640': case '\uC65C': case '\uC678': case '\uC694': case '\uC6B0': case '\uC6CC': case '\uC6E8': case '\uC704': case '\uC720': case '\uC73C': case '\uC758': case '\uC774': case '\uC790': case '\uC7AC': case '\uC7C8': case '\uC7E4': case '\uC800': case '\uC81C': case '\uC838': case '\uC854': case '\uC870': case '\uC88C': case '\uC8A8': case '\uC8C4': case '\uC8E0': case '\uC8FC': case '\uC918': case '\uC934': case '\uC950': case '\uC96C': case '\uC988': case '\uC9A4': case '\uC9C0': case '\uC9DC': case '\uC9F8': case '\uCA14': case '\uCA30': case '\uCA4C': case '\uCA68': case '\uCA84': case '\uCAA0': case '\uCABC': case '\uCAD8': case '\uCAF4': case '\uCB10': case '\uCB2C': case '\uCB48': case '\uCB64': case '\uCB80': case '\uCB9C': case '\uCBB8': case '\uCBD4': case '\uCBF0': case '\uCC0C': case '\uCC28': case '\uCC44': case '\uCC60': case '\uCC7C': case '\uCC98': case '\uCCB4': case '\uCCD0': case '\uCCEC': case '\uCD08': case '\uCD24': case '\uCD40': case '\uCD5C': case '\uCD78': case '\uCD94': case '\uCDB0': case '\uCDCC': case '\uCDE8': case '\uCE04': case '\uCE20': case '\uCE3C': case '\uCE58': case '\uCE74': case '\uCE90': case '\uCEAC': case '\uCEC8': case '\uCEE4': case '\uCF00': case '\uCF1C': case '\uCF38': case '\uCF54': case '\uCF70': case '\uCF8C': case '\uCFA8': case '\uCFC4': case '\uCFE0': case '\uCFFC': case '\uD018': case '\uD034': case '\uD050': case '\uD06C': case '\uD088': case '\uD0A4': case '\uD0C0': case '\uD0DC': case '\uD0F8': case '\uD114': case '\uD130': case '\uD14C': case '\uD168': case '\uD184': case '\uD1A0': case '\uD1BC': case '\uD1D8': case '\uD1F4': case '\uD210': case '\uD22C': case '\uD248': case '\uD264': case '\uD280': case '\uD29C': case '\uD2B8': case '\uD2D4': case '\uD2F0': case '\uD30C': case '\uD328': case '\uD344': case '\uD360': case '\uD37C': case '\uD398': case '\uD3B4': case '\uD3D0': case '\uD3EC': case '\uD408': case '\uD424': case '\uD440': case '\uD45C': case '\uD478': case '\uD494': case '\uD4B0': case '\uD4CC': case '\uD4E8': case '\uD504': case '\uD520': case '\uD53C': case '\uD558': case '\uD574': case '\uD590': case '\uD5AC': case '\uD5C8': case '\uD5E4': case '\uD600': case '\uD61C': case '\uD638': case '\uD654': case '\uD670': case '\uD68C': case '\uD6A8': case '\uD6C4': case '\uD6E0': case '\uD6FC': case '\uD718': case '\uD734': case '\uD750': case '\uD76C': case '\uD788':
`;

enum hangul_LVT = `
    case '\uAC01':..case '\uAC1B':case '\uAC1D':..case '\uAC37':case '\uAC39':..case '\uAC53':case '\uAC55':..case '\uAC6F':case '\uAC71':..case '\uAC8B':case '\uAC8D':..case '\uACA7':case '\uACA9':..case '\uACC3':case '\uACC5':..case '\uACDF':case '\uACE1':..case '\uACFB':case '\uACFD':..case '\uAD17':case '\uAD19':..case '\uAD33':case '\uAD35':..case '\uAD4F':case '\uAD51':..case '\uAD6B':case '\uAD6D':..case '\uAD87':case '\uAD89':..case '\uADA3':case '\uADA5':..case '\uADBF':case '\uADC1':..case '\uADDB':case '\uADDD':..case '\uADF7':case '\uADF9':..case '\uAE13':case '\uAE15':..case '\uAE2F':case '\uAE31':..case '\uAE4B':case '\uAE4D':..case '\uAE67':case '\uAE69':..case '\uAE83':case '\uAE85':..case '\uAE9F':case '\uAEA1':..case '\uAEBB':case '\uAEBD':..case '\uAED7':case '\uAED9':..case '\uAEF3':case '\uAEF5':..case '\uAF0F':case '\uAF11':..case '\uAF2B':case '\uAF2D':..case '\uAF47':case '\uAF49':..case '\uAF63':case '\uAF65':..case '\uAF7F':case '\uAF81':..case '\uAF9B':case '\uAF9D':..case '\uAFB7':case '\uAFB9':..case '\uAFD3':case '\uAFD5':..case '\uAFEF':case '\uAFF1':..case '\uB00B':case '\uB00D':..case '\uB027':case '\uB029':..case '\uB043':case '\uB045':..case '\uB05F':case '\uB061':..case '\uB07B':case '\uB07D':..case '\uB097':case '\uB099':..case '\uB0B3':case '\uB0B5':..case '\uB0CF':case '\uB0D1':..case '\uB0EB':case '\uB0ED':..case '\uB107':case '\uB109':..case '\uB123':case '\uB125':..case '\uB13F':case '\uB141':..case '\uB15B':case '\uB15D':..case '\uB177':case '\uB179':..case '\uB193':case '\uB195':..case '\uB1AF':case '\uB1B1':..case '\uB1CB':case '\uB1CD':..case '\uB1E7':case '\uB1E9':..case '\uB203':case '\uB205':..case '\uB21F':case '\uB221':..case '\uB23B':case '\uB23D':..case '\uB257':case '\uB259':..case '\uB273':case '\uB275':..case '\uB28F':case '\uB291':..case '\uB2AB':case '\uB2AD':..case '\uB2C7':case '\uB2C9':..case '\uB2E3':case '\uB2E5':..case '\uB2FF':case '\uB301':..case '\uB31B':case '\uB31D':..case '\uB337':case '\uB339':..case '\uB353':case '\uB355':..case '\uB36F':case '\uB371':..case '\uB38B':case '\uB38D':..case '\uB3A7':case '\uB3A9':..case '\uB3C3':case '\uB3C5':..case '\uB3DF':case '\uB3E1':..case '\uB3FB':case '\uB3FD':..case '\uB417':case '\uB419':..case '\uB433':case '\uB435':..case '\uB44F':case '\uB451':..case '\uB46B':case '\uB46D':..case '\uB487':case '\uB489':..case '\uB4A3':case '\uB4A5':..case '\uB4BF':case '\uB4C1':..case '\uB4DB':case '\uB4DD':..case '\uB4F7':case '\uB4F9':..case '\uB513':case '\uB515':..case '\uB52F':case '\uB531':..case '\uB54B':case '\uB54D':..case '\uB567':case '\uB569':..case '\uB583':case '\uB585':..case '\uB59F':case '\uB5A1':..case '\uB5BB':case '\uB5BD':..case '\uB5D7':case '\uB5D9':..case '\uB5F3':case '\uB5F5':..case '\uB60F':case '\uB611':..case '\uB62B':case '\uB62D':..case '\uB647':case '\uB649':..case '\uB663':case '\uB665':..case '\uB67F':case '\uB681':..case '\uB69B':case '\uB69D':..case '\uB6B7':case '\uB6B9':..case '\uB6D3':case '\uB6D5':..case '\uB6EF':case '\uB6F1':..case '\uB70B':case '\uB70D':..case '\uB727':case '\uB729':..case '\uB743':case '\uB745':..case '\uB75F':case '\uB761':..case '\uB77B':case '\uB77D':..case '\uB797':case '\uB799':..case '\uB7B3':case '\uB7B5':..case '\uB7CF':case '\uB7D1':..case '\uB7EB':case '\uB7ED':..case '\uB807':case '\uB809':..case '\uB823':case '\uB825':..case '\uB83F':case '\uB841':..case '\uB85B':case '\uB85D':..case '\uB877':case '\uB879':..case '\uB893':case '\uB895':..case '\uB8AF':case '\uB8B1':..case '\uB8CB':case '\uB8CD':..case '\uB8E7':case '\uB8E9':..case '\uB903':case '\uB905':..case '\uB91F':case '\uB921':..case '\uB93B':case '\uB93D':..case '\uB957':case '\uB959':..case '\uB973':case '\uB975':..case '\uB98F':case '\uB991':..case '\uB9AB':case '\uB9AD':..case '\uB9C7':case '\uB9C9':..case '\uB9E3':case '\uB9E5':..case '\uB9FF':case '\uBA01':..case '\uBA1B':case '\uBA1D':..case '\uBA37':case '\uBA39':..case '\uBA53':case '\uBA55':..case '\uBA6F':case '\uBA71':..case '\uBA8B':case '\uBA8D':..case '\uBAA7':case '\uBAA9':..case '\uBAC3':case '\uBAC5':..case '\uBADF':case '\uBAE1':..case '\uBAFB':case '\uBAFD':..case '\uBB17':case '\uBB19':..case '\uBB33':case '\uBB35':..case '\uBB4F':case '\uBB51':..case '\uBB6B':case '\uBB6D':..case '\uBB87':case '\uBB89':..case '\uBBA3':case '\uBBA5':..case '\uBBBF':case '\uBBC1':..case '\uBBDB':case '\uBBDD':..case '\uBBF7':case '\uBBF9':..case '\uBC13':case '\uBC15':..case '\uBC2F':case '\uBC31':..case '\uBC4B':case '\uBC4D':..case '\uBC67':case '\uBC69':..case '\uBC83':case '\uBC85':..case '\uBC9F':case '\uBCA1':..case '\uBCBB':case '\uBCBD':..case '\uBCD7':case '\uBCD9':..case '\uBCF3':case '\uBCF5':..case '\uBD0F':case '\uBD11':..case '\uBD2B':case '\uBD2D':..case '\uBD47':case '\uBD49':..case '\uBD63':case '\uBD65':..case '\uBD7F':case '\uBD81':..case '\uBD9B':case '\uBD9D':..case '\uBDB7':case '\uBDB9':..case '\uBDD3':case '\uBDD5':..case '\uBDEF':case '\uBDF1':..case '\uBE0B':case '\uBE0D':..case '\uBE27':case '\uBE29':..case '\uBE43':case '\uBE45':..case '\uBE5F':case '\uBE61':..case '\uBE7B':case '\uBE7D':..case '\uBE97':case '\uBE99':..case '\uBEB3':case '\uBEB5':..case '\uBECF':case '\uBED1':..case '\uBEEB':case '\uBEED':..case '\uBF07':case '\uBF09':..case '\uBF23':case '\uBF25':..case '\uBF3F':case '\uBF41':..case '\uBF5B':case '\uBF5D':..case '\uBF77':case '\uBF79':..case '\uBF93':case '\uBF95':..case '\uBFAF':case '\uBFB1':..case '\uBFCB':case '\uBFCD':..case '\uBFE7':case '\uBFE9':..case '\uC003':case '\uC005':..case '\uC01F':case '\uC021':..case '\uC03B':case '\uC03D':..case '\uC057':case '\uC059':..case '\uC073':case '\uC075':..case '\uC08F':case '\uC091':..case '\uC0AB':case '\uC0AD':..case '\uC0C7':case '\uC0C9':..case '\uC0E3':case '\uC0E5':..case '\uC0FF':case '\uC101':..case '\uC11B':case '\uC11D':..case '\uC137':case '\uC139':..case '\uC153':case '\uC155':..case '\uC16F':case '\uC171':..case '\uC18B':case '\uC18D':..case '\uC1A7':case '\uC1A9':..case '\uC1C3':case '\uC1C5':..case '\uC1DF':case '\uC1E1':..case '\uC1FB':case '\uC1FD':..case '\uC217':case '\uC219':..case '\uC233':case '\uC235':..case '\uC24F':case '\uC251':..case '\uC26B':case '\uC26D':..case '\uC287':case '\uC289':..case '\uC2A3':case '\uC2A5':..case '\uC2BF':case '\uC2C1':..case '\uC2DB':case '\uC2DD':..case '\uC2F7':case '\uC2F9':..case '\uC313':case '\uC315':..case '\uC32F':case '\uC331':..case '\uC34B':case '\uC34D':..case '\uC367':case '\uC369':..case '\uC383':case '\uC385':..case '\uC39F':case '\uC3A1':..case '\uC3BB':case '\uC3BD':..case '\uC3D7':case '\uC3D9':..case '\uC3F3':case '\uC3F5':..case '\uC40F':case '\uC411':..case '\uC42B':case '\uC42D':..case '\uC447':case '\uC449':..case '\uC463':case '\uC465':..case '\uC47F':case '\uC481':..case '\uC49B':case '\uC49D':..case '\uC4B7':case '\uC4B9':..case '\uC4D3':case '\uC4D5':..case '\uC4EF':case '\uC4F1':..case '\uC50B':case '\uC50D':..case '\uC527':case '\uC529':..case '\uC543':case '\uC545':..case '\uC55F':case '\uC561':..case '\uC57B':case '\uC57D':..case '\uC597':case '\uC599':..case '\uC5B3':case '\uC5B5':..case '\uC5CF':case '\uC5D1':..case '\uC5EB':case '\uC5ED':..case '\uC607':case '\uC609':..case '\uC623':case '\uC625':..case '\uC63F':case '\uC641':..case '\uC65B':case '\uC65D':..case '\uC677':case '\uC679':..case '\uC693':case '\uC695':..case '\uC6AF':case '\uC6B1':..case '\uC6CB':case '\uC6CD':..case '\uC6E7':case '\uC6E9':..case '\uC703':case '\uC705':..case '\uC71F':case '\uC721':..case '\uC73B':case '\uC73D':..case '\uC757':case '\uC759':..case '\uC773':case '\uC775':..case '\uC78F':case '\uC791':..case '\uC7AB':case '\uC7AD':..case '\uC7C7':case '\uC7C9':..case '\uC7E3':case '\uC7E5':..case '\uC7FF':case '\uC801':..case '\uC81B':case '\uC81D':..case '\uC837':case '\uC839':..case '\uC853':case '\uC855':..case '\uC86F':case '\uC871':..case '\uC88B':case '\uC88D':..case '\uC8A7':case '\uC8A9':..case '\uC8C3':case '\uC8C5':..case '\uC8DF':case '\uC8E1':..case '\uC8FB':case '\uC8FD':..case '\uC917':case '\uC919':..case '\uC933':case '\uC935':..case '\uC94F':case '\uC951':..case '\uC96B':case '\uC96D':..case '\uC987':case '\uC989':..case '\uC9A3':case '\uC9A5':..case '\uC9BF':case '\uC9C1':..case '\uC9DB':case '\uC9DD':..case '\uC9F7':case '\uC9F9':..case '\uCA13':case '\uCA15':..case '\uCA2F':case '\uCA31':..case '\uCA4B':case '\uCA4D':..case '\uCA67':case '\uCA69':..case '\uCA83':case '\uCA85':..case '\uCA9F':case '\uCAA1':..case '\uCABB':case '\uCABD':..case '\uCAD7':case '\uCAD9':..case '\uCAF3':case '\uCAF5':..case '\uCB0F':case '\uCB11':..case '\uCB2B':case '\uCB2D':..case '\uCB47':case '\uCB49':..case '\uCB63':case '\uCB65':..case '\uCB7F':case '\uCB81':..case '\uCB9B':case '\uCB9D':..case '\uCBB7':case '\uCBB9':..case '\uCBD3':case '\uCBD5':..case '\uCBEF':case '\uCBF1':..case '\uCC0B':case '\uCC0D':..case '\uCC27':case '\uCC29':..case '\uCC43':case '\uCC45':..case '\uCC5F':case '\uCC61':..case '\uCC7B':case '\uCC7D':..case '\uCC97':case '\uCC99':..case '\uCCB3':case '\uCCB5':..case '\uCCCF':case '\uCCD1':..case '\uCCEB':case '\uCCED':..case '\uCD07':case '\uCD09':..case '\uCD23':case '\uCD25':..case '\uCD3F':case '\uCD41':..case '\uCD5B':case '\uCD5D':..case '\uCD77':case '\uCD79':..case '\uCD93':case '\uCD95':..case '\uCDAF':case '\uCDB1':..case '\uCDCB':case '\uCDCD':..case '\uCDE7':case '\uCDE9':..case '\uCE03':case '\uCE05':..case '\uCE1F':case '\uCE21':..case '\uCE3B':case '\uCE3D':..case '\uCE57':case '\uCE59':..case '\uCE73':case '\uCE75':..case '\uCE8F':case '\uCE91':..case '\uCEAB':case '\uCEAD':..case '\uCEC7':case '\uCEC9':..case '\uCEE3':case '\uCEE5':..case '\uCEFF':case '\uCF01':..case '\uCF1B':case '\uCF1D':..case '\uCF37':case '\uCF39':..case '\uCF53':case '\uCF55':..case '\uCF6F':case '\uCF71':..case '\uCF8B':case '\uCF8D':..case '\uCFA7':case '\uCFA9':..case '\uCFC3':case '\uCFC5':..case '\uCFDF':case '\uCFE1':..case '\uCFFB':case '\uCFFD':..case '\uD017':case '\uD019':..case '\uD033':case '\uD035':..case '\uD04F':case '\uD051':..case '\uD06B':case '\uD06D':..case '\uD087':case '\uD089':..case '\uD0A3':case '\uD0A5':..case '\uD0BF':case '\uD0C1':..case '\uD0DB':case '\uD0DD':..case '\uD0F7':case '\uD0F9':..case '\uD113':case '\uD115':..case '\uD12F':case '\uD131':..case '\uD14B':case '\uD14D':..case '\uD167':case '\uD169':..case '\uD183':case '\uD185':..case '\uD19F':case '\uD1A1':..case '\uD1BB':case '\uD1BD':..case '\uD1D7':case '\uD1D9':..case '\uD1F3':case '\uD1F5':..case '\uD20F':case '\uD211':..case '\uD22B':case '\uD22D':..case '\uD247':case '\uD249':..case '\uD263':case '\uD265':..case '\uD27F':case '\uD281':..case '\uD29B':case '\uD29D':..case '\uD2B7':case '\uD2B9':..case '\uD2D3':case '\uD2D5':..case '\uD2EF':case '\uD2F1':..case '\uD30B':case '\uD30D':..case '\uD327':case '\uD329':..case '\uD343':case '\uD345':..case '\uD35F':case '\uD361':..case '\uD37B':case '\uD37D':..case '\uD397':case '\uD399':..case '\uD3B3':case '\uD3B5':..case '\uD3CF':case '\uD3D1':..case '\uD3EB':case '\uD3ED':..case '\uD407':case '\uD409':..case '\uD423':case '\uD425':..case '\uD43F':case '\uD441':..case '\uD45B':case '\uD45D':..case '\uD477':case '\uD479':..case '\uD493':case '\uD495':..case '\uD4AF':case '\uD4B1':..case '\uD4CB':case '\uD4CD':..case '\uD4E7':case '\uD4E9':..case '\uD503':case '\uD505':..case '\uD51F':case '\uD521':..case '\uD53B':case '\uD53D':..case '\uD557':case '\uD559':..case '\uD573':case '\uD575':..case '\uD58F':case '\uD591':..case '\uD5AB':case '\uD5AD':..case '\uD5C7':case '\uD5C9':..case '\uD5E3':case '\uD5E5':..case '\uD5FF':case '\uD601':..case '\uD61B':case '\uD61D':..case '\uD637':case '\uD639':..case '\uD653':case '\uD655':..case '\uD66F':case '\uD671':..case '\uD68B':case '\uD68D':..case '\uD6A7':case '\uD6A9':..case '\uD6C3':case '\uD6C5':..case '\uD6DF':case '\uD6E1':..case '\uD6FB':case '\uD6FD':..case '\uD717':case '\uD719':..case '\uD733':case '\uD735':..case '\uD74F':case '\uD751':..case '\uD76B':case '\uD76D':..case '\uD787':case '\uD789':..case '\uD7A3':
`;

enum hangul_V = `
    case '\u1160': .. case '\u11A7':
    case '\uD7B0': .. case '\uD7C6':
`;

enum hangul_T = `
    case '\u11A8':..case '\u11FF': case '\uD7CB':..case '\uD7FB':
`;

//control - '\r'
enum controlSwitch = `
    case '\u0000':..case '\u0008':case '\u000E':..case '\u001F':case '\u007F':..case '\u0084':case '\u0086':..case '\u009F': case '\u0009':..case '\u000C': case '\u0085':
`;
//TODO: redo hangul stuff algorithmically in case of Graphemes too, kill unrolled switches

private static bool isRegionalIndicator(dchar ch)
{
    return ch >= '\U0001F1E6' && ch <= '\U0001F1FF';
}

template genericDecodeGrapheme(bool getValue)
{
    static if(getValue)
        alias Grapheme Value;
    else
        alias void Value;

    Value genericDecodeGrapheme(Input)(ref Input range)
    {
        enum GraphemeState {
            Start,
            CR,
            RI, 
            L,
            V,
            LVT
        };
        static if(getValue)
            Grapheme grapheme;
        auto state = GraphemeState.Start;
        enum eat = q{
            static if(getValue)
                grapheme ~= ch;
            range.popFront();
        };

        dchar ch;
        assert(!range.empty, "Attempting to decode grapheme from an empty " ~ Input.stringof);
        while(!range.empty)
        {
            ch = range.front;
            final switch(state) with(GraphemeState)
            {
            case Start:
                mixin(eat);
                if(ch == '\r')
                    state = CR;
                else if(isRegionalIndicator(ch))
                    state = RI;             
                else if(hangL[ch])
                    state = L;
                else if(hangLV[ch] || hangV[ch])
                    state = V;
                else if(hangLVT[ch])
                    state = LVT;
                else if(hangT[ch])
                    state = LVT;
                else
                {
                    switch(ch)
                    {
                    mixin(controlSwitch);
                        goto L_End;
                    default:
                        goto L_End_Extend;
                    }
                }
            break;
            case CR:
                if(ch == '\n')
                    mixin(eat);
                goto L_End_Extend;
            break;
            case RI:
                if(isRegionalIndicator(ch))
                    mixin(eat);
                else 
                    goto L_End_Extend;                
            break;
            case L:
                if(hangL[ch])
                    mixin(eat);
                else if(hangV[ch] || hangLV[ch])
                {
                    state = V;
                    mixin(eat);
                }
                else if(hangLVT[ch])
                {
                    state = LVT;
                    mixin(eat);
                }
                else
                    goto L_End_Extend;
            break;
            case V:
                if(hangV[ch])
                    mixin(eat);
                else if(hangT[ch])
                {
                    state = LVT;
                    mixin(eat);
                }
                else 
                    goto L_End_Extend;
            break;
            case LVT:
                if(hangT[ch])
                {
                    mixin(eat);
                }
                else
                    goto L_End_Extend;
            break;
            }
        }
    L_End_Extend:
        
        while(!range.empty)
        {
            ch = range.front;
            //extend & spacing marks
            if(!graphemeExtend[ch] && !spacingMark[ch])
                break;
            mixin(eat);
        }
    L_End:
        static if(getValue)
            return grapheme;
    }

}

unittest
{
    assert(graphemeStride("  ", 1) == 1);
    //for now tested separately see test_grapheme.d
}

@trusted:
public: //Public API continues

/++
    Returns the length of grapheme cluster starting at $(D index).
    Both resulting length and $(D index) are measured in codeunits.
+/
size_t graphemeStride(C)(in C[] input, size_t index)
    if(is(C : dchar))
{
    auto src = input[index..$];
    auto n = src.length;
    genericDecodeGrapheme!(false)(src);
    return n - src.length;
}

/++
    Read and return one full grapheme cluster from an input range of dchar $(D inp). 
    Note: this function modifies $(D inp) and thus $(D inp) 
    must be an L-value.
+/
Grapheme decodeGrapheme(Input)(ref Input inp)
    if(isInputRange!Input && is(Unqual!(ElementType!Input) == dchar))
{
    return genericDecodeGrapheme!true(inp);
}

unittest
{
    Grapheme gr;
    string s = " \u0020\u0308 ";
    gr = decodeGrapheme(s);
    assert(gr.length == 1 && gr[0] == ' ');
    gr = decodeGrapheme(s);
    assert(gr.length == 2 && equal(gr[0..2], " \u0308"));
}

/++
    A structure designed to effectively pack codepoints of a grapheme cluster. 
    $(D Grapheme) has value smemantics so 2 copies of $(D Grapheme) 
    always refer to distinct objects. In most actual scenarios (D Grapheme) 
    fits on stack and avoids memory allocation overhead for all but very long clusters.
+/
struct Grapheme
{
public:
    this(C)(in C[] seq...)
        if(is(C : dchar))
    {
        this ~= seq;
    }

    /// Get codepoint at given index in this cluster.
    dchar opIndex(size_t index) const
    {
        return read24(isBig ? ptr_ : small_.ptr, index);
    }

    /++
        Write codepoint at given index of this cluster.

        Warning: use of this facility may invalidate grapheme cluster, see also $(D valid).
     +/
    void opIndexAssign(dchar ch, size_t index)
    {
        write24(isBig ? ptr_ : small_.ptr, ch, index);
    }

    /++
        Random-access range over Grapheme's codepoints.

        Warning: Invalidates when this Grapheme leaves the scope.
    +/
    auto opSlice(size_t a, size_t b)
    {
        return sliceOverIndexed(a, b, &this);
    }

    ///ditto
    auto opSlice()
    {
        return sliceOverIndexed(0, length, &this);
    }

    ///Grapheme cluster length in codepoints.
    @property size_t length() const 
    { 
        return isBig ? len_ : slen_ & 0x7F; 
    }

    /// Append $(D ch) to this grapheme.
    ref opOpAssign(string op)(dchar ch)
    {
        static if(op == "~")
        {
            if(!isBig)
            {
                if(slen_ + 1 > small_cap)
                    convertToBig();// & fallthrough to "big" branch
                else
                {
                    write24(small_.ptr, ch, smallLength); 
                    slen_++;
                    return this;
                }
            }

            assert(isBig);
            if(len_ + 1 > cap_)
            {
                cap_ += grow;                
                ptr_ = cast(ubyte*)enforce(realloc(ptr_, 3*(cap_+1)));
            }
            write24(ptr_, ch, len_++);
            return this;
        }
        else
            static assert(false, "No operation "~op~" defined for Grapheme");
    }

    ///Append all of codepoints from input range $(D inp) to this Grapheme.
    ref opOpAssign(string op, Input)(Input inp)
        if(isInputRange!Input)
    {
        static if(op == "~")
        {
            foreach(dchar ch; inp)
                this ~= ch;
            return this;
        }
        else
            static assert(false, "No operation "~op~" defined for Grapheme");
    }

    /++
        True if this object contains valid extended grapheme cluster.
        Decoding primitives of this module always return valid $(D Grapheme).

        Appending to and direct manipulation of grapheme's codepoints may 
        render it no longer valid. Certain applications may chose to use 
        Grapheme as a "small string" of codepoints and ignore this property
        entierly.
    +/
    @property bool valid() /*const*/
    {
        auto r = this[];
        genericDecodeGrapheme!false(r);
        return r.length == 0;
    }

    this(this)
    {
        if(isBig)
        {//dup it
            auto raw_cap = 3*(cap_+1);
            auto p = cast(ubyte*)enforce(malloc(raw_cap));
            p[0..raw_cap] = ptr_[0..raw_cap];
            ptr_ = p;
        }
    }

    ~this()
    {
        if(isBig)
            free(ptr_);
    }


private:
    enum small_bytes = ((ubyte*).sizeof+3*size_t.sizeof-1);
    //out of the blue grow rate, needs testing 
    //(though graphemes are typically small < 9)
    enum grow = 20;
    enum small_cap = small_bytes/3;
    enum small_flag = 0x80, small_mask = 0x7F;
    //16 bytes in 32bits, should be enough for the majority of cases
    union
    {
        struct
        {
            ubyte* ptr_; 
            size_t cap_;           
            size_t len_;
            size_t padding_;
        }
        struct
        {
            ubyte[small_bytes] small_;
            ubyte slen_;
        }
    }

    void convertToBig()
    {
        size_t k = smallLength;
        ubyte* p = cast(ubyte*)enforce(malloc(3*(grow+1)));
        for(int i=0; i<k; i++)
            write24(p, read24(small_.ptr, i), i);
        //now we can overwrite small array data
        ptr_ = p;
        len_ = slen_;
        assert(grow > len_);
        cap_ = grow;
        setBig();
    }

    void setBig(){ slen_ |= small_flag; }
    void setSmall(){ slen_ &= ~small_flag; }

    @property size_t smallLength(){ return slen_ & small_mask; }
    @property ubyte isBig() const { return slen_ & small_flag; }
}

unittest
{ //not valid clusters (but it just a test)
    auto g  = Grapheme('a', 'b', 'c', 'd', 'e');
    assert(g[0] == 'a');
    assert(g[1] == 'b');
    assert(g[2] == 'c');
    assert(g[3] == 'd');
    assert(g[4] == 'e');
    g[3] = 'Й';
    assert(g[2] == 'c');
    assert(g[3] == 'Й', text(g[3], " vs ", 'Й'));
    assert(g[4] == 'e');
    assert(!g.valid);

    g ~= 'ц';
    g ~= '~';
    assert(g[0] == 'a');
    assert(g[1] == 'b');
    assert(g[2] == 'c');
    assert(g[3] == 'Й');
    assert(g[4] == 'e');
    assert(g[5] == 'ц');
    assert(g[6] == '~');
    assert(!g.valid);

    Grapheme copy = g;
    copy[0] = 'X';
    copy[1] = '-';
    assert(g[0] == 'a' && copy[0] == 'X');
    assert(g[1] == 'b' && copy[1] == '-');
    assert(equal(g[2..g.length], copy[2..copy.length]));
    copy = Grapheme("АБВГДЕЁЖЗИКЛМ");
    assert(equal(copy[0..8], "АБВГДЕЁЖ"), text(copy[0..8]));
    copy ~= "xyz";
    assert(equal(copy[13..15], "xy"), text(copy[13..15]));
    assert(!copy.valid);
}

/++
    Does basic case-insensitive comparison of strings $(D str1) and $(D str2).

    Warning: this function only handles 1:1 codepoint mapping
    and thus is not sufficent for certain alphabets 
    like German, Greek and few others.
+/
int sicmp(C1, C2)(in C1[] str1, in C2[] str2)
{
    alias simpleCaseTable stab;
    size_t ridx=0;
    foreach(dchar lhs; str1)
    {
        if(ridx == str2.length)
            return 1;
        dchar rhs = std.utf.decode(str2, ridx);
        int diff = lhs - rhs;
        if(!diff)
            continue;
        size_t idx = simpleCaseTrie[lhs];
        size_t idx2 = simpleCaseTrie[rhs];        
        //simpleCaseTrie is packed index table
        if(idx != EMPTY_CASE_TRIE)
        {
            if(idx2 != EMPTY_CASE_TRIE)
            {//both cased chars
                //adjust idx --> start of bucket
                idx = idx - stab[idx].n;
                idx2 = idx2 - stab[idx2].n;
                if(idx == idx2)//one bucket, equivalent chars
                    continue;
                else//  not the same bucket
                    diff = stab[idx].ch - stab[idx2].ch;
            }
            else
                diff = stab[idx - stab[idx].n].ch - rhs;
        }
        else if(idx2 != EMPTY_CASE_TRIE)
        {
            diff = lhs - stab[idx2 - stab[idx2].n].ch;
        }
        //one of chars is not cased at all
        return diff;
    }
    return ridx == str2.length ? 0 : -1;
}

private int fullCasedCmp(C)(ref dchar lhs, ref dchar rhs, ref inout(C)[] str)
{
    alias fullCaseTable ftab;
    size_t idx = fullCaseTrie[lhs];
    //fullCaseTrie is packed index table
    if(idx != EMPTY_CASE_TRIE)
    {
        size_t start = idx - ftab[idx].n;
        size_t end = ftab[idx].size + start;
        assert(ftab[start].entry_len == 1);
        lhs = ftab[start].ch;//to use when diff is required
        for(idx=start; idx<end; idx++)
        {
            if(ftab[idx].entry_len == 1)
            {
                if(ftab[idx].ch == rhs)
                    return 0;
            }
            else 
            {//OK it's a long chunk, like 'ss' for German
                dstring seq = ftab[idx].seq;
                if(rhs == seq[0] 
                    && str.skipOver(seq[1..$]))
                {
                    return 0;
                }
            }
        }
    }
    return 1;
}

/++
    Does case insensitive comparison of $(D str1) and $(D str2).
    Follows rules of full casefolding mapping. 
    This includes matching as equal german ß with "ss" and 
    other 1:M codepoints relations unlike $(D sicmp).
+/
int icmp(C1, C2)(inout(C1)[] str1, inout(C2)[] str2)
{
    
    for(;;)
    {
        if(str1.empty)
            return str2.empty ? 0 : -1;
        dchar lhs = str1.front;
        if(str2.empty)
            return 1;
        dchar rhs = str2.front;
        str1.popFront();
        str2.popFront();

        int diff = lhs - rhs;
        if(!diff)
            continue;
        if(fullCasedCmp(lhs, rhs, str2) == 0)
            continue;
        else if(fullCasedCmp(rhs, lhs, str1) == 0)
            continue;
        diff = lhs - rhs;//lhs & rhs are remapped to the start of bucket
        return diff;
    }
}

unittest
{
    foreach(cfunc; TypeTuple!(icmp, sicmp)) 
    {
        foreach(S1; TypeTuple!(string, wstring, dstring))
        foreach(S2; TypeTuple!(string, wstring, dstring))
        {
            assert(cfunc("".to!S1, "".to!S2) == 0);
            assert(cfunc("A".to!S1, "".to!S2) > 0);
            assert(cfunc("".to!S1, "0".to!S2) < 0);
            assert(cfunc("abc".to!S1, "abc".to!S2) == 0);
            assert(cfunc("abcd".to!S1, "abc".to!S2) > 0);
            assert(cfunc("abc".to!S1, "abcd".to!S2) < 0);
            assert(cfunc("Abc".to!S1, "aBc".to!S2) == 0);
            assert(cfunc("авГуст".to!S1, "АВгУСТ".to!S2) == 0);
        }
        //check that the order is propely agonstic to the case
        auto strs = [ "Apple", "ORANGE",  "orAcle", "amp", "banana"];
        sort!((a,b) => cfunc(a,b) < 0)(strs);    
        assert(strs == ["amp", "Apple",  "banana", "orAcle", "ORANGE"]);        
    }
    assert(icmp("ßa", "ssa") == 0);
}

/++
    Returns the canonical combining class of $(D ch).
+/
ubyte combiningClass(dchar ch)
{
    return combiningClassTrie[ch];
}

///Unicode decomposition types.
enum { Canonical="Canonical", Compatibility="Compatibility"};

/++
    Try to canonically compose 2 codepoints.
    Returns the composed codepoint if they do compose and D.init otherwise.

    The assumption is that $(D first) comes before $(D second) in the original text, 
    usually meaning that the first is a starter.

    Note: Hangul syllables are not covered by this function. 
    See $(D composeJamo) below.
+/
public dchar compose(dchar first, dchar second)
{
    size_t packed = compositionJumpTrie[first];
    if(packed == ushort.max)
        return dchar.init;
    //unpack offset and length
    size_t idx = packed & composeIdxMask, cnt = packed >> composeCntShift;
    //TODO: optimize this micro binary search (no more then 4-5 steps)
    auto r = compositionTable[idx..idx+cnt].map!"a.rhs".assumeSorted;
    auto target = r.lowerBound(second).length;
    if(target == cnt)
        return dchar.init;
    auto entry = compositionTable[idx+target];
    if(entry.rhs != second)
        return dchar.init;
    return entry.composed;
}

/++
    Try to compose hangul syllable out of a leading consonant ($(D lead)), 
    a $(D vowel) and optional $(D trailing) consonant jamos.

    On success returns the composed LV or LVT hangul syllable.

    If any of $(D lead) and $(D vowel) are not a valid hangul jamo 
    of the respective character class returns dchar.init.
+/
public dchar composeJamo(dchar lead, dchar vowel, dchar trailing=dchar.init)
{
    if(!isJamoL(lead))
        return dchar.init;
    int indexL = lead - jamoLBase;    
    if(!isJamoV(vowel)) 
        return dchar.init;
    int indexV = vowel - jamoVBase;    
    int indexLV = indexL * jamoNCount + indexV * jamoTCount;
    dchar syllable = jamoSBase + indexLV;
    return isJamoT(trailing) ? syllable + (trailing - jamoTBase) : syllable;
}

/++
    Returns a full Canonical (by default) or Compatibility decomposition of codepoint 
    $(D ch). If no decomposition is available returns Grapheme with the $(D ch) itself.
+/
public Grapheme decompose(string decompType=Canonical)(dchar ch)
{
    static if(decompType == Canonical)
    {
        alias decompCanonTable table;
        alias canonMapping mapping;
    }
    else static if(decompType == Compatibility)
    {
        alias decompCompatTable table;
        alias compatMapping mapping;
    }
    ushort idx = mapping[ch];
    if(!idx) //not found, check hangul arithmetic decomposition
        return hangulDecompose(ch); 
    return Grapheme(table[idx]);
}

//----------------------------------------------------------------------------
//Hangul specific composition/decomposition
enum jamoSBase = 0xAC00;
enum jamoLBase = 0x1100;
enum jamoVBase = 0x1161;
enum jamoTBase = 0x11A7;
enum jamoLCount = 19, jamoVCount = 21, jamoTCount = 28;
enum jamoNCount = jamoVCount * jamoTCount;
enum jamoSCount = jamoLCount * jamoNCount;

//Tests if $(D ch) is a Hangul leading consonant jamo.
bool isJamoL(dchar ch)
{
    //first cmp rejects ~ 1M codepoints above leading jamo range
    return ch < jamoLBase+jamoLCount && ch >= jamoLBase;
}

//Tests if $(D ch) is a Hangul vowel jamo.
bool isJamoT(dchar ch)
{
    //first cmp rejects ~ 1M codepoints above trailing jamo range
    //Note: ch == jamoTBase doesn't indicate trailing jamo (TIndex must be > 0)
    return ch < jamoTBase+jamoTCount && ch > jamoTBase;
}

//Tests if $(D ch) is a Hangul trailnig consonant jamo.
bool isJamoV(dchar ch)
{
    //first cmp rejects ~ 1M codepoints above vowel range
    return  ch < jamoVBase+jamoVCount && ch >= jamoVBase;
}

int hangulSyllableIndex(dchar ch)
{
    int idxS = cast(int)ch - jamoSBase;
    return idxS >= 0 && idxS < jamoSCount ? idxS : -1;
}

//
Grapheme hangulDecompose(dchar ch)
{
    int idxS = cast(int)ch - jamoSBase;
    if(idxS < 0 || idxS >= jamoSCount) return Grapheme(ch);
    int idxL = idxS / jamoNCount;   
    int idxV = (idxS % jamoNCount) / jamoTCount;
    int idxT = idxS % jamoTCount;

    int partL = jamoLBase + idxL;
    int partV = jamoVBase + idxV;
    if(idxT > 0) //there is a trailling consonant (T); <L,V,T> decomposition
        return Grapheme(partL, partV, jamoTBase + idxT);
    else // <L, V> decomposition
        return Grapheme(partL, partV);
}

//internal helper: compose hangul syllables leaving dchar.init in holes
void hangulRecompose(dchar[] seq)
{
    for(size_t idx = 0; idx + 1 < seq.length; )
    {
        if(isJamoL(seq[idx]) && isJamoV(seq[idx+1]))
        {
            int indexL = seq[idx] - jamoLBase;
            int indexV = seq[idx+1] - jamoVBase;
            int indexLV = indexL * jamoNCount + indexV * jamoTCount;
            if(idx + 2 < seq.length && isJamoT(seq[idx+2]))
            {
                seq[idx] = jamoSBase + indexLV + seq[idx+2] - jamoTBase;
                seq[idx+1] = dchar.init;
                seq[idx+2] = dchar.init;
                idx += 3;
            }
            else
            {
                seq[idx] = jamoSBase + indexLV;
                seq[idx+1] = dchar.init;
                idx += 2;
            }
        } 
        else
            idx++;
    }
}

//----------------------------------------------------------------------------

public:

unittest{
    void testDecomp(string T)(dchar ch, string r)
    {
        assert(equal(decompose!T(ch)[], r), text(decompose(ch)[], " vs ", r));
    }
    testDecomp!Canonical('\u1FF4', "\u03C9\u0301\u0345");
    testDecomp!Canonical('\uF907', "\u9F9C");
    testDecomp!Compatibility('\u33FF', "\u0067\u0061\u006C");
    testDecomp!Compatibility('\uA7F9', "\u0153");
}

enum { NFC="NFC", NFD="NFD", NFKC="NFKC", NFKD="NFKD" };

private enum QC{ No = -1, Maybe = 0, Yes = 1};

/++
    Returns input string normalized to the chosen form. Form C is used by default. 
    In case where the string in question is already normalized, 
    it is returned unmodified and no memory allocation happens.
+/
inout(C)[] normalize(string norm=NFC, C)(inout(C)[] input)
{ 
    auto anchors = splitNormalized!norm(input);
    if(anchors[0] == input.length && anchors[1] == input.length)
        return input;
    dchar[] decomposed;
    decomposed.reserve(31);
    ubyte[] ccc;
    ccc.reserve(31);
    auto app = appender!(C[])();
    do
    {
        app.put(input[0..anchors[0]]);
        foreach(dchar ch; input[anchors[0]..anchors[1]])
            static if(norm == NFD || norm == NFC)
            {
                foreach(dchar c; decompose!Canonical(ch)[])
                    decomposed ~= c;
            }
            else //NFKD & NFKC
            {
                foreach(dchar c; decompose!Compatibility(ch)[])
                    decomposed ~= c;
            }
        ccc.length = decomposed.length;
        size_t firstNonStable = 0;
        ubyte lastClazz = 0;
        
        foreach(idx, dchar ch; decomposed)
        {
            auto clazz = combiningClassTrie[ch];
            ccc[idx] = clazz;
            if(clazz == 0 && lastClazz != 0)
            {
                // found a stable codepoint after unstable ones 
                sort!("a[0] < b[0]", SwapStrategy.stable)
                    (zip(ccc[firstNonStable..idx], decomposed[firstNonStable..idx]));
                firstNonStable = decomposed.length;
            }
            else if(clazz != 0 && lastClazz == 0)
            {
                // found first unstable codepoint after stable ones 
                firstNonStable = idx;
            }
            lastClazz = clazz;            
        }           
        sort!("a[0] < b[0]", SwapStrategy.stable)
            (zip(ccc[firstNonStable..$], decomposed[firstNonStable..$]));
        static if(norm == NFC || norm == NFKC)
        {
            size_t idx = 0;
            auto first = countUntil(ccc, 0);
            if(first >= 0) // no starters?? no recomposition
            {                
                for(;;)
                {                    
                    auto second = recompose(first, decomposed, ccc);
                    if(second == decomposed.length)
                        break;
                    first = second;
                }
                //2nd pass for hangul syllables
                hangulRecompose(decomposed);
            }
        }
        static if(norm == NFD || norm == NFKD)
            app.put(decomposed);
        else
        {
            auto clean = remove!("a == dchar.init", SwapStrategy.stable)(decomposed);
            app.put(decomposed[0 .. clean.length]);
        }
        //reset variables
        decomposed.length = 0;
        decomposed.assumeSafeAppend();
        ccc.length = 0;
        ccc.assumeSafeAppend();
        input = input[anchors[1]..$];
        //and move on
        anchors = splitNormalized!norm(input);       
    }while(anchors[0] != input.length);
    app.put(input[0..anchors[0]]);
    return cast(inout(C)[])app.data;
}

unittest
{
    assert(normalize!NFD("abc\uF904def") == "abc\u6ED1def", text(normalize!NFD("abc\uF904def")));
    assert(normalize!NFKD("2¹⁰") == "210", normalize!NFKD("2¹⁰"));
    assert(normalize!NFD("Äffin") == "A\u0308ffin");
}

//canonically recompose given slice of codepoints, works in-place and mutates data
private size_t recompose(size_t start, dchar[] input, ubyte[] ccc)
{
    assert(input.length == ccc.length);
    int accumCC = -1;//so that it's out of 0..255 range
    bool foundSolidStarter = false;
    //writefln("recomposing %( %04x %)", input);
    //first one is always a starter thus we start at i == 1
    size_t i = start+1;
    for(; ; )
    {
        if(i == input.length)
            break;
        int curCC = ccc[i];
        // In any character sequence beginning with a starter S
        // a character C is blocked from S if and only if there 
        // is some character B between S and C, and either B 
        // is a starter or it has the same or higher combining class as C.
        //------------------------
        // Applying to our case:
        // S is input[0]
        // accumCC is the maximum CCC of characters between C and S,
        //     as ccc are sorted
        // C is input[i]

        if(curCC > accumCC)
        {
            dchar comp = compose(input[start], input[i]);
            if(comp != dchar.init)
            {                
                input[start] = comp;
                input[i] = dchar.init;//put a sentinel
                //current was merged so its CCC shouldn't affect 
                //composing with the next one 
            }
            else {
                //if it was a starter then accumCC is now 0, end of loop
                accumCC = curCC;
                if(accumCC == 0)
                    break;
            }
        }
        else{
            //ditto here
            accumCC = curCC;
            if(accumCC == 0)
                break;
        }
        i++;        
    }
    return i;
}

//returns tuple of 2 indexes that delimit:
//normalized text, piece that needs normalization and 
//the rest of input starting with stable codepoint
private auto splitNormalized(string norm, C)(const(C)[] input)
{
    auto result = input;
    ubyte lastCC = 0;

    foreach(idx, dchar ch; input)
    {
        static if(norm == NFC)
            if(ch < 0x0300)
            {
                lastCC = 0;
                continue;
            }
        ubyte CC = combiningClassTrie[ch];
        if(lastCC > CC && CC != 0)
        {
            return seekStable!norm(idx, input);
        }
        
        if(notAllowedIn!norm(ch))
        {
           return seekStable!norm(idx, input);
        }
        lastCC = CC;
    }
    return tuple(input.length, input.length);
}

private auto seekStable(string norm, C)(size_t idx, in C[] input)
{
    auto br = input[0..idx];
    size_t region_start = 0;//default
    for(;;)
    {
        if(br.empty)//start is 0
            break;
        dchar ch = br.back;
        if(combiningClassTrie[ch] == 0 && allowedIn!norm(ch))
        {
            region_start = br.length - std.utf.codeLength!C(ch);
            break;
        }
        br.popFront();
    }
    ///@@@BUG@@@ can't use find: " find is a nested function and can't be used..."
    size_t region_end=input.length;//end is $ by default
    foreach(i, dchar ch; input[idx..$])
    {
        if(combiningClassTrie[ch] == 0 && allowedIn!norm(ch))
        {
            region_end = i+idx;
            break;
        }
    }
    //writeln("Region to normalize: ", input[region_start..region_end]);
    return tuple(region_start, region_end);
}

///Checks if dchar $(D ch) is always allowed (Quick_Check=YES) in normalization form $(D norm).
public bool allowedIn(string norm)(dchar ch)
{
    return !notAllowedIn!norm(ch);
}

//not user friendly name but more direct 
private bool notAllowedIn(string norm)(dchar ch)
{
    static if(norm == NFC)
        return nfcQC[ch];
    else static if(norm == NFD)
        return nfdQC[ch];
    else static if(norm == NFKC)
        return nfkcQC[ch];
    else static if(norm == NFKD)
        return nfkdQC[ch];
    else
        static assert("Unknown normalization form "~norm);
}

}

version(std_uni_bootstrap)
{
    //old version used for bootstrapping of gen_uni.d that generates 
    //up to date optimal versions of all of isXXX functions
    public bool isWhite(dchar c) 
    {
        return std.ascii.isWhite(c) ||
               c == lineSep || c == paraSep ||
               c == '\u0085' || c == '\u00A0' || c == '\u1680' || c == '\u180E' ||
               (c >= '\u2000' && c <= '\u200A') ||
               c == '\u202F' || c == '\u205F' || c == '\u3000';
    }
}
else
{
public:

/++
    Whether or not $(D c) is a Unicode whitespace character.
    (general Unicode category: Part of C0(tab, vertical tab, form feed,
    carriage return, and linefeed characters), Zs, Zl, Zp, and NEL(U+0085))
  +/
public bool isWhite(dchar c) 
{
    return isWhiteGen(c); //call pregenerated binary search
}

/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isLower) instead.)

    Return whether $(D c) is a Unicode lowercase character.
  +/
deprecated bool isUniLower(dchar c) //@safe pure nothrow
{
    return isLower(c);
}

/++
    Return whether $(D c) is a Unicode lowercase character.
+/
bool isLower(dchar c) //@safe pure nothrow
{
    if(std.ascii.isASCII(c))
        return std.ascii.isLower(c);

    return upperCaseTrie[c];
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isUpper) instead.)

    Return whether $(D c) is a Unicode uppercase character.
+/
deprecated bool isUniUpper(dchar c) //@safe pure nothrow
{
    return isUpper(c);
}

/++
    Return whether $(D c) is a Unicode uppercase character.
+/
bool isUpper(dchar c) //@safe pure nothrow
{
    if(std.ascii.isASCII(c))
        return std.ascii.isUpper(c);

    return lowerCaseTrie[c];
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D toLower) instead.)

    If $(D c) is a Unicode uppercase character, then its lowercase equivalent
    is returned. Otherwise $(D c) is returned.
     
    Warning: certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toLower which takes full string instead.
+/
deprecated dchar toUniLower(dchar c) //@safe pure nothrow
{
    return toLower(c);
}

/++
    If $(D c) is a Unicode uppercase character, then its lowercase equivalent
    is returned. Otherwise $(D c) is returned.
    
    Warning: certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toLower which takes full string instead.
+/
dchar toLower(dchar c) // @safe pure nothrow
{
     //optimize ASCII case
    if(c < 'A')
        return c;
    if(c <= 'Z')
        c += 32;
    else
    {
        size_t idx = simpleCaseTrie[c];
        alias simpleCaseTable stab;
        if(idx != EMPTY_CASE_TRIE)
        {
            size_t sz = stab[idx].size;
            idx = idx - stab[idx].n;
            switch(sz){
            default:
                assert(false);//no even buckets of size 5 currently
            case 5:
                if(stab[idx+4].isLower)
                    return stab[idx+4].ch;
                goto case;
            case 4:
                if(stab[idx+3].isLower)
                    return stab[idx+3].ch;
                goto case;
            case 3:
                if(stab[idx+2].isLower)
                    return stab[idx+2].ch;
                goto case;
            case 2:
                if(stab[idx+1].isLower)
                    return stab[idx+1].ch;
                if(stab[idx].isLower)
                    return stab[idx].ch;
            }
        }
    }
    return c;
}

unittest
{
    foreach(ch; 0..0x80)
        assert(std.ascii.toLower(ch) == toLower(ch));
    assert(toLower('Я') == 'я');
    assert(toLower('Δ') == 'δ');

}

/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D toUpper) instead.)

    If $(D c) is a Unicode lowercase character, then its uppercase equivalent
    is returned. Otherwise $(D c) is returned.
     
    Warning: certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toUpper which takes full string instead.
+/
deprecated dchar toUniUpper(dchar c) //@safe pure nothrow
{
    return toUpper(c);
}

/++
    If $(D c) is a Unicode lowercase character, then its uppercase equivalent
    is returned. Otherwise $(D c) is returned.
     
    Warning: certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toUpper which takes full string instead.
+/
dchar toUpper(dchar c) //@safe pure nothrow
{
    //optimize ASCII case
    if(c < 'a')
        return c;
    if(c <= 'z')
        c -= 32;
    else
    {
        size_t idx = simpleCaseTrie[c];
        alias simpleCaseTable stab;
        if(idx != EMPTY_CASE_TRIE)
        {
            size_t sz = stab[idx].size;
            idx = idx - stab[idx].n;
            switch(sz){
            default:
                assert(false);//no even buckets of size 5 currently
            case 5:
                if(stab[idx+4].isUpper)
                    return stab[idx+4].ch;
                goto case;
            case 4:
                if(stab[idx+3].isUpper)
                    return stab[idx+3].ch;
                goto case;
            case 3:
                if(stab[idx+2].isUpper)
                    return stab[idx+2].ch;
                goto case;
            case 2:
                if(stab[idx+1].isUpper)
                    return stab[idx+1].ch;
                if(stab[idx].isUpper)
                    return stab[idx].ch;
            }
        }
    }
    return c;
}


/++
   $(RED Deprecated. It will be removed in August 2012. Please use
   $(D isAlpha) instead.)

    Returns whether $(D c) is a Unicode alphabetic character
    (general Unicode category: Alphabetic).

  +/
deprecated bool isUniAlpha(dchar c) //@safe pure nothrow
{
    return isAlpha(c);
}

unittest
{
    foreach(ch; 0..0x80)
        assert(std.ascii.toUpper(ch) == toUpper(ch));
    assert(toUpper('я') == 'Я');
    assert(toUpper('δ') == 'Δ');

}

/++
    Returns whether $(D c) is a Unicode alphabetic character
    (general Unicode category: Alphabetic).

  +/
bool isAlpha(dchar c) /*@safe pure nothrow*/
{    
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

    return alphaTrie[c];
}

unittest
{
    auto alpha = unicode("Alphabetic");
    foreach(ch; alpha.byChar)
        assert(isAlpha(ch));   
    foreach(ch; 0..0x4000)
        assert((ch in alpha) == isAlpha(ch)); 
}


/++
    Returns whether $(D c) is a Unicode mark
    (general Unicode category: Mn, Me, Mc).

  +/


@trusted
bool isMark(dchar c) //@safe pure nothrow
{
    return markTrie[c];
}

unittest
{
    auto mark = unicode("Mark");
    foreach(ch; mark.byChar)
        assert(isMark(ch));   
    foreach(ch; 0..0x4000)
        assert((ch in mark) == isMark(ch)); 
}


/++
    Returns whether $(D c) is a Unicode numerical character
    (general Unicode category: Nd, Nl, No).
    
  +/

bool isNumber(dchar c) //@safe pure nothrow
{
    return numberTrie[c];
}

unittest
{
    auto n = unicode("N");
    foreach(ch; n.byChar)
        assert(isNumber(ch));
    foreach(ch; 0..0x4000)
        assert((ch in n) == isNumber(ch));
}


/++
    Returns whether $(D c) is a Unicode punctuation character
    (general Unicode category: Pd, Ps, Pe, Pc, Po, Pi, Pf).

+/
bool isPunctuation(dchar c) //@safe pure nothrow
{
    return punctuationTrie[c];
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
    foreach(ch; unicode("P").byChar)
        assert(isPunctuation(ch));
}


/++
    Returns whether $(D c) is a Unicode symbol character
    (general Unicode category: Sm, Sc, Sk, So)
    
+/
bool isSymbol(dchar c) //@safe pure nothrow
{
   return symbolTrie[c];
}

unittest
{
    import std.string;
    assert(isSymbol('\u0024'));
    assert(isSymbol('\u002B'));
    assert(isSymbol('\u005E'));
    assert(isSymbol('\u00A6'));
    foreach(ch; unicode("S").byChar)
        assert(isSymbol(ch), format("%04x", ch));
}

/++
    Returns whether $(D c) is a Unicode space character
    (general Unicode category: Zs)
    Note: that this doesn't include '\n', '\r', \t' and other non-space characters.
    For commonly used less strict semantics see $(LREF isWhite).
+/
bool isSpace(dchar c) //@safe pure nothrow
{
    return isSpaceGen(c);
}

unittest
{
    assert(isSpace('\u0020'));
    auto space = unicode.Zs;
    foreach(ch; space.byChar)
        assert(isSpace(ch));
    foreach(ch; 0..0x1000)
        assert(isSpace(ch) == space[ch]);
}


/++
    Returns whether $(D c) is a Unicode graphical character
    (general Unicode category: L, M, N, P, S, Zs).
 
+/
bool isGraphical(dchar c) //@safe pure nothrow
{
    return graphicalTrie[c];
}


unittest
{   
    auto set = unicode("Graphical");
    import std.string;
    foreach(ch; set.byChar)
        assert(isGraphical(ch), format("%4x", ch));    
    foreach(ch; 0..0x4000)
        assert((ch in set) == isGraphical(ch));
}


/++
    Returns whether $(D c) is a Unicode control character
    (general Unicode category: Cc)

  +/

bool isControl(dchar c) //@safe pure nothrow
{
    return isControlGen(c);
}

unittest
{
    assert(isControl('\u0000'));
    assert(isControl('\u0081'));
    assert(!isControl('\u0100'));
    auto cc = unicode.Cc;    
    foreach(ch; cc.byChar)
        assert(isControl(ch));
    foreach(ch; 0..0x1000)
        assert(isControl(ch) == cc[ch]);
}


/++
    Returns whether $(D c) is a Unicode formatting character
    (general Unicode category: Cf)

+/
bool isFormat(dchar c) //@safe pure nothrow
{
    return isFormatGen(c);
}


unittest
{
    assert(isFormat('\u00AD'));
    foreach(ch; unicode("Format").byChar)
        assert(isFormat(ch));
}

//codepoints for private use, surrogates are not likely to change in near feature
//if need be they can be generated from unicode data as well

/++
    Returns whether $(D c) is a Unicode Private Use character
    (general Unicode category: Co)
 
+/
bool isPrivateUse(dchar c) //@safe pure nothrow
{
    return (0x00_E000 <= c && c <= 0x00_F8FF)
        || (0x0F_0000 <= c && c <= 0x0F_FFFD)
        || (0x10_0000 <= c && c <= 0x10_FFFD);
}


/++
    Returns whether $(D c) is a Unicode surrogate character
    (general Unicode category: Cs)

+/
bool isSurrogate(dchar c) //@safe pure nothrow
{
    return (0xD800 <= c && c <= 0xDFFF);
}

/++
    Returns whether $(D c) is a Unicode high surrogate (lead surrogate).

+/
bool isSurrogateHi(dchar c) @safe pure nothrow
{
    return (0xD800 <= c && c <= 0xDBFF);
}

/++
    Returns whether $(D c) is a Unicode low surrogate (trail surrogate).

+/
bool isSurrogateLo(dchar c) @safe pure nothrow
{
    return (0xDC00 <= c && c <= 0xDFFF);
}

/++
    Returns whether $(D c) is a Unicode non-character
    (general Unicode category: Cn)
 
+/

bool isNonCharacter(dchar c) //@safe pure nothrow
{
    return nonCharacterTrie[c]; 
}


unittest
{
    auto set = unicode("Cn");
    foreach(ch; set.byChar)
        assert(isNonCharacter(ch));
}

private:
//load static data from pre-generated tables into usable datastructures

auto asSet(const (ubyte)[] compressed)
{
    return CodepointSet(decompressIntervals(compressed));
}

auto asTrie(T...)(in TrieEntry!T e)
{
    return CodepointTrie!T(e.offsets, e.sizes, e.data);
}

immutable alphaTrie = asTrie(alphaTrieEntries);
immutable markTrie = asTrie(markTrieEntries);
immutable numberTrie = asTrie(numberTrieEntries);
immutable punctuationTrie = asTrie(punctuationTrieEntries);
immutable symbolTrie = asTrie(symbolTrieEntries);
immutable graphicalTrie = asTrie(graphicalTrieEntries);
immutable formatTrie = asTrie(formatTrieEntries);
immutable nonCharacterTrie  = asTrie(nonCharacterTrieEntries);

immutable nfcQC = asTrie(nfcQCTrieEntries);
immutable nfdQC = asTrie(nfdQCTrieEntries);
immutable nfkcQC = asTrie(nfkcQCTrieEntries);
immutable nfkdQC = asTrie(nfkdQCTrieEntries);

immutable graphemeExtend = asTrie(graphemeExtendTrieEntries); 
immutable spacingMark = asTrie(mcTrieEntries);

//TODO: move sets below to Tries

__gshared CodepointSet hangLV;
__gshared CodepointSet hangLVT;
__gshared CodepointSet hangL;
__gshared CodepointSet hangV;
__gshared CodepointSet hangT;

shared static this()
{
    hangLV = asSet(unicodeLV);
    hangLVT = asSet(unicodeLVT);
    hangL = asSet(unicodeL);
    hangV = asSet(unicodeV);
    hangT = asSet(unicodeT);
}

immutable combiningClassTrie = asTrie(combiningClassTrieEntries);
immutable canonMapping = asTrie(canonMappingTrieEntries);
immutable compatMapping = asTrie(compatMappingTrieEntries);
immutable simpleCaseTrie = asTrie(simpleCaseTrieEntries);
immutable fullCaseTrie = asTrie(fullCaseTrieEntries);
immutable lowerCaseTrie = asTrie(lowerCaseTrieEntries);
immutable upperCaseTrie = asTrie(upperCaseTrieEntries);
immutable compositionJumpTrie = asTrie(compositionJumpTrieEntries);

}//version(!std_uni_bootstrap)