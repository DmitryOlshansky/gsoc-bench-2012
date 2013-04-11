// Written in the D programming language.

/++
    $(SECTION Overview)

    $(P The $(D std.uni) module provides an implementation 
    of fundamental Unicode algorithms and data structures.
    This doesn't include UTF encoding and decoding primitives, 
    see module std.utf for $(XREF _utf, decode) and $(XREF _utf, encode). )

    $(P All primitives listed operate on Unicode character and 
    sets of characters. For functions which operate on ASCII characters 
    and ignore Unicode $(CHARACTER), see $(LINK2 std_ascii.html, std.ascii).
    For definitions of Unicode $(CHARACTER), $(CODEPOINT) and other terms 
    used throughout this module see the section $(S_LINK Terminology, terminology)
    below.
    )

    $(P The focus of this module is the core needs of developing Unicode-aware 
    applications. To that effect it provides the following optimized primitives: 
    )
    $(UL 
    $(LI Character classification by category and common properties:
        $(LREF isAlpha), $(LREF isWhite) and others.
    )
    $(LI
        Case-insensitive string comparison ($(LREF sicmp), $(LREF icmp)).
    )
    $(LI
        Converting text to any of the four normalization forms via $(LREF normalize).
    )
    $(LI 
        Decoding ($(LREF decodeGrapheme))  and iteration ($(LREF graphemeStride))
        by user-perceived characters, that is by $(LREF Grapheme) clusters.
    )
    $(LI 
        Decomposing and composing of individual character(s) according to canonical 
        or compatibility rules, see $(LREF compose) and $(LREF decompose), 
        including the specific version for Hangul syllables $(LREF composeJamo)
        and $(LREF hangulDecompose).
    )
    )
    $(P It's recognized that an application may need further enhancements 
    and extensions, such as less-commonly known algorithms, 
    or tailoring existing ones for region-specific needs. To help users 
    with building any extra functionality beyond the core primitives,
    the module provides: 
    )
    $(UL 
    $(LI
        A type for easy manipulation of sets of characters $(LREF CodepointSet).
        Besides the typical set algebra it provides an unusual feature: 
        a D source code generator for detection of $(CODEPOINTS) in this set.
        This is a boon for meta-programming parser frameworks,
        and is used internally to power classification in small 
        sets like $(LREF isWhite).
    )
    $(LI
        A way to construct optimal packed multi-stage tables also known as a 
        special case of $(LUCKY Trie).
        The functions $(LREF codepointTrie), $(LREF codepointSetTrie) 
        construct custom tries that map dchar to value.
        The end result is a fast and predictable $(BIGOH 1) lookup that powers 
        functions like $(LREF isAlpha) and $(LREF combiningClass),
        but for user-defined data sets.
    )
    $(LI 
        Generally useful building blocks for customized normalization:
        $(LREF combiningClass) for querying combining class
        and $(LREF allowedIn) for testing Quick_Check.YES 
        property of a given normalization form.
    )
    $(LI 
        Access to a large selection of commonly used sets of $(CODEPOINTS). 
        The exact names can be observed in the CLDR utility, on page
        $(WEB www.unicode.org/cldr/utility/properties.jsp, property index).
        Supported ones include Script, Block and General Category.
        See $(LREF unicode) for easy and (optionally) compile-time checked queries.
    )
    )
    $(SECTION Synopsis)
    ---
    import std.uni;
    void main()
    {
        // initialize code point sets using script/block or property name
        // now 'set' contains code points from both scripts.
        auto set = unicode("Cyrillic") | unicode("Armenian");
        //same thing but simpler and checked at compile-time
        auto ascii = unicode.ASCII;
        auto currency = unicode.Currency_Symbol;

        // easy set ops
        auto a = set & ascii;
        assert(a.empty); // as it has no intersection with ascii
        a = set | ascii;
        auto b = currency - a; // subtract all ASCII, Cyrillic and Armenian

        // some properties of code point sets
        assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
        // testing presence of a code point in a set
        // is just fine, it is O(logN)
        assert(!b['$']); 
        assert(!b['\u058F']); // Armenian dram sign
        assert(b['¥']); 

        // building fast lookup tables, these guarantee O(1) complexity
        // 1-level Trie lookup table essentially a huge bit-set ~262Kb
        auto oneTrie = toTrie!1(b);
        // 2-level far more compact but typically slightly slower
        auto twoTrie = toTrie!2(b);
        // 3-level even smaller, and a bit slower yet
        auto threeTrie = toTrie!3(b);
        assert(oneTrie['£']);
        assert(twoTrie['£']);
        assert(threeTrie['£']);
        
        // build the trie with the most sensible trie level 
        // and bind it as a functor
        auto cyrilicOrArmenian = toDelegate(set);
        auto balance = find!(cyrilicOrArmenian)("Hello ընկեր!");
        assert(balance == "ընկեր!");
        // compatible with bool delegate(dchar)
        bool delegate(dchar) bindIt = cyrilicOrArmenian;

        // Normalization
        string s = "Plain ascii (and not only), is always normalized!";
        assert(s is normalize(s));// is the same string

        string nonS = "A\u0308ffin"; // A ligature
        auto nS = normalize(nonS); // to NFC, the W3C endorsed standard
        assert(nS == "Äffin");
        assert(nS != nonS);
        string composed = "Äffin";
        
        assert(normalize!NFD(composed) == "A\u0308ffin");
        // to NFKD, compatibility decomposition useful for fuzzy matching/searching
        assert(normalize!NFKD("2¹⁰") == "210");
    }
    ---
    $(SECTION Terminology)
    $(P The following is a list of important Unicode notions
    and definitions. Any conventions used specifically in this 
    module alone are marked as such. The descriptions are based on the formal 
    definition as found in The Unicode Standard Core Specification, 
    specifically the 3rd chapter.)

    $(P $(DEF Code point) Any value in the Unicode codespace; 
    that is, the range of integers from 0 to 10FFFF (hex).
    Not all code points are assigned to encoded characters.
    )
    $(P $(DEF Code unit) The minimal bit combination that can represent 
    a unit of encoded text for processing or interchange.  
    Depending on the encoding this could be:
    8-bit code units in the UTF-8 ($(D char)), 
    16-bit code units in the UTF-16 ($(D wchar)),
    and 32-bit code units in the UTF-32 ($(D dchar)).
    $(I Note that in UTF-32, a code unit is a code point 
    and is represented by the D $(D dchar) type.)
    )
    $(P $(DEF Abstract character) A unit of information used for the organization, 
    control, or representation of textual data.
    Note that:
        $(UL 
        $(LI When representing data, the nature of that data 
        is generally symbolic as opposed to some other 
        kind of data (for example, visual).)

        $(LI An abstract character has no concrete form 
        and should not be confused with a $(I glyph).)

        $(LI An abstract character does not necessarily 
        correspond to what a user thinks of as a “character”
         and should not be confused with a $(LREF Grapheme).)
        
        $(LI The abstract characters encoded (see Encoded character) 
        are known as Unicode abstract characters.)

        $(LI Abstract characters not directly 
        encoded by the Unicode Standard can often be
        represented by the use of combining character sequences.)
        )
    
    )
    $(P $(DEF Glyph) The actual, concrete image of a glyph representation 
    having been rasterized or otherwise imaged onto some display surface.
    )
    $(P $(DEF Encoded character) An association (or mapping) 
    between an abstract character and a code point.
    )    
    $(P $(DEF Character) Typically differs by context. 
    For the purpose of this documentation the term $(I character)
    implies $(I encoded character), that is a code point having 
    an assigned abstract character (a symbolic meaning).
    )    
    $(P $(DEF Grapheme cluster) Defined as the text between 
        grapheme boundaries  as specified by Unicode Standard Annex #29, 
        $(WEB www.unicode.org/reports/tr29/, Unicode text segmentation).
        Important general properties of a grapheme:
        $(UL 
            $(LI The grapheme cluster represents a horizontally segmentable 
            unit of text, consisting of some grapheme base (which may 
            consist of a Korean syllable) together with any number of 
            nonspacing marks applied to it.
            )
            $(LI  A grapheme cluster typically starts with a grapheme base 
            and then extends across any subsequent sequence of nonspacing marks. 
            A grapheme cluster is most directly relevant to text rendering and 
            processes such as cursor placement and text selection in editing, 
            but may also be relevant to comparison and searching. 
            )
            $(LI For many processes, a grapheme cluster behaves as if it was a 
            single character with the same properties as its grapheme base. 
            Effectively, nonspacing marks apply $(I graphically) to the base, 
            but do not change its properties.
            )
        )
        $(P This module defines a number of primitives that work with graphemes:
        $(LREF Grapheme), $(LREF decodeGrapheme) and $(LREF graphemeStride).
        All of them are using $(I extended grapheme) boundaries 
        as defined in the aforementioned standard annex.
        )
    )
    $(P $(DEF Grapheme base) A character with the property
     Grapheme_Base, or any standard Korean syllable block.
    )
    $(P $(DEF Combining character) A character with the General Category
     of Combining Mark(M).
        $(UL 
            $(LI All characters with non-zero canonical combining class 
            are combining characters, but the reverse is not the case:
            there are combining characters with a zero combining class.
            )
            $(LI These characters are not normally used in isolation 
            unless they are being described. They include such characters
            as accents, diacritics, Hebrew points, Arabic vowel signs, 
            and Indic matras.
            )
        )
    )
    $(P $(DEF Nonspacing mark) A combining character with the 
        General Category of Nonspacing Mark (Mn) or Enclosing Mark (Me).
    )
    $(P $(DEF Spacing mark) A combining character that is not a nonspacing mark.)
    
    $(P $(DEF Canonical decomposition) 
    The decomposition of a character or character sequence
    that results from recursively applying the canonical 
    mappings found in the Unicode Character Database 
    and these described in Conjoining Jamo Behavior
    (section 12 of 
    $(WEB www.unicode.org/uni2book/ch03.pdf, Unicode Conformance)).
    )

    $(P $(DEF Compatibility decomposition)
    The decomposition of a character or character sequence that results 
    from recursively applying both the compatibility mappings and
    the canonical mappings found in the Unicode Character Database, and those
    described in Conjoining Jamo Behavior no characters 
    can be further decomposed.
    )

    $(P $(DEF Canonical equivalent)
    Two character sequences are said to be canonical equivalents if
    their full canonical decompositions are identical.
    )

    $(P $(DEF Compatibility equivalent)
    Two character sequences are said to be compatibility
    equivalents if their full compatibility decompositions are identical.
    )

    $(P $(DEF Canonical composition)
    The precise definition of the Canonical composition 
    is the algorithm as specified in $(WEB www.unicode.org/uni2book/ch03.pdf, 
    Unicode Conformance) section 11. 
    Informally it's the process that does the reverse of the canonical
    decomposition with the addition of certain rules 
    that e.g. prevent legacy characters from appearing in the composed result.    
    )

    $(SECTION Normalization)
    
    $(P The concepts of $(S_LINK Canonical equivalent, canonical equivalent)
     or $(S_LINK Compatibility equivalent, compatibility equivalent)
    characters in the Unicode Standard make it necessary to have a full, formal 
    definition of equivalence for Unicode strings.
    String equivalence is determined by a process called normalization,
    whereby strings are converted into forms which are compared 
    directly for identity. This is the primary goal of the normalization process,
    see the function $(LREF normalize) to convert into any of 
    the four defined forms.
    )

    $(P A very important attribute of the Unicode Normalization Forms 
    is that they must remain stable between versions of the Unicode Standard. 
    A Unicode string normalized to a particular Unicode Normalization Form 
    in one version of the standard is guaranteed to remain in that Normalization 
    Form for implementations of future versions of the standard.
    )

    $(P The Unicode Standard specifies four normalization forms. 
    Informally, two of these forms are defined by maximal decomposition 
    of equivalent sequences, and two of these forms are defined 
    by maximal $(I composition) of equivalent sequences.
        $(UL 
        $(LI Normalization Form D (NFD): The $(S_LINK Canonical decomposition,
            canonical decomposition) of a character sequence.)
        $(LI Normalization Form KD (NFKD): The $(S_LINK Compatibility decomposition,
            compatibility decomposition) of a character sequence.)
        $(LI Normalization Form C (NFC): The canonical composition of the 
            $(S_LINK Canonical decomposition, canonical decomposition) 
            of a coded character sequence.)
        $(LI Normalization Form KC (NFKC): The canonical composition 
        of the $(S_LINK Compatibility decomposition,
            compatibility decomposition) of a character sequence)
        )
    )

    $(P The choice of the normalization form depends on the particular use case. 
    NFC is the best form for general text, since it's more compatible with 
    strings converted from legacy encodings. NFKC is the preferred form for 
    identifiers, especially where there are security concerns. NFD and NFKD 
    are the most useful for internal processing.
    )

    $(SECTION Construction of lookup tables)

    $(P The Unicode standard describes a set of algorithms that 
    depend on having the ability to quickly look up various properties 
    of a code point. Given the the codespace of about 1 million $(CODEPOINTS),
    it is not a trivial task to provide a space-efficient solution for 
    the multitude of properties.)

    $(P Common approaches such as hash-tables or binary search over
     sorted code point intervals (as in $(LREF InversionList)) are insufficient.
     Hash-tables have enormous memory footprint and binary search 
     over intervals is not fast enough for some heavy-duty algorithms.      
     )

    $(P The recommended solution (see Unicode Implementation Guidelines) 
    is using multi-stage tables that is an instance of 
    $(WEB http://en.wikipedia.org/wiki/Trie, Trie) with integer keys
    and a fixed number of stages. For the reminder of the section 
    it will be called a fixed trie. The following describes a particular 
    implementation that is aimed for the speed of access at the expense 
    of ideal size savings.
    )

    $(P Taking 2-level Trie as an example the principle of operation is as follows.
        Split the number of bits in a key (code point, 21 bits) into 2 components 
        (e.g. 15 and 8).  The first is the number of bits in the index of the trie
         and the other is number of bits in each page of the trie.
        The layout of the trie is then an array of size 2^^bits-of-index followed
        an array of memory chunks of size 2^^bits-of-page/bits-per-element. 
    )

    $(P The number of pages is variable (but no less then 1) 
        unlike the number of entries in the index. The slots of the index 
        all have to contain a number of a page that is present. The lookup is then 
        just a couple of operations - slice the upper bits, 
        lookup an index for these, take a page at this index and use
        the lower bits as an offset within this page.

        Assuming that pages are laid out consequently 
        in one array at $(D pages), the pseudo-code is:
    )
    ---        
    auto elemsPerPage = 2^^bits_per_page/Value.sizeOfInBits;
    pages[index[n>>bits_per_page]][n & (elemsPerPage-1)];
    ---
    $(P Where if the $(D elemsPerPage) is a power of 2 the whole process is 
    a handful of simple instructions and 2 array reads. Subsequent levels 
    of the trie are introduced by recursing on this notion - the index array
    is treated as values. The number of bits in index is then again 
    split into 2 parts, with pages over 'current-index' and the new 'upper-index'. 
    )

    $(P For completeness a level 1 trie is simply an array.
    The current implementation takes advantage of bit-packing values 
    when the range is known to be limited in advance (such as $(D bool)). 
    See also $(LREF BitPacked) for enforcing it manually. 
    The major size advantage however comes from the fact 
    that multiple $(B identical pages on every level are merged) by construction.
    )

    $(P The process of construction of a trie is more involved and is hidden from 
    the user in a form of convenience functions: $(LREF codepointTrie), 
    $(LREF codepointSetTrie) and even more convenient $(LREF toTrie). 
    In general a set or built-in AA with $(D dchar) type 
    can be turned into a trie. The trie object in this module 
    is read-only (immutable); it's effectively frozen after construction.
    )
    
    References:
        $(WEB www.digitalmars.com/d/ascii-table.html, ASCII Table),
        $(WEB en.wikipedia.org/wiki/Unicode, Wikipedia),
        $(WEB www.unicode.org, The Unicode Consortium),
        $(WEB www.unicode.org/reports/tr15/, Unicode normalization forms), 
        $(WEB www.unicode.org/reports/tr29/, Unicode text segmentation)
        $(WEB www.unicode.org/uni2book/ch05.pdf, 
            Unicode Implementation Guidelines)
        $(WEB www.unicode.org/uni2book/ch03.pdf, 
            Unicode Conformance)
    Trademarks:
        Unicode(tm) is a trademark of Unicode, Inc.

    Macros:
        WIKI=Phobos/StdUni

    Copyright: Copyright 2013 -
    License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   Dmitry Olshansky
    Source:    $(PHOBOSSRC std/_uni.d)
    Standards: $(WEB www.unicode.org/versions/Unicode6.2.0/, Unicode v6.2)

Macros:

SECTION = <h3><a id="$1">$0</a></h3>
DEF = <div><a id="$1"><i>$0</i></a></div>
S_LINK = <a href="#$1">$+</a>
CODEPOINT = $(S_LINK Code point, code point)
CODEPOINTS = $(S_LINK Code point, code points)
CHARACTER = $(S_LINK Character, character)
CHARACTERS = $(S_LINK Character, characters)
CLUSTER = $(S_LINK Grapheme cluster, grapheme cluster)
+/
module uni;

static import std.ascii;
import std.traits, std.range, std.algorithm, std.typecons,
    std.format, std.conv, std.typetuple, std.exception, core.stdc.stdlib;
import std.array; //@@BUG UFCS doesn't work with 'local' imports
import core.bitop;

version(std_uni_bootstrap){}
else
{
    import unicode_tables; // generated file
}

// update to reflect all major CPUs supporting unaligned reads
version(X86)
    enum hasUnalignedReads = true;
else version(X86_64)
    enum hasUnalignedReads = true;
else
    enum hasUnalignedReads = false; //better be safe then sorry

enum dchar lineSep = '\u2028'; /// Constant $(CODEPOINT) (0x2028) - line separator. 
enum dchar paraSep = '\u2029'; /// Constant $(CODEPOINT) (0x2029) - paragraph separator.

// test the intro example
unittest
{
    // initialize code point sets using script/block or property name
    // set contains code points from both scripts.
    auto set = unicode("Cyrillic") | unicode("Armenian");
    //or simpler and statically-checked look
    auto ascii = unicode.ASCII;
    auto currency = unicode.Currency_Symbol;

    // easy set ops
    auto a = set & ascii;
    assert(a.empty); // as it has no intersection with ascii
    a = set | ascii;
    auto b = currency - a; // subtract all ASCII, Cyrillic and Armenian

    // some properties of code point sets
    assert(b.length > 45); // 46 items in Unicode 6.1, even more in 6.2
    // testing presence of a code point in a set
    // is just fine, it is O(logN)
    assert(!b['$']); 
    assert(!b['\u058F']); // Armenian dram sign
    assert(b['¥']); 

    // building fast lookup tables, these guarantee O(1) complexity
    // 1-level Trie lookup table essentially a huge bit-set ~262Kb
    auto oneTrie = toTrie!1(b);
    // 2-level far more compact but typically slightly slower
    auto twoTrie = toTrie!2(b);
    // 3-level even smaller, and a bit slower yet
    auto threeTrie = toTrie!3(b);
    assert(oneTrie['£']);
    assert(twoTrie['£']);
    assert(threeTrie['£']);
    
    // build the trie with the most sensible trie level 
    // and bind it as a functor
    auto cyrilicOrArmenian = toDelegate(set);
    auto balance = find!(cyrilicOrArmenian)("Hello ընկեր!");
    assert(balance == "ընկեր!");
    // compatible with bool delegate(dchar)
    bool delegate(dchar) bindIt = cyrilicOrArmenian;

    // Normalization
    string s = "Plain ascii (and not only), is always normalized!";
    assert(s is normalize(s));// is the same string

    string nonS = "A\u0308ffin"; // A ligature
    auto nS = normalize(nonS); // to NFC, the W3C endorsed standard
    assert(nS == "Äffin");
    assert(nS != nonS);
    string composed = "Äffin";
    
    assert(normalize!NFD(composed) == "A\u0308ffin");
    // to NFKD, compatibility decomposition useful for fuzzy matching/searching
    assert(normalize!NFKD("2¹⁰") == "210");
}

// debug = std_uni;

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
    if(isBitPacked!T && !is(T == F))
{
    assert(from <= 2^^bitSizeOf!T-1);
    return T(cast(TypeOfBitPacked!T)from);
}

auto force(T, F)(F from)
    if(is(T == F))
{
    return from;
}

// cheap algorithm grease ;)
auto adaptIntRange(T, F)(F[] src)
{
    //@@@BUG when in the 9 hells will map be copyable again?!
    static struct ConvertIntegers
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

        // doesn't work with slices @@@BUG 7097
        @property size_t opDollar(){   return data.length; }
    }
    return ConvertIntegers(src);
}

// repeat X times the bit-pattern in val assuming it's length is 'bits'
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
{// for replicate
    size_t m = 0b111;
    foreach(i; TypeTuple!(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    {
        assert(replicateBits!(i, 3)(m)+1 == (1<<(3*i)));
        // writefln("%2d:%32b", i, replicateBits!(i, 3)(m));
    }
}

// multiple arrays squashed into one memory block
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

    @property auto slice(size_t n)()inout pure nothrow
    {
        auto ptr = raw_ptr!n;        
        size_t len = spaceFor!(bitSizeOf!(Types[n]))(sz[n]);
        assert(ptr + len <= storage.ptr+storage.length);
        return packedArrayView!(Types[n])(ptr[0..len]);
    }

    template length(size_t n)
    {
        @property size_t length()const{ return sz[n]; }

        @property void length(size_t new_size)
        {
            if(new_size > sz[n])
            {// extend
                size_t delta = (new_size - sz[n]);
                sz[n] += delta;
                delta = spaceFor!(bitSizeOf!(Types[n]))(delta);
                storage.length +=  delta;// extend space at end
                // raw_slice!x must follow resize as it could be moved!
                // next stmts move all data past this array, last-one-goes-first
                static if(n != dim-1)
                {
                    auto start = raw_ptr!(n+1);
                    // len includes delta
                    size_t len = (storage.ptr+storage.length-start);

                    copy(retro(start[0..len-delta])
                        , retro(start[delta..len]));

                    start[0..delta] = 0;
                    // offsets are used for raw_slice, ptr etc.
                    foreach(i; n+1..dim)
                        offsets[i] += delta;
                }
            }
            else if(new_size < sz[n])
            {// shrink
                size_t delta = (sz[n] - new_size);
                sz[n] -= delta;
                delta = spaceFor!(bitSizeOf!(Types[n]))(delta);            
                // move all data past this array, forward direction
                static if(n != dim-1)
                {
                    auto start = raw_ptr!(n+1);
                    size_t len = storage.length;
                    copy(start[delta..len]
                     , start[0..len-delta]);
                    
                    // adjust offsets last, they affect raw_slice
                    foreach(i; n+1..dim)
                        offsets[i] -= delta;
                }
                storage.length -= delta;
            }
            // else - NOP
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

    void store(OutRange)(scope OutRange sink) const
        if(isOutputRange!(OutRange, char))
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
    enum dim = Types.length;
    size_t[dim] offsets;// offset for level x
    size_t[dim] sz;// size of level x
    alias staticMap!(bitSizeOf, Types) bitWidth;
    size_t[] storage;
}

unittest
{
    // sizes are:
    // lvl0: 3, lvl1 : 2, lvl2: 1
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

    auto marr = MultiArray!(BitPacked!(uint, 4), BitPacked!(uint, 6))(20, 10);
    marr.length!0 = 15;
    marr.length!1 = 30;
    fill!1(marr, 30);
    fill!0(marr, 15);
    check!1(marr, 30);
    check!0(marr, 15);
}

unittest
{// more bitpacking tests
    alias MultiArray!(BitPacked!(size_t, 3)
                , BitPacked!(size_t, 4)
                , BitPacked!(size_t, 3)
                , BitPacked!(size_t, 6)
                , bool) Bitty;
    alias sliceBits!(13, 16) fn1;
    alias sliceBits!( 9, 13) fn2;
    alias sliceBits!( 6,  9) fn3;
    alias sliceBits!( 0,  6) fn4;
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

size_t spaceFor(size_t _bits)(size_t new_len) pure nothrow
{
    enum bits = _bits == 1 ? 1 : ceilPowerOf2(_bits);// see PackedArrayView
    static if(bits > 8*size_t.sizeof)
    {
        static assert(bits % (size_t.sizeof*8) == 0);
        return new_len * bits/(8*size_t.sizeof);
    }
    else
    {
        enum factor = size_t.sizeof*8/bits;
        return (new_len+factor-1)/factor; // rounded up
    }
}

template isBitPackableType(T)
{
    enum isBitPackableType = isBitPacked!T 
        || isIntegral!T || is(T == bool) || isSomeChar!T;
}

//============================================================================
template PackedArrayView(T)
    if((is(T dummy == BitPacked!(U, sz), U, size_t sz) 
        && isBitPackableType!U) || isBitPackableType!T)
{
    private enum bits = bitSizeOf!T;
    alias PackedArrayView = PackedArrayViewImpl!(T, bits > 1 ? ceilPowerOf2(bits) : 1);
}

// data is packed only by power of two sized packs per word,
// thus avoiding mul/div overhead at the cost of ultimate packing
// this construct doesn't own memory, only provides access, see MultiArray for usage
@trusted struct PackedArrayViewImpl(T, size_t bits)
{
pure nothrow:
    static assert(isPowerOf2(bits));
    import core.bitop;      

    this(inout(size_t)[] arr)inout
    {
        original = arr;
    }

    static if(factor == bytesPerWord// can safely pack by byte
         || factor == 1 //a whole word at a time
         || ((factor == bytesPerWord/2 || factor == bytesPerWord/4) 
                && hasUnalignedReads)) //this needs unaligned reads
    {   
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

        static if(isBitPacked!T) // lack of user-defined implicit conversion
        {
            void opIndexAssign(T val, size_t idx)
            {
                return opIndexAssign(cast(TypeOfBitPacked!T)val, idx);
            }
        }

        void opIndexAssign(TypeOfBitPacked!T val, size_t idx)
        {
            (cast(U*)original.ptr)[idx] = cast(U)val;
        }
    }
    else
    {
        T opIndex(size_t n) inout
        in
        {
            //@@BUG text is impure: text(n/factor, " vs ", original.length)
            assert(n/factor < original.length);
        }
        body
        {                     
            static if(factor == bytesPerWord*8)
            {
                // a re-write with less data dependency
                auto q = n / factor;
                auto r = n % factor;
                return force!T(original[q] & (mask<<r) ? 1 : 0);
            }
            else
            {
                auto q = n / factor;
                auto r = n % factor;
                return force!T((original[q] >> bits*r) & mask);
            }
        }

        static if(isBitPacked!T) // lack of user-defined implicit conversion
        {
            void opIndexAssign(T val, size_t idx)
            {
                return opIndexAssign(cast(TypeOfBitPacked!T)val, idx);
            }
        }

        void opIndexAssign(TypeOfBitPacked!T val, size_t n)
        in
        {
            //@@@ BUG text is not pure
            /*text("mask: ",mask, " bits: ", bits
                        , "value:", val, " > ", mask)*/
            static if(isIntegral!T)
                assert(val <= mask);
        }
        body
        {
            auto q = n / factor;
            auto r = n % factor;
            size_t tgt_shift = bits*r;
            size_t word = original[q];
            original[q] = (word & ~(mask<<tgt_shift)) 
                | (cast(size_t)val << tgt_shift);
        }
    }

    static if(isBitPacked!T) // lack of user-defined implicit conversions
    {
        void opSliceAssign(T val, size_t start, size_t end)    
        {
            opSliceAssign(cast(TypeOfBitPacked!T)val, start, end);
        }
    }
    void opSliceAssign(TypeOfBitPacked!T val, size_t start, size_t end)
    {
        // rounded to factor granuarity
        // TODO: re-test and implement
        /*size_t pad_start = (start+factor/2)/factor*factor;// rounded up
        size_t pad_end = end/factor*factor; // rounded down
        size_t i;
        for(i=start; i<pad_start; i++)
            this[i] = val;
        writeln("!!~!~!!");
        // all in between is x*factor elements
        if(pad_start != pad_end)
        {
            size_t repval = replicateBits!(factor, bits)(val);
            for(size_t j=i/factor; i<pad_end; i+=factor, j++)
                original[j] = repval;// so speed it up by factor
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

    bool opEquals(T)(auto ref T arr) const
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

    // factor - number of elements in one machine word
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
        return (*arr)[from+idx];
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

    // static if(assignableSlice)
    void opSliceAssign(T)(T val, size_t start, size_t end)
    {
        (*arr)[start+from .. end+from] = val;
    }

    auto opSlice()
    {
        return typeof(this)(from, to, arr);
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

    bool opEquals(T)(auto ref T arr) const
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

// BUG? forward reference to return type of sliceOverIndexed!Grapheme
SliceOverIndexed!(const(T)) sliceOverIndexed(T)(size_t a, size_t b, const(T)* x)
    if(is(Unqual!T == T))
{
    return SliceOverIndexed!(const(T))(a, b, x);
}

// BUG? inout is out of reach 
//...SliceOverIndexed.arr only parameters or stack based variables can be inout
SliceOverIndexed!T sliceOverIndexed(T)(size_t a, size_t b, T* x)
    if(is(Unqual!T == T))
{
    return SliceOverIndexed!T(a, b, x);
}

unittest
{
    int[] idxArray = [2, 3, 5, 8, 13];
    auto sliced = sliceOverIndexed(0, idxArray.length, &idxArray);

    assert(!sliced.empty);
    assert(sliced.front == 2);
    sliced.front = 1;
    assert(sliced.front == 1);
    assert(sliced.back == 13);
    sliced.popFront();
    assert(sliced.front == 3);
    assert(sliced.back == 13);
    sliced.back = 11;
    assert(sliced.back == 11);
    sliced.popBack();

    assert(sliced.front == 3);
    assert(sliced[$-1] == 8);
    sliced = sliced[];
    assert(sliced[0] == 3);
    assert(sliced.back == 8);
    sliced = sliced[1..$];
    assert(sliced.front == 5);
    sliced = sliced[0..$-1];
    assert(sliced[$-1] == 5);

    int[] other = [2, 5];
    assert(sliced[] == sliceOverIndexed(1, 2, &other));
    sliceOverIndexed(0, 2, &idxArray)[0..2] = -1;
    assert(idxArray[0..2] == [-1, -1]);
    uint[] nullArr = null;
    auto nullSlice = sliceOverIndexed(0, 0, &idxArray);
    assert(nullSlice.empty);
}

private auto packedArrayView(T)(inout(size_t)[] arr) @trusted pure nothrow
{    
    return inout(PackedArrayView!T)(arr);
}


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

bool isPowerOf2(size_t sz) @safe pure nothrow
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
   
//
size_t floorPowerOf2(size_t arg) @safe pure nothrow
{
    assert(arg > 1); // else bsr is undefined
    return 1<<bsr(arg-1);
}

size_t ceilPowerOf2(size_t arg) @safe pure nothrow
{
    assert(arg > 1); // else bsr is undefined
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
        size_t n = floorPowerOf2(range.length);
        if(pred(range[n-1], needle))
        {// search in another 2^^k area that fully covers the tail of range
            size_t k = ceilPowerOf2(range.length - n + 1);
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
// hope to see simillar stuff in public interface... once Allocators are out
//@@@BUG moveFront and friends? dunno, for now it's POD-only

@trusted size_t genericReplace(Policy=void, T, Range)
    (ref T dest, size_t from, size_t to, Range stuff)
{
    size_t delta = to - from;
    size_t stuff_end = from+stuff.length;
    if(stuff.length > delta)
    {// replace increases length
        delta = stuff.length - delta;// now, new is > old  by delta
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
    {// replace decreases length by delta
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


// Simple storage manipulation policy
@trusted public struct GcPolicy
{
    static T[] dup(T)(const T[] arr)
    {
        return arr.dup;
    }

    static T[] alloc(T)(size_t size)
    {
        return new T[size];
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
        arr = null;
    }

    static void destroy(T)(ref T arr)
        if(isDynamicArray!T && !is(Unqual!T == T))
    { 
        arr = null; 
    }
}

// ditto
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
    Tests if T is some kind a set of code points. Intended for template constraints.    
*/
public template isCodepointSet(T)
{
    static if(is(T dummy == InversionList!(Args), Args...))
        enum isCodepointSet = true;
    else
        enum isCodepointSet = false;
}

/**
    Tests if $(D T) is a pair of integers that implicitly convert to $(D V).
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


/**
    The recommended default type for set of $(CODEPOINTS).
    For details, see the current implementation: $(LREF InversionList).
*/
public alias InversionList!GcPolicy CodepointSet;

/**
    The recommended type of $(XREF _typecons, Tuple)
    to represent [a, b) intervals of $(CODEPOINTS). As used in $(LREF InversionList).
    Any interval type should pass $(LREF isIntegralPair) trait.
*/
public alias Tuple!(uint, "a", uint, "b") CodepointInterval;

/**
    $(P 
    $(D InversionList) is a set of $(CODEPOINTS)
    represented as an array of open-right [a, b$(RPAREN) 
    intervals (see $(LREF CodepointInterval) above). 
    The name comes from the way the representation reads left to right.
    For instance a set of all values [10, 50$(RPAREN), [80, 90$(RPAREN), 
    plus a singular value 60 looks like this:
    )
    ---
    10, 50, 60, 61, 80, 90
    ---
    $(P
    The way to read this is: start with negative meaning that all numbers 
    smaller then the next one are not present in this set (and positive 
    - the contrary). Then switch positive/negative after each 
    number passed from left to right.
    )
    $(P This way negative spans until 10, then positive until 50, 
    then negative until 60, then positive until 61, and so on.
    As seen this provides a space-efficient storage of highly redundant data 
    that comes in long runs. A description which Unicode $(CHARACTER) 
    properties fit nicely. The technique itself could be seen as a variation 
    on $(LUCKY RLE encoding).
    )
    
    $(P Sets are value types (just like $(D int) is) thus they 
        are never aliased.
    )
        Example:
        ---
        auto a = CodepointSet('a', 'z'+1);
        auto b = CodepointSet('A', 'Z'+1);
        auto c = a;
        a = a | b;
        assert(a == CodepointSet('A', 'Z'+1, 'a', 'z'+1));
        assert(a != c);
        ---
    $(P See also $(LREF unicode) for simpler construction of sets
        out of predefined ones.
    )

    $(P Memory usage is 6 bytes per each contiguous interval in a set. 
    The value semantics are achieved by using COW technique  
    and thus it's $(RED not) safe to cast this type to shared.
    )

    Note: 
    $(P It's not recommended to rely on the template parameters
    or the exact type of a current $(CODEPOINT) set in $(D std.uni). 
    The type and parameters may change when standard 
    allocators design is finalized.
    Use $(LREF isCodepointSet) with templates or just stick with the default 
    alias $(LREF CodepointSet) throughout the whole code base.
    )
*/
@trusted public struct InversionList(SP=GcPolicy)
{
public:
    /**
        Construct from another code point set of any type.
    */
    this(Set)(Set set)
        if(isCodepointSet!Set)
    {
        uint[] arr;
        foreach(v; set.byInterval)
        {
            arr ~= v.a;
            arr ~= v.b;
        }
        data = Uint24Array!(SP)(arr);
    }

    /**
        Construct a set from a range of sorted code point intervals.
    */
    this(Range)(Range intervals)
        if(isForwardRange!Range && isIntegralPair!(ElementType!Range))
    {
        auto flattened = roundRobin(intervals.save.map!"a[0]", intervals.save.map!"a[1]");
        data = Uint24Array!(SP)(flattened);
    }

    /**
        Construct a set from plain values of sorted code point intervals.
        Example:
        ---
        auto set = CodepointSet('a', 'z'+1, 'а', 'я'+1);
        foreach(v; 'a'..'z'+1)
            assert(set[v]);
        //Cyrillic lowercase interval
        foreach(v; 'а'..'я'+1)
            assert(set[v]);
        ---
    */
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

    /**
        Get range that spans all of the $(CODEPOINT) intervals in this $(LREF InversionList).

        Example:
        ---
        import std.algorithm, std.typecons;
        auto set = CodepointSet('A', 'D'+1, 'a', 'd'+1);
        set.byInterval.equal([tuple('A', 'E'), tuple('a', 'e')]);
        ---
    */
    @property auto byInterval() 
    {        
        static struct Intervals
        {
            this(Uint24Array!SP sp)
            {
                slice = sp;
                start = 0;
                end = sp.length;
            }

            @property auto front()const
            {
                uint a = slice[start];
                uint b = slice[start+1];                
                return CodepointInterval(a, b);
            }

            @property auto back()const
            {
                uint a = slice[end-2];
                uint b = slice[end-1];
                return CodepointInterval(a, b);
            }

            void popFront()
            {
                start += 2;
            }

            void popBack()
            {
                end -= 2;
            }

            @property bool empty()const { return start == end; }

            @property auto save(){ return this; }
        private:
            size_t start, end;
            Uint24Array!SP slice;
        }
        return Intervals(data);
    }

    /**
        Tests the presence of code point $(D val) in this set.

        Example:
        ---
        auto gothic = unicode.Gothic;
        //Gothic letter ahsa
        assert(gothic['\U00010330']);
        // no ascii in Gothic obviously
        assert(!gothic['$']);
        ---
    */
    bool opIndex(uint val) const
    {
        // the <= ensures that searching in  interval of [a, b) for 'a' you get .length == 1
        // return assumeSorted!((a,b) => a<=b)(data[]).lowerBound(val).length & 1;
        return sharSwitchLowerBound!"a<=b"(data[], val) & 1;
    }

    /// Number of $(CODEPOINTS) in this set
    @property size_t length()
    {
        size_t sum = 0;
        foreach(iv; byInterval)
        {
            sum += iv.b - iv.a;
        }
        return sum;
    }

// bootstrap full set operations from 4 primitives (suitable as a template mixin):
// addInterval, skipUpTo, dropUpTo & byInterval iteration
//============================================================================
public:
    /**
        $(P Sets support natural syntax for set algebra, namely: )
        $(BOOKTABLE ,
            $(TR $(TH Operator) $(TH Math notation) $(TH Description) )
            $(TR $(TD &) $(TD a ∩ b) $(TD intersection) )
            $(TR $(TD |) $(TD a ∪ b) $(TD union) )
            $(TR $(TD -) $(TD a ∖ b) $(TD subtraction) )
            $(TR $(TD ~) $(TD a ~ b) $(TD symmetric set difference i.e. (a ∪ b) \ (a ∩ b)) )
        )

        Example:
        ---
        auto lower = unicode.LowerCase;
        auto upper = unicode.UpperCase;
        auto ascii = unicode.ASCII;

        assert((lower & upper).empty); //no intersection
        auto lowerASCII = lower & ascii;
        assert(lowerASCII.byCodepoint.equal(iota('a', 'z'+1)));
        //throw away all of the lowercase ASCII
        assert((ascii - lower).length == 128 - 26);

        auto onlyOneOf = lower ~ ascii;
        assert(!onlyOneOf['Δ']); //not ASCII and not lowercase
        assert(onlyOneOf['$']); //ASCII and not lowercase
        assert(!onlyOneOf['a']); //ASCII and lowercase
        assert(onlyOneOf['я']); //not ASCII but lowercase

        //throw away all cased letters from ASCII
        auto noLetters = ascii - (lower | upper);
        assert(noLetters.length == 128 - 26*2);
        ---
    */
    This opBinary(string op, U)(U rhs) 
        if(isCodepointSet!U || is(U:dchar))
    {
        static if(op == "&" || op == "|" || op == "~")
        {// symmetric ops thus can swap arguments to reuse r-value
            static if(is(U:dchar))
            {
                auto tmp = this;
                mixin("tmp "~op~"= rhs; ");
                return tmp;
            }
            else
            {
                static if(is(Unqual!U == U))
                {
                    // try hard to reuse r-value         
                    mixin("rhs "~op~"= this;");
                    return rhs;
                }
                else
                {
                    auto tmp = this;
                    mixin("tmp "~op~"= rhs;");
                    return tmp;
                }
            }
        }
        else static if(op == "-") // anti-symmetric
        {
            auto tmp = this;
            tmp -= rhs;
            return tmp;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    /// The 'op=' versions of the above overloaded operators.
    ref This opOpAssign(string op, U)(U rhs)
        if(isCodepointSet!U || is(U:dchar))
    {
        static if(op == "|")    // union
        {
            static if(is(U:dchar))
            {
                this.addInterval(rhs, rhs+1);
                return this;
            }
            else
                return this.add(rhs);
        }
        else static if(op == "&")   // intersection
                return this.intersect(rhs);// overloaded
        else static if(op == "-")   // set difference
                return this.sub(rhs);// overloaded
        else static if(op == "~")   // symmetric set difference
        {
            auto copy = this & rhs;
            this |= rhs;
            this -= copy;
            return this;
        }
        else
            static assert(0, "no operator "~op~" defined for Set");
    }

    /**
        Tests the presence of codepoint $(D ch) in this set,
        the same as $(LREF opIndex).
    */
    bool opBinaryRight(string op: "in", U)(U ch)
        if(is(U : dchar))
    {
        return this[ch];
    }

    /// Obtains a set that is the inversion of this set. See also $(LREF inverted).
    auto opUnary(string op: "!")()
    {
        return this.inverted; 
    }

    /**
        A range that spans each $(CODEPOINT) in this set.

        Example:
        ---
        import std.algorithm;
        auto set = unicode.ASCII;
        set.byCodepoint.equal(iota(0, 0x80));
        ---
    */
    @property auto byCodepoint()
    {
        @trusted static struct CodepointRange
        {
            this(This set)
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

        return CodepointRange(this);
    }

    /**
        $(P Obtain textual representation of this set in from of 
        open-right intervals and feed it to $(D sink). 
        )        
        $(P Used by various standard formatting facilities such as
         $(XREF _format, formattedWrite), $(XREF _stdio, write),
         $(XREF _stdio, writef), $(XREF _conv, to) and others.         
        )
        Example:
        ---
        import std.conv;
        assert(unicode.ASCII.to!string == "[0..128$(RPAREN)");        
        ---
    */
    void toString(scope void delegate (const(char)[]) sink)
    {
        import std.format;
        auto range = byInterval;
        if(range.empty)
            return;
        auto val = range.front;
        formattedWrite(sink, "[%d..%d)", val.a, val.b);
        range.popFront();
        foreach(i; range)
            formattedWrite(sink, " [%d..%d)", i.a, i.b);
    }
    /**
        Add an interval [a, b) to this set.

        Example:
        ---
        CodepointSet someSet;
        someSet.add('0', '5').add('A','Z'+1);
        someSet.add('5', '9'+1);
        assert(someSet['0']);
        assert(someSet['5']);
        assert(someSet['9']);        
        assert(someSet['Z']);
        ---
    */
    ref add()(uint a, uint b)
    {
        addInterval(a, b);
        return this;
    }

private:

    ref intersect(U)(U rhs)
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
        return subChar(ch);
    }

    // same as the above except that skip & drop parts are swapped
    ref sub(U)(U rhs)
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

    ref add(U)(U rhs)
        if(isCodepointSet!U)
    {
        Marker start;
        foreach(i; rhs.byInterval)
        {
            start = addInterval(i.a, i.b, start);
        }
        return this;
    }
//end of mixin-able part
//============================================================================
public:
    /**
        Obtains a set that is the inversion of this set.

        See the '!' $(LREF opUnary) for the same but using operators.

        Example:
        ---
        set = unicode.ASCII;
        //union with the inverse gets all of the code points in the Unicode
        assert((set | set.inverted).length == 0x110000);
        //no intersection with the inverse 
        assert((set & set.inverted).empty);
        ---
    */
    auto inverted()
    {
        InversionList inversion = this;
        if(inversion.data.length == 0)
        {
            inversion.addInterval(0, lastDchar+1);
            return inversion;
        }
        if(inversion.data[0] != 0)
            genericReplace(inversion.data, 0, 0, [0]);
        else
            genericReplace(inversion.data, 0, 1, cast(uint[])null);
        if(data[data.length-1] != lastDchar+1)
            genericReplace(inversion.data,
                inversion.data.length, inversion.data.length, [lastDchar+1]);
        else
            genericReplace(inversion.data,
                inversion.data.length-1, inversion.data.length, cast(uint[])null);

        return inversion;
    }

    /**
        Generates string with D source code of unary function with name of 
        $(D funcName) taking a single $(D dchar) argument. If $(D funcName) is empty
        the code is adjusted to be a lambda function.

        The function generated tests if the $(CODEPOINT) passed
        belongs to this set or not. The result is to be used with string mixin.
        The intended usage area is aggressive optimization via meta programming 
        in parser generators and the like.

        Note: To be used with care for relatively small or regular sets. It
        could end up being slower then just using multi-staged tables.

        Example:
        ---
        import std.stdio;

        //construct set directly from [a, b) intervals
        auto set = CodepointSet(10, 12, 45, 65, 100, 200);
        writeln(set);
        writeln(set.toSourceCode("func"));
        ---

        The above outputs something along the lines of:
        ---
        bool func(dchar ch)
        {
            if(ch < 45)
            {
                if(ch == 10 || ch == 11) return true;
                return false;
            }
            else if (ch < 65) return true;
            else
            {
                if(ch < 100) return false;
                if(ch < 200) return true;
                return false;
            }
        }
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
                    if(ival[0] != 0) // dchar is unsigned and  < 0 is useless
                        result ~= format("%sif(ch < %s) return false;\n", deeper, ival[0]);
                    result ~= format("%sif(ch < %s) return true;\n", deeper, ival[1]);
                }
            }
            result ~= format("%sreturn false;\n%s}\n", deeper, indent); // including empty range of intervals
            return result;
        }

        static string binaryScope(R)(R ivals, string indent)
        { 
            // time to do unrolled comparisons?
            if(ivals.length < maxBinary)
                return linearScope(ivals, indent);
            else
                return bisect(ivals, ivals.length/2, indent);
        }

        // not used yet if/elsebinary search is far better with DMD  as of 2.061
        // and GDC is doing fine job either way
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
            // bisect on one [a, b) interval at idx
            string result = indent~"{\n";
            // less branch, < a
            result ~= format("%sif(ch < %s)\n%s", 
                deeper, range[idx][0], binaryScope(range[0..idx], deeper));            
            // middle point,  >= a && < b
            result ~= format("%selse if (ch < %s) return true;\n", 
                deeper, range[idx][1]);
            // greater or equal branch,  >= b
            result ~= format("%selse\n%s", 
                deeper, binaryScope(range[idx+1..$], deeper));
            return result~indent~"}\n";
        }

        string code = format("bool %s(dchar ch) @safe pure nothrow\n",
            funcName.empty ? "function" : funcName);
        auto range = byInterval.array;
        // special case first bisection to be on ASCII vs beyond
        auto tillAscii = countUntil!"a[0] > 0x80"(range);
        if(tillAscii <= 0) // everything is ASCII or nothing is ascii (-1 & 0)
            code ~= binaryScope(range, "");
        else
            code ~= bisect(range, tillAscii, "");
        return code;
    }

    /**
        True if this set doesn't contain any $(CODEPOINTS). 
        Example:
        ---
        CodepointSet emptySet;
        assert(emptySet.length == 0);
        assert(emptySet.empty);
        ---
    */
    @property bool empty() const
    {
        return data.length == 0;
    }

private:
    alias typeof(this) This;
    alias size_t Marker;

    // special case for normal InversionList
    ref subChar(dchar ch)
    {
        auto mark = skipUpTo(ch);
        if(mark != data.length
            && data[mark] == ch && data[mark-1] == ch)
        {
            // it has split, meaning that ch happens to be in one of intervals
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
            if(a_idx & 1)// a in positive
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
        {// a in positive
            if(b_idx & 1)// b in positive
            {
                //  [-------++++++++----++++++-]
                //  [       s    a        b    ]
                to_insert = [top];
            }
            else // b in negative
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
            if(b_idx & 1) // b in positive
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
        assert(pos % 2 == 0); // at start of interval
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
        {   // a in positive
            //[--+++----++++++----+++++++------...]
            //      |<---si       s  a  t
            genericReplace(data, pos, idx, [a]);
        }
        else
        {   // a in negative
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
        assert(result % 2 == 0);// always start of interval
        //(may be  0-width after-split)
    }
    body
    {
        assert(data.length % 2 == 0);
        auto range = assumeSorted!"a<=b"(data[pos..data.length]);
        size_t idx = pos+range.lowerBound(a).length;

        if(idx >= data.length) // could have Marker point to recently removed stuff
            return data.length;

        if(idx & 1)// inside of interval, check for split
        {

            uint top = data[idx];
            if(top == a)// no need to split, it's end
                return idx+1;
            uint start = data[idx-1];
            if(a == start)
                return idx-1;
            // split it up
            genericReplace(data, idx, idx+1, [a, a, top]);
            return idx+1;        // avoid odd index
        }
        return idx;
    }

    Uint24Array!SP data;
};

@system unittest 
{
    //test examples
    import std.algorithm, std.typecons;
    auto set = CodepointSet('A', 'D'+1, 'a', 'd'+1);
    set.byInterval.equal([tuple('A', 'E'), tuple('a', 'e')]);
    set = unicode.ASCII;
    assert(set.byCodepoint.equal(iota(0, 0x80)));
    set = CodepointSet('a', 'z'+1, 'а', 'я'+1);
    foreach(v; 'a'..'z'+1)
        assert(set[v]);
    //Cyrillic lowercase interval
    foreach(v; 'а'..'я'+1)
        assert(set[v]);

    auto gothic = unicode.Gothic;
    //Gothic letter ahsa
    assert(gothic['\U00010330']);
    // no ascii in Gothic obviously
    assert(!gothic['$']);

    CodepointSet emptySet;
    assert(emptySet.length == 0);
    assert(emptySet.empty);

    set = unicode.ASCII;
    //union with the inverse gets all of code points in the Unicode
    assert((set | set.inverted).length == 0x110000);
    //no intersection with inverse 
    assert((set & set.inverted).empty);

    CodepointSet someSet;
    someSet.add('0', '5').add('A','Z'+1);
    someSet.add('5', '9'+1);
    assert(someSet['0']);
    assert(someSet['5']);
    assert(someSet['9']);
    assert(someSet['Z']);

    auto lower = unicode.LowerCase;
    auto upper = unicode.UpperCase;
    auto ascii = unicode.ASCII;
    assert((lower & upper).empty); //no intersection
    auto lowerASCII = lower & ascii;
    assert(lowerASCII.byCodepoint.equal(iota('a', 'z'+1)));
    //throw away all of the lowercase ASCII
    assert((ascii - lower).length == 128 - 26);
    auto onlyOneOf = lower ~ ascii;
    assert(!onlyOneOf['Δ']); //not ASCII and not lowercase
    assert(onlyOneOf['$']); //ASCII and not lowercase
    assert(!onlyOneOf['a']); //ASCII and lowercase
    assert(onlyOneOf['я']); //not ASCII but lowercase
    
    auto noLetters = ascii - (lower | upper);
    assert(noLetters.length == 128 - 26*2);
    import std.conv;
    assert(unicode.ASCII.to!string == "[0..128)");
}

// pedantic version for ctfe, and aligned-access only architectures
@trusted uint safeRead24(const ubyte* ptr, size_t idx) pure nothrow
{
    idx *= 3;
    version(LittleEndian)
        return ptr[idx] + (cast(uint)ptr[idx+1]<<8)
             + (cast(uint)ptr[idx+2]<<16);
    else
        return (cast(uint)ptr[idx]<<16) + (cast(uint)ptr[idx+1]<<8)
             + ptr[idx+2];
}

// ditto
@trusted void safeWrite24(ubyte* ptr, uint val, size_t idx) pure nothrow
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

// unaligned x86-like read/write functions
@trusted uint unalignedRead24(const ubyte* ptr, size_t idx) pure nothrow
{
    uint* src = cast(uint*)(ptr+3*idx);
    version(LittleEndian)
        return *src & 0xFF_FFFF;
    else
        return *src >> 8;
}

// ditto
@trusted void unalignedWrite24(ubyte* ptr, uint val, size_t idx) pure nothrow
{
    uint* dest = cast(uint*)(cast(ubyte*)ptr + 3*idx);
    version(LittleEndian)
        *dest = val | (*dest & 0xFF00_0000);
    else
        *dest = (val<<8) | (*dest & 0xFF);
}

uint read24(const ubyte* ptr, size_t idx) pure nothrow
{
    static if(hasUnalignedReads)
        return __ctfe ? safeRead24(ptr, idx) : unalignedRead24(ptr, idx);
    else
        return safeRead24(ptr, idx);
}

void write24(ubyte* ptr, uint val, size_t idx) pure nothrow
{
    static if(hasUnalignedReads)
        return __ctfe ? safeWrite24(ptr, val, idx) : unalignedWrite24(ptr, val, idx);
    else
        return safeWrite24(ptr, val, idx);    
}

// Packed array of 24-bit integers, COW semantics.
@trusted struct Uint24Array(SP=GcPolicy)
{
    this(Range)(Range range)
        if(isInputRange!Range && hasLength!Range)
    {
        length = range.length;
        copy(range, this[]);
    }

    this(Range)(Range range)
        if(isForwardRange!Range && !hasLength!Range)
    {
        auto len = walkLength(range.save);
        length = len;
        copy(range, this[]);
    }

    this(this)
    {           
        if(!empty)
        {
            refCount = refCount + 1;
        }
    }

    ~this()
    {
        if(!empty)
        {
            auto cnt = refCount;
            if(cnt == 1)
                SP.destroy(data);
            else
                refCount = cnt - 1;
        }
    }

    //no ref-count for empty U24 array
    @property bool empty() const { return data.length == 0; }

    //report one less then actual size
    @property size_t length() const 
    { 
        return data.length ? (data.length-4)/3 : 0; 
    }

    //+ an extra slot for ref-count
    @property void length(size_t len)
    {
        if(len == 0)
        {
            if(!empty)
                freeThisReference();
            return;
        }
        immutable bytes = len*3+4; //including ref-count
        if(empty)
        {
            data = SP.alloc!ubyte(bytes);
            refCount = 1;
            return;
        }
        auto cur_cnt = refCount;
        if(cur_cnt != 1) //have more references to this memory
        {
            refCount = cur_cnt - 1;
            auto new_data = SP.alloc!ubyte(bytes);
            // take shrinking into account
            auto to_copy = min(bytes, data.length)-4;
            copy(data[0..to_copy], new_data[0..to_copy]);
            data = new_data; // before setting refCount!
            refCount = 1;
        }
        else // 'this' is the only reference
        {
            // use the realloc (hopefully in-place operation)
            data = SP.realloc(data, bytes);
            refCount = 1; //setup a ref-count in the new end of the array
        }
    }

    alias opDollar = length;

    // Read 24-bit packed integer
    uint opIndex(size_t idx)const
    {
        return read24(data.ptr, idx);
    }

    // Write 24-bit packed integer
    void opIndexAssign(uint val, size_t idx)
    in
    {
        assert(!empty && val <= 0xFF_FFFF);
    }
    body
    {
        auto cnt = refCount;
        if(cnt != 1)
            dupThisReference(cnt);
        write24(data.ptr, val, idx);
    }

    //
    auto opSlice(size_t from, size_t to)
    {
        return sliceOverIndexed(from, to, &this);
    }

    ///
    auto opSlice(size_t from, size_t to) const
    {
        return sliceOverIndexed(from, to, &this);
    }

    //length slices before the ref count
    auto opSlice()
    {
        return opSlice(0, length);
    }

    //length slices before the ref count
    auto opSlice() const
    {
        return opSlice(0, length);
    }

    void append(Range)(Range range)
        if(isInputRange!Range && hasLength!Range && is(ElementType!Range : uint))
    {
        size_t nl = length + range.length;
        length = nl;
        copy(range, this[nl-range.length..nl]);
    }

    void append()(uint val)
    {
        length = length + 1;
        this[$-1] = val;
    }
    
    bool opEquals()(auto const ref Uint24Array rhs)const
    {
        if(empty ^ rhs.empty)
            return false; //one is empty and the other isn't
        return empty || data[0..$-4] == rhs.data[0..$-4];
    }

private:
    // ref-count is right after the data
    @property uint refCount() const 
    { 
        return read24(data.ptr, length); 
    }

    @property void refCount(uint cnt)
    in
    {
        assert(cnt <= 0xFF_FFFF);
    }
    body
    { 
        write24(data.ptr, cnt, length);
    }

    void freeThisReference()
    {
        auto count = refCount;
        if(count != 1) // have more references to this memory
        {
            // dec shared ref-count
            refCount = count - 1;
            data = [];
        }
        else
            SP.destroy(data);
        assert(!data.ptr);
    }

    void dupThisReference(uint count)
    in
    {
        assert(!empty && count != 1 && count == refCount);
    }
    body
    {
        //dec shared ref-count
        refCount = count - 1;
        //copy to the new chunk of RAM
        auto new_data = SP.alloc!ubyte(data.length);
        //bit-blit old stuff except the counter
        copy(data[0..$-4], new_data[0..$-4]);
        data = new_data; // before setting refCount!
        refCount = 1; // so that this updates the right one
    }

    ubyte[] data;
}

@trusted unittest// Uint24 tests //@@@BUG@@ iota is system ?!
{
    void funcRef(T)(ref T u24)
    {        
        u24.length = 2;
        u24[1] = 1024; 
        T u24_c = u24;
        assert(u24[1] == 1024);
        u24.length = 0;
        assert(u24.empty);
        u24.append([1, 2]);
        assert(equal(u24[], [1, 2]));
        u24.append(111);
        assert(equal(u24[], [1, 2, 111]));
        assert(!u24_c.empty && u24_c[1] == 1024);
        u24.length = 3;
        copy(iota(0, 3), u24[]);
        assert(equal(u24[], iota(0, 3)));
        assert(u24_c[1] == 1024);
    }

    void func2(T)(T u24)
    {
        T u24_2 = u24;
        T u24_3;        
        u24_3 = u24_2;
        assert(u24_2 == u24_3);        
        assert(equal(u24[], u24_2[]));
        assert(equal(u24_2[], u24_3[]));
        funcRef(u24_3);

        assert(equal(u24_3[], iota(0, 3)));
        assert(!equal(u24_2[], u24_3[]));
        assert(equal(u24_2[], u24[]));
        u24_2 = u24_3;
        assert(equal(u24_2[], iota(0, 3)));
        // to test that passed arg is intact outside
        // plus try out opEquals
        u24 = u24_3;
        u24 = T.init;
        u24_3 = T.init;
        assert(u24.empty);
        assert(u24 == u24_3);
        assert(u24 != u24_2);
    }

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
        U24A arr2 = arr;
        assert(arr2[0] == 72);
        arr2[0] = 11;
        //test COW-ness
        assert(arr[0] == 72);
        assert(arr2[0] == 11);
        //set this to about 100M to stress-test COW memory management
        foreach(v; 0..10_000) 
            func2(arr);
        assert(equal(arr[], [72, 0xFE_FEFE, 100]));

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

@trusted unittest// core set primitives test
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

        // simple unions, mostly edge effects
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
        a.add(0, 5); // prepand
        assert(a == CodeList(0, 5, 10, 20, 40, 60), text(a));

        a = x;
        a.add(5, 20);
        assert(a == CodeList(5, 20, 40, 60));

        a = x;
        a.add(3, 37);
        assert(a == CodeList(3, 37, 40, 60));

        a = x;
        a.add(37, 65);
        assert(a == CodeList(10, 20, 37, 65));

        // some tests on helpers for set intersection
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
{   // full set operations
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

unittest// iteration & opIndex
{
    import std.typecons;
    foreach(CodeList; TypeTuple!(InversionList!(ReallocPolicy)))
    {
        auto arr = "ABCDEFGHIJKLMabcdefghijklm"d;
        auto a = CodeList('A','N','a', 'n');
        assert(equal(a.byInterval, 
                [tuple(cast(uint)'A', cast(uint)'N'), tuple(cast(uint)'a', cast(uint)'n')]
            ), text(a.byInterval));
        
        //same @@@BUG as in issue 8949 ?
        version(bug8949)
        {
            assert(equal(retro(a.byInterval), 
                [tuple(cast(uint)'a', cast(uint)'n'), tuple(cast(uint)'A', cast(uint)'N')]
            ), text(retro(a.byInterval)));  
        }  
        auto achr = a.byCodepoint;
        assert(equal(achr, arr), text(a.byCodepoint));
        foreach(ch; a.byCodepoint)
            assert(a[ch]);
        auto x = CodeList(100, 500, 600, 900, 1200, 1500);
        assert(equal(x.byInterval, [ tuple(100, 500), tuple(600, 900), tuple(1200, 1500)]), text(x.byInterval));
        foreach(ch; x.byCodepoint)
            assert(x[ch]);
        static if(is(CodeList == CodepointSet))
        {
            auto y = CodeList(x.byInterval);
            assert(equal(x.byInterval, y.byInterval));
        }
        assert(equal(CodepointSet.init.byInterval, cast(Tuple!(uint, uint)[])[]));
        assert(equal(CodepointSet.init.byCodepoint, cast(dchar[])[]));
    }
}

//============================================================================
// Generic Trie template and various ways to build it
//============================================================================

// debug helper to get a shortened array dump
auto arrayRepr(T)(T x)
{
    if(x.length > 32)
    {
        return text(x[0..16],"~...~", x[x.length-16..x.length]);
    }
    else
        return text(x);
}

/**
    Maps $(D Key) to a suitable integer index within the range of $(D size_t).
    The mapping is constructed by applying predicates from $(D Prefix) left to right
    and concatenating the resulting bits. 

    The first (leftmost) predicate defines the most significant bits of 
    the resulting index.
 */
template mapTrieIndex(Prefix...)
{
    size_t mapTrieIndex(Key)(Key key)
        if(isValidPrefixForTrie!(Key, Prefix))
    {
        alias Prefix p;
        size_t idx;
        foreach(i, v; p[0..$-1])
        {
            idx |= p[i](key);
            idx <<= p[i+1].bitSize;
        }
        idx |= p[$-1](key);
        return idx;
    }
}

/*
    $(D TrieBuilder) is a type used for incremental construction
    of $(LREF Trie)s. 

    See $(LREF buildTrie) for generic helpers built on top of it.
*/
@trusted struct TrieBuilder(Value, Key, Args...)
    if(isBitPackableType!Value && isValidArgsForTrie!(Key, Args))
{
private:
    // last index is not stored in table, it is used as an offset to values in a block.
    static if(is(Value == bool))// always pack bool
        alias V = BitPacked!(Value, 1);
    else
        alias V = Value;
    static auto deduceMaxIndex(Preds...)()
    {
        size_t idx = 1;
        foreach(v; Preds)
            idx *= 2^^v.bitSize;
        return idx;
    }

    static if(is(typeof(Args[0]) : Key)) // Args start with upper bound on Key
    {
        alias Prefix = Args[1..$];
        enum lastPageSize = 2^^Prefix[$-1].bitSize;
        enum translatedMaxIndex = mapTrieIndex!(Prefix)(Args[0]);
        enum roughedMaxIndex = 
            (translatedMaxIndex + lastPageSize-1)/lastPageSize*lastPageSize;
        //check warp around - if wrapped, use the default deduction rule
        enum maxIndex = roughedMaxIndex < translatedMaxIndex ? 
            deduceMaxIndex!(Prefix) : roughedMaxIndex;
    }
    else
    {
        alias Prefix = Args;
        enum maxIndex = deduceMaxIndex!(Prefix);
    }
    
    alias getIndex = mapTrieIndex!(Prefix);

    enum lastLevel = Prefix.length-1;
    struct ConstructState
    {
        bool zeros, ones; // current page is zeros? ones?
        uint idx_zeros, idx_ones;
    }
    // iteration over levels of Trie, each indexes its own level and thus a shortened domain
    size_t[Prefix.length] indices;
    // default filler value to use
    Value defValue; 
    // this is a full-width index of next item
    size_t curIndex; 
    // all-zeros page index, all-ones page index (+ indicator if there is such a page)
    ConstructState[Prefix.length] state;
    // the table being constructed 
    MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), V) table;   

    @disable this();

    // this function assumes no holes in the input so 
    // indices are going one by one
    void addValue(size_t level, T)(T val, size_t numVals)
    {
        enum pageSize = 1<<Prefix[level].bitSize;
        if(numVals == 0)
            return;
        do
        {
            // need to take pointer again, memory block may move on resize
            auto ptr = table.slice!(level);
            static if(is(T : bool))
            {
                if(val)
                    state[level].zeros = false;
                else
                    state[level].ones = false;
            }
            if(numVals == 1)
            {
                static if(level == Prefix.length-1)
                    ptr[indices[level]] = val;
                else{// can incur narrowing conversion
                    assert(indices[level] < ptr.length);
                    ptr[indices[level]] = force!(typeof(ptr[indices[level]]))(val);
                }
                indices[level]++;
                numVals = 0;                
            }
            else
            {
                // where is the next page boundary
                size_t nextPB = (indices[level]+pageSize)/pageSize*pageSize;
                size_t j = indices[level];
                size_t n =  nextPB-j;// can fill right in this page
                if(numVals > n)
                    numVals -= n;
                else
                {
                    n = numVals;
                    numVals = 0;
                }
                static if(level < Prefix.length-1)
                    assert(indices[level] <= 2^^Prefix[level+1].bitSize);                
                ptr[j..j+n]  = val;
                j += n;
                indices[level] = j;
            }
            // last level (i.e. topmost) has 1 "page" 
            // thus it need not to add a new page on upper level
            static if(level != 0)
            {
                if(indices[level] % pageSize == 0)
                    spillToNextPage!level(ptr); 
            }
        }
        while(numVals);
    }

    // this can re-use the current page if duplicate or allocate a new one
    // it also makes sure that previous levels point to the correct page in this level
    void spillToNextPage(size_t level, Slice)(ref Slice ptr)
    {
        alias typeof(table.slice!(level-1)[0]) NextIdx;
        NextIdx next_lvl_index;
        enum pageSize = 1<<Prefix[level].bitSize;
        static if(is(T : bool))
        {
            if(state[level].zeros)
            {
                if(state[level].idx_empty == uint.max)
                {
                    state[level].idx_empty = cast(uint)(indices[level]/pageSize - 1);
                    goto L_allocate_page;
                }
                else
                {
                    next_lvl_index = force!NextIdx(state[level].idx_empty);
                    indices[level] -= pageSize;// it is a duplicate
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
                // get index to it, reuse ptr space for the next block
                next_lvl_index = force!NextIdx(j/pageSize);
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
                indices[level] -= pageSize; // reuse this page, it is duplicate
                break;
            }
        }
        if(j == last)
        {                            
    L_allocate_page:    
            next_lvl_index = force!NextIdx(indices[level]/pageSize - 1);                        
            // allocate next page
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
        // reset all zero/ones tracking variables
        static if(is(TypeOfBitPacked!T : bool))
        {
            state[level].zeros = true;
            state[level].ones = true;
        }
        // for the previous level, values are indices to the pages in the current level
        addValue!(level-1)(next_lvl_index, 1);
    }

    // idx - full-width index to fill with v (full-width index != key)
    // fills everything in the range of [curIndex, idx) with filler
    void putAt(size_t idx, Value v)
    {
        assert(idx >= curIndex);
        size_t numFillers = idx - curIndex;
        addValue!lastLevel(defValue, numFillers);
        addValue!lastLevel(v, 1);
        curIndex = idx + 1;
    }

    // ditto, but sets the range of [idxA, idxB) to v
    void putRangeAt(size_t idxA, size_t idxB, Value v)
    {
        assert(idxA >= curIndex);
        assert(idxB >= idxA);
        size_t numFillers = idxA - curIndex;
        addValue!lastLevel(defValue, numFillers);
        addValue!lastLevel(v, idxB - idxA);
        curIndex = idxB; //open-right
    }

    enum errMsg = "non-monotonic prefix function(s), an unsorted range or "
        "duplicate key->value mapping";

public:
    /**
        Construct a builder, where $(D filler) is a value 
        to indicate empty slots (or "not found" condition).
    */
    this(Value filler) 
    {
        curIndex = 0;
        defValue = filler;
        // zeros-page index, ones-page index
        foreach(ref v; state)
            v = ConstructState(true, true, uint.max, uint.max);
        table = typeof(table)(indices);
        // one page per level is a bootstrap minimum
        foreach(i; Sequence!(0, Prefix.length))
            table.length!i = (1<<Prefix[i].bitSize);
    }

    /**
        Put a value $(D v) into interval as 
        mapped by keys from $(D a) to $(D b).
        All slots prior to $(D a) are filled with 
        the default filler.
    */
    void putRange(Key a, Key b, Value v)
    {
        auto idxA = getIndex(a), idxB = getIndex(b);
        // indexes of key should always grow
        enforce(idxB >= idxA && idxA >= curIndex, errMsg);
        putRangeAt(idxA, idxB, v);
    }

    /**
        Put a value $(D v) into slot mapped by $(D key).
        All slots prior to $(D key) are filled with the
        default filler.
    */
    void putValue(Key key, Value v)
    {
        auto idx = getIndex(key);
        enforce(idx >= curIndex, text(errMsg, " ", idx));
        putAt(idx, v);
    }

    /// Finishes construction of Trie, yielding an immutable Trie instance.
    auto build()
    {
        static if(maxIndex != 0) // doesn't cover full range of size_t
        {
            assert(curIndex <= maxIndex);
            addValue!lastLevel(defValue, maxIndex - curIndex);
        }
        else
        {
            if(curIndex != 0 //couldn't wrap around
                || (Prefix.length != 1 && indices[lastLevel] == 0)) //can be just empty 
            {
                addValue!lastLevel(defValue, size_t.max - curIndex);
                addValue!lastLevel(defValue, 1);
            }
            //else curIndex already completed the full range of size_t by wrapping around
        }
        return Trie!(V, Key, maxIndex, Prefix)(table);
    }
}

/*
    $(P A generic Trie data-structure for a fixed number of stages.
    The design goal is optimal speed with smallest footprint size.
    )
    $(P It's intentionally read-only and doesn't provide constructors.
     To construct one use a special builder, 
     see $(LREF TrieBuilder) and $(LREF buildTrie).
    )

*/
@trusted public struct Trie(Value, Key, Args...)
    if(isValidPrefixForTrie!(Key, Args)
        || (isValidPrefixForTrie!(Key, Args[1..$]) 
            && is(typeof(Args[0]) : size_t)))
{
    static if(is(typeof(Args[0]) : size_t))
    {
        enum maxIndex = Args[0];
        enum hasBoundsCheck = true;
        alias Prefix = Args[1..$];
    }
    else
    {
        enum hasBoundsCheck = false;
        alias Prefix = Args;
    }

    private this()(typeof(_table) table)
    {
        _table = table;
    }

    // only for constant Tries constructed from precompiled tables
    private this()(const(size_t)[] offsets, const(size_t)[] sizes, 
        const(size_t)[] data) const
    {
        _table = typeof(_table)(offsets, sizes, data);
    }

    /*
        $(P Lookup the $(D key) in this $(D Trie). )

        $(P The lookup always succeeds if key fits the domain 
        provided during construction. The whole domain defined 
        is covered so instead of not found condition 
        the sentinel (filler) value could be used. )

        $(P See $(LREF buildTrie), $(LREF TrieBuilder) for how to 
        define a domain of $(D Trie) keys and the sentinel value. )

        Note:
        Domain range-checking is only enabled in debug builds
        and results in assertion failure.
    */
    //templated to auto-detect pure, @safe and nothrow
    TypeOfBitPacked!Value opIndex()(Key key) const
    {
        static if(hasBoundsCheck)
            assert(mapTrieIndex!Prefix(key) < maxIndex);
        size_t idx;
        alias p = Prefix;
        idx = cast(size_t)p[0](key);
        foreach(i, v; p[0..$-1])
            idx = cast(size_t)((_table.slice!i[idx]<<p[i+1].bitSize) + p[i+1](key));
        auto val = _table.slice!(p.length-1)[idx];
        return val;
    }

    @property size_t bytes(size_t n=size_t.max)() const
    {
        return _table.bytes!n;
    }

    @property size_t pages(size_t n)() const
    {
        return (bytes!n+2^^(Prefix[n].bitSize-1))
                /2^^Prefix[n].bitSize;
    }

    void store(OutRange)(scope OutRange sink) const
        if(isOutputRange!(OutRange, char))
    {
        _table.store(sink);
    }

private:
    MultiArray!(idxTypes!(Key, fullBitSize!(Prefix), Prefix[0..$]), Value) _table;
}

// create a tuple of 'sliceBits' that slice the 'top' of bits into pieces of sizes 'sizes'
// left-to-right, the most significant bits first
template GetBitSlicing(size_t top, sizes...)
{
    static if(sizes.length > 0)
        alias TypeTuple!(sliceBits!(top - sizes[0], top)
            , GetBitSlicing!(top - sizes[0], sizes[1..$])) GetBitSlicing;
    else
        alias TypeTuple!()  GetBitSlicing;
}

template callableWith(T)
{
    template callableWith(alias Pred)
    {
        static if(!is(typeof(Pred(T.init))))
            enum callableWith = false;
        else
        {
            alias Result = typeof(Pred(T.init));
            enum callableWith = isBitPackableType!(TypeOfBitPacked!(Result));
        }
    }
}

/*
    Check if $(D Prefix) is a valid set of predicates 
    for $(D Trie) template having $(D Key) as the type of keys.
    This requires all predicates to be callable, take 
    single argument of type $(D Key) and return unsigned value.
*/
template isValidPrefixForTrie(Key, Prefix...)
{
    enum isValidPrefixForTrie = allSatisfy!(callableWith!Key, Prefix); //TODO: tighten the screws
}

/*
    Check if $(D Args) is a set of maximum key value followed by valid predicates 
    for $(D Trie) template having $(D Key) as the type of keys.
*/
template isValidArgsForTrie(Key, Args...)
{
    static if(Args.length > 1)
    {
        enum isValidArgsForTrie = isValidPrefixForTrie!(Key, Args)
            || (isValidPrefixForTrie!(Key, Args[1..$]) && is(typeof(Args[0]) : Key));
    }
    else
        enum isValidArgsForTrie = isValidPrefixForTrie!Args;
}

@property size_t sumOfIntegerTuple(ints...)()
{
    size_t count=0;
    foreach(v; ints)
        count += v;
    return count;
}

/**
    A shorthand for creating a custom multi-level fixed Trie 
    from a $(D CodepointSet). $(D sizes) are numbers of bits per level, 
    with the most significant bits used first.    

    Note: The sum of $(D sizes) must be equal 21.

    See also even simpler $(LREF toTrie).
    
    Example:
    ---
    {
        import std.stdio;
        auto set = unicode("Number");
        auto trie = codepointSetTrie!(8, 5, 8)(set);
        writeln("Input code points to test:");
        foreach(line; stdin.byLine)
        {
            int count=0;
            foreach(dchar ch; line)
                if(trie[ch])// is number
                    count++;
            writefln("Contains %d number code points.", count);
        }
    }
    ---
*/
public template codepointSetTrie(sizes...)
    if(sumOfIntegerTuple!sizes == 21)
{
    auto codepointSetTrie(Set)(Set set)
        if(isCodepointSet!Set)
    {
        auto builder = TrieBuilder!(bool, dchar, lastDchar+1, GetBitSlicing!(21, sizes))(false);
        foreach(ival; set.byInterval)
            builder.putRange(ival[0], ival[1], true);
        return builder.build();
    }
}

/// Type of Trie generated by codepointSetTrie function.
public template CodepointSetTrie(sizes...)
    if(sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);
    alias CodepointSetTrie = typeof(TrieBuilder!(bool, dchar, lastDchar+1, Prefix)(false).build());
}

/**
    A slightly more general tool for building fixed $(D Trie)
    for the Unicode data.

    Specifically unlike $(D codepointSetTrie) it's allows creating mappings 
    of $(D dchar) to an arbitrary type $(D T).

    Note: Overload taking $(D CodepointSet)s will naturally convert 
    only to bool mapping $(D Trie)s.

    Example:
    ---    
    // pick characters from the Greek script
    auto set = unicode.Greek; 
        
    // a user-defined property (or an expensive function) 
    // that we want to look up
    static uint luckFactor(dchar ch)
    {
        // here we consider a character lucky  
        // if its code point has a lot of identical hex-digits
        // e.g. arabic letter DDAL (\u0688) has a "luck factor" of 2
        ubyte[6] nibbles; //6 4-bit chunks of code point
        uint value = ch;
        foreach(i; 0..6)
        {
            nibbles[i] = value & 0xF;
            value >>= 4;
        }
        uint luck;
        foreach(n; nibbles)
            luck = max(luck, count(nibbles[], n));
        return luck;
    }

    // only unsigned built-ins are supported at the moment
    alias LuckFactor = BitPacked!(uint, 3);

    // create a temporary associative array (AA)
    LuckFactor[dchar] map;
    foreach(ch; set.byCodepoint)
        map[ch] = luckFactor(ch);

    // bits per stage are chosen randomly, fell free to optimize
    auto trie = codepointTrie!(LuckFactor, 8, 5, 8)(map);

    // from now on the AA is not needed
    foreach(ch; set.byCodepoint)
        assert(trie[ch] == luckFactor(ch)); //verify
    // CJK is not Greek, thus it has the default value
    assert(trie['\u4444'] == 0);
    // and here is a couple of quite lucky Greek characters:
    // Greek small letter epsilon with dasia
    assert(trie['\u1F11'] == 3); 
    // Ancient Greek metretes sign
    assert(trie['\U00010181'] == 3);   
    ---
*/
public template codepointTrie(T, sizes...)
    if(sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);

    static if(is(TypeOfBitPacked!T == bool))
    {
        auto codepointTrie(Set)(in Set set)
            if(isCodepointSet!Set)
        {
            return codepointSetTrie(set);        
        }
    }

    auto codepointTrie()(T[dchar] map, T defValue=T.init)
    {
        return buildTrie!(T, dchar, Prefix)(map, defValue);
    }
}

unittest //codepointTrie example
{
    // pick characters from the Greek script
    auto set = unicode.Greek; 
        
    // a user-defined property (or an expensive function) 
    // that we want to look up
    static uint luckFactor(dchar ch)
    {
        // here we consider a character lucky  
        // if its code point has a lot of identical hex-digits
        // e.g. arabic letter DDAL (\u0688) has a "luck factor" of 2
        ubyte[6] nibbles; //6 4-bit chunks of code point
        uint value = ch;
        foreach(i; 0..6)
        {
            nibbles[i] = value & 0xF;
            value >>= 4;
        }
        uint luck;
        foreach(n; nibbles)
            luck = max(luck, count(nibbles[], n));
        return luck;
    }

    // only unsigned built-ins are supported at the moment
    alias LuckFactor = BitPacked!(uint, 3);

    // create a temporary associative array (AA)
    LuckFactor[dchar] map;
    foreach(ch; set.byCodepoint)
        map[ch] = luckFactor(ch);

    // bits per stage are chosen randomly, fell free to optimize
    auto trie = codepointTrie!(LuckFactor, 8, 5, 8)(map);

    // from now on the AA is not needed
    foreach(ch; set.byCodepoint)
        assert(trie[ch] == luckFactor(ch)); //verify
    // CJK is not Greek, thus it has the default value
    assert(trie['\u4444'] == 0);
    // and here is a couple of quite lucky Greek characters:
    // Greek small letter epsilon with dasia
    assert(trie['\u1F11'] == 3); 
    // Ancient Greek metretes sign
    assert(trie['\U00010181'] == 3);

}

/// Type of Trie as generated by codepointTrie function.
public template CodepointTrie(T, sizes...)
    if(sumOfIntegerTuple!sizes == 21)
{
    alias Prefix = GetBitSlicing!(21, sizes);
    alias CodepointTrie = typeof(TrieBuilder!(T, dchar, lastDchar+1, Prefix)(T.init).build());
}

// @@@BUG multiSort can's access private symbols from uni
public template cmpK0(alias Pred)
{
    static bool cmpK0(Value, Key)
        (Tuple!(Value, Key) a, Tuple!(Value, Key) b)
    {
        return Pred(a[1]) < Pred(b[1]);
    }
}

/*
    The most general utility for construction of $(D Trie)s 
    short of using $(D TrieBuilder) directly.

    Provides a number of convenience overloads.
    $(D Args) is tuple of maximum key value followed by 
    predicates to construct index from key.

    Alternatively if the first argument is not a value convertible to $(D Key) 
    then the whole tuple of $(D Args) is treated as predicates 
    and the maximum Key is deduced from predicates.
*/
public template buildTrie(Value, Key, Args...)
    if(isValidArgsForTrie!(Key, Args))
{
    static if(is(typeof(Args[0]) : Key)) //prefix starts with upper bound on Key
    {
        alias Prefix = Args[1..$];
    }
    else
        alias Prefix = Args;
    
    alias getIndex = mapTrieIndex!(Prefix);

    // for multi-sort
    template GetComparators(size_t n)
    {
        static if(n > 0)
            alias GetComparators = 
                TypeTuple!(GetComparators!(n-1), cmpK0!(Prefix[n-1]));
        else
            alias GetComparators = TypeTuple!();
    }

    /*
        Build $(D Trie) from a range of a Key-Value pairs,
        assuming it is sorted by Key as defined by the following lambda:
        ------
        (a, b) => mapTrieIndex!(Prefix)(a) < mapTrieIndex!(Prefix)(b)
        ------
        Exception is thrown if it's detected that the above order doesn't hold.

        In other words $(LREF mapTrieIndex) should be a 
        monotonically increasing function that maps $(D Key) to an integer.

        See also: $(XREF _algorithm, sort), 
        $(XREF _range, SortedRange),
        $(XREF _algorithm, setUnion).
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
        if(isInputRange!Range && is(typeof(Range.init.front[0]) : Value) 
            && is(typeof(Range.init.front[1]) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, maxIndex, Prefix)(filler);
        foreach(v; range)
            builder.putValue(v[1], v[0]);
        return builder.build();
    }

    /*
        If $(D Value) is bool (or BitPacked!(bool, x)) then it's possible 
        to build $(D Trie) from a range of open-right intervals of ($D Key)s.
        The requirement  on the ordering of keys (and the behavior on the 
        violation of it) is the same as for Key-Value range overload.

        Intervals denote ranges of !$(D filler) i.e. the opposite of filler.
        If no filler provided keys inside of the intervals map to true, 
        and $(D filler) is false.
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
        if(is(TypeOfBitPacked!Value ==  bool) 
            && isInputRange!Range && is(typeof(Range.init.front[0]) : Key) 
            && is(typeof(Range.init.front[1]) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach(ival; range)
            builder.putRange(ival[0], ival[1], !filler);
        return builder.build();
    }

    /*
        If $(D Value) is bool (or BitPacked!(bool, x)) then it's possible 
        to build $(D Trie) simply from an input range of $(D Key)s.
        The requirement  on the ordering of keys (and the behavior on the 
        violation of it) is the same as for Key-Value range overload.

        Keys found in range denote !$(D filler) i.e. the opposite of filler.
        If no filler provided keys map to true, and $(D filler) is false.
    */
    auto buildTrie(Range)(Range range, Value filler=Value.init)
        if(is(TypeOfBitPacked!Value ==  bool) 
            && isInputRange!Range && is(typeof(Range.init.front) : Key))
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach(v; range)
            builder.putValue(v, !filler);
        return builder.build();
    }

    /*
        If $(D Key) is unsigned integer $(D Trie) could be constructed from array
        of values where array index serves as key.
    */
    auto buildTrie()(Value[] array, Value filler=Value.init)
        if(isUnsigned!Key)
    {
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach(idx, v; array)
            builder.putValue(idx, v);
        return builder.build();
    }

    /*
        Builds $(D Trie) from associative array.
    */
    auto buildTrie(Key, Value)(Value[Key] map, Value filler)
    {
        auto range = array(zip(map.values, map.keys));
        alias Comps = GetComparators!(Prefix.length);
        multiSort!(Comps)(range);
        auto builder = TrieBuilder!(Value, Key, Prefix)(filler);
        foreach(v; range)
            builder.putValue(v[1], v[0]);
        return builder.build();
    }
}

/++
    Convenience function to construct optimal configurations for 
    packed Trie from any $(D set) of $(CODEPOINTS).
    
    The parameter $(D level) indicates the number of trie levels to use, 
    allowed values are: 1, 2, 3 or 4. Levels represent different trade-offs 
    speed-size wise.
    
    $(P Level 1 is fastest and the most memory hungry (a bit array). )
    $(P Level 4 is the slowest and has the smallest footprint. )

    See the $(S_LINK Synopsis, Synopsis) section for example.

    Note:
    Level 4 stays very practical (being faster and more predictable)
    compared to using direct lookup on the $(D set) itself.


+/
public auto toTrie(size_t level, Set)(Set set)
    if(isCodepointSet!Set)
{
    static if(level == 1)
        return codepointSetTrie!(21)(set);
    else static if(level == 2)
        return codepointSetTrie!(10, 11)(set);
    else static if(level == 3)
        return codepointSetTrie!(8, 5, 8)(set);
    else static if(level == 4)
         return codepointSetTrie!(6, 4, 4, 7)(set);
    else
        static assert(false, 
            "Sorry, toTrie doesn't support levels > 4, use codepointSetTrie directly");
}

/**
    $(P Builds $(D Trie) with typically optimal speed-size trade-off
    and wraps it into a delegate of the following type:
    $(D delegate bool (dchar ch)). )

    $(P Effectively this creates a 'tester' lambda suitable 
    for algorithms like std.algorithm.find that take unary predicates. )

    See the $(S_LINK Synopsis, Synopsis) section for example.
*/
public auto toDelegate(Set)(Set set)
    if(isCodepointSet!Set)
{
    //3 is very small and is almost as fast as 2-level (due to CPU caches?)
    auto t = toTrie!3(set); 
    return (dchar ch) => t[ch];
}

/**
    $(P Opaque wrapper around unsigned built-in integers and 
    code unit (char/wchar/dchar) types.
    Parameter $(D sz) indicates that the value is confined 
    to the range of [0, 2^^sz$(RPAREN). With this knowledge it can be 
    packed more tightly when stored in certain 
    data-structures like trie. )

    Note:
    $(P The $(D BitPacked!(T, sz)) is implicitly convertible to $(D T)
    but not vise-versa. Users have to ensure the value fits in 
    the range required and use the $(D cast) 
    operator to perform the conversion.)
*/
struct BitPacked(T, size_t sz) 
    if(isIntegral!T || is(T:dchar))
{
    enum bitSize = sz;
    T _value;
    alias _value this;
}

/*
    Depending on the form of the passed argument $(D bitSizeOf) returns 
    the amount of bits required to represent a given type 
    or a return type of a given functor.
*/
template bitSizeOf(Args...)
    if(Args.length == 1)
{
    alias T = Args[0];
    static if(is(typeof(T.bitSize) : size_t))
    {
        enum bitSizeOf = T.bitSize;
    }
    else static if(is(ReturnType!T dummy == BitPacked!(U, bits), U, size_t bits))
    {
        enum bitSizeOf = bitSizeOf!(ReturnType!T);
    }
    else
    {
        enum bitSizeOf = T.sizeof*8;
    }
}

/**
    Tests if $(D T) is some instantiation of $(LREF BitPacked)!(U, x) 
    and thus suitable for packing.
*/
template isBitPacked(T)
{
    static if(is(T dummy == BitPacked!(U, bits), U, size_t bits))    
        enum isBitPacked = true;
    else
        enum isBitPacked = false;
}

/**
    Gives the type $(D U) from $(LREF BitPacked)!(U, x) 
    or $(D T) itself for every other type.
*/
template TypeOfBitPacked(T)
{
    static if(is(T dummy == BitPacked!(U, bits), U, size_t bits))    
        alias TypeOfBitPacked = U;
    else
        alias TypeOfBitPacked = T;   
}

/*
    Wrapper, used in definition of custom data structures from $(D Trie) template.
    Applying it to a unary lambda function indicates that the returned value always
    fits within $(D bits) of bits.
*/
public struct assumeSize(alias Fn, size_t bits)
{
    enum bitSize = bits;
    static auto ref opCall(T)(auto ref T arg)
    {
        return Fn(arg);
    }
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

/*
    A helper for defining lambda function that yields a slice 
    of certain bits from an unsigned integral value.
    The resulting lambda is wrapped in assumeSize and can be used directly 
    with $(D Trie) template.
*/
public template sliceBits(size_t from, size_t to)
{
    alias assumeSize!(sliceBitsImpl!(from, to), to-from) sliceBits;
}

uint low_8(uint x) { return x&0xFF; }
@safe pure nothrow uint midlow_8(uint x){ return (x&0xFF00)>>8; }
alias assumeSize!(low_8, 8) lo8;
alias assumeSize!(midlow_8, 8) mlo8;

static assert(bitSizeOf!lo8 == 8);
static assert(bitSizeOf!(sliceBits!(4, 7)) == 3);
static assert(bitSizeOf!(BitPacked!(uint, 2)) == 2);

template Sequence(size_t start, size_t end)
{
    static if(start < end)
        alias TypeTuple!(start, Sequence!(start+1, end)) Sequence;
    else
        alias TypeTuple!() Sequence;
}

//---- TRIE TESTS ----
unittest
{
    static trieStats(TRIE)(TRIE t)
    {
        version(std_uni_stats)
        {
            import std.stdio;
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
    // alias assumeSize!(8, function (uint x) { return x&0xFF; }) lo8;
    // alias assumeSize!(7, function (uint x) { return (x&0x7F00)>>8; }) next8;
    alias CodepointSet Set;
    auto set = Set('A','Z','a','z');
    auto trie = buildTrie!(bool, uint, 256, lo8)(set.byInterval);// simple bool array
    for(int a='a'; a<'z';a++)
        assert(trie[a]);
    for(int a='A'; a<'Z';a++)
        assert(trie[a]);
    for(int a=0; a<'A'; a++)
        assert(!trie[a]);
    for(int a ='Z'; a<'a'; a++)
        assert(!trie[a]);
    trieStats(trie);

    auto redundant2 = Set(
        1, 18, 256+2, 256+111, 512+1, 512+18, 768+2, 768+111);
    auto trie2 = buildTrie!(bool, uint, 1024, mlo8, lo8)(redundant2.byInterval);
    trieStats(trie2);
    foreach(e; redundant2.byCodepoint)
        assert(trie2[e], text(cast(uint)e, " - ", trie2[e]));
    foreach(i; 0..1024)
    {
        assert(trie2[i] == (i in redundant2));
    }


    auto redundant3 = Set(
          2,    4,    6,    8,    16,
       2+16, 4+16, 16+6, 16+8, 16+16,
       2+32, 4+32, 32+6, 32+8,
      );

    enum max3 = 256;
    // sliceBits
    auto trie3 = buildTrie!(bool, uint, max3, 
            sliceBits!(6,8), sliceBits!(4,6), sliceBits!(0,4)
        )(redundant3.byInterval);
    trieStats(trie3);
    foreach(i; 0..max3)
        assert(trie3[i] == (i in redundant3), text(cast(uint)i));

    auto redundant4 = Set(
            10, 64, 64+10, 128, 128+10, 256, 256+10, 512,
            1000, 2000, 3000, 4000, 5000, 6000
        );
    enum max4 = 2^^16;
    auto trie4 = buildTrie!(bool, size_t, max4,
            sliceBits!(13, 16), sliceBits!(9, 13), sliceBits!(6, 9) , sliceBits!(0, 6)
        )(redundant4.byInterval);
    foreach(i; 0..max4){        
        if(i in redundant4)
            assert(trie4[i], text(cast(uint)i));
    }
    trieStats(trie4);

    alias mapToS = mapTrieIndex!(useItemAt!(0, char));
    string[] redundantS = ["tea", "start", "orange"];
    redundantS.sort!((a,b) => mapToS(a) < mapToS(b));
    auto strie = buildTrie!(bool, string, useItemAt!(0, char))(redundantS);
    // using first char only
    assert(redundantS == ["orange", "start", "tea"]);
    assert(strie["test"], text(strie["test"]));
    assert(!strie["aea"]);
    assert(strie["s"]);

    // a bit size test
    auto a = array(map!(x => to!ubyte(x))(iota(0, 256)));
    auto bt = buildTrie!(bool, ubyte, sliceBits!(7, 8), sliceBits!(5, 7), sliceBits!(0, 5))(a);
    trieStats(bt);
    foreach(i; 0..256)
        assert(bt[cast(ubyte)i]);
}

template useItemAt(size_t idx, T)
    if(isIntegral!T || is(T: dchar))
{
    size_t impl(in T[] arr){ return arr[idx]; }
    alias useItemAt = assumeSize!(impl, 8*T.sizeof);
}

template useLastItem(T)
{
    size_t impl(in T[] arr){ return arr[$-1]; }
    alias useLastItem = assumeSize!(impl, 8*T.sizeof);
}

template fullBitSize(Prefix...)
{
    static if(Prefix.length > 0)
        enum fullBitSize = bitSizeOf!(Prefix[0])+fullBitSize!(Prefix[1..$]);
    else
        enum fullBitSize = 0;
}

template idxTypes(Key, size_t fullBits, Prefix...)
{
    static if(Prefix.length == 1)
    {// the last level is value level, so no index once reduced to 1-level
        alias TypeTuple!() idxTypes;
    }
    else
    {
        // Important note on bit packing
        // Each level has to hold enough of bits to address the next one    
        // The bottom level is known to hold full bit width
        // thus it's size in pages is full_bit_width - size_of_last_prefix
        // Recourse on this notion
        alias TypeTuple!(
            idxTypes!(Key, fullBits - bitSizeOf!(Prefix[$-1]), Prefix[0..$-1]),
            BitPacked!(typeof(Prefix[$-2](Key.init)), fullBits - bitSizeOf!(Prefix[$-1]))
        ) idxTypes;
    }
}

//============================================================================

@trusted int comparePropertyName(Char1, Char2)(const(Char1)[] a, const(Char2)[] b)
{
    alias low = std.ascii.toLower;
    return cmp(a.map!(x => low(x)).filter!(x => !isWhite(x) && x != '-' && x != '_'),
        b.map!(x => low(x)).filter!(x => !isWhite(x) && x != '-' && x != '_'));
    /*for(;;)
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
    }*/
}

bool propertyNameLess(Char1, Char2)(const(Char1)[] a, const(Char2)[] b)
{
    return comparePropertyName(a, b) < 0;
}

//============================================================================
// Utilities for compression of Unicode code point sets
//============================================================================

@safe void compressTo(uint val, ref ubyte[] arr) pure nothrow
{
    // not optimized as usually done 1 time (and not public interface)
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

@safe uint decompressFrom(const(ubyte)[] arr, ref size_t idx) pure
{
    uint first = arr[idx++];
    if(!(first & 0x80)) // no top bit -> [0..127]
        return first;
    uint extra = ((first>>5) & 1) + 1; // [1, 2]
    uint val = (first & 0x1F);
    enforce(idx + extra <= arr.length, "bad code point interval encoding");
    foreach(j; 0..extra)
        val = (val<<8) | arr[idx+j];
    idx += extra;
    return val;
}


public ubyte[] compressIntervals(Range)(Range intervals)
    if(isInputRange!Range && isIntegralPair!(ElementType!Range))
{
    ubyte[] storage;
    uint base = 0;
    // RLE encode
    foreach(val; intervals)
    {        
        compressTo(val[0]-base, storage);
        base = val[0];
        if(val[1] != lastDchar+1) // till the end of the domain so don't store it
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
    ubyte[] enc2 = [cast(ubyte)0, (0b1_01<<5) | (1<<4), 2, 1, 3]; // odd length-ed
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

// Creates a range of $(D CodepointInterval) that lazily decodes compressed data.
// TODO: make it package when pushed to std.
@safe public auto decompressIntervals(const(ubyte)[] data) 
{
    return DecompressedIntervals(data);
}

@trusted struct DecompressedIntervals
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
        if(_idx == _stream.length)// odd length ---> till the end
            _front[1] = lastDchar+1;
        else
        {
            base = _front[0];
            _front[1] = base + decompressFrom(_stream, _idx);
        }
    }

    @property bool empty() const
    {
        return _idx == size_t.max;
    }

    @property DecompressedIntervals save() { return this; } 
}

static assert(isInputRange!DecompressedIntervals);
static assert(isForwardRange!DecompressedIntervals);
//============================================================================

version(std_uni_bootstrap){}
else
{

// helper for looking up code point sets
@trusted ptrdiff_t findUnicodeSet(alias table, C)(in C[] name)
{
    auto range = assumeSorted!((a,b) => propertyNameLess(a,b))
        (table.map!"a.name");    
    size_t idx = range.lowerBound(name).length;
    if(idx < range.length && comparePropertyName(range[idx], name) == 0)
        return idx;
    return -1;
}

// another one that loads it
@trusted bool loadUnicodeSet(alias table, Set, C)(in C[] name, ref Set dest)
{
    auto idx = findUnicodeSet!table(name);
    if(idx >= 0)
    {
        dest = Set(asSet(table[idx].compressed));
        return true;
    }
    return false;
}

@trusted bool loadProperty(Set=CodepointSet, C)
    (in C[] name, ref Set target)
{        
    alias comparePropertyName ucmp;
    // conjure cumulative properties by hand
    if(ucmp(name, "L") == 0 || ucmp(name, "Letter") == 0)
    {
        target |= asSet(uniPropLu);
        target |= asSet(uniPropLl);
        target |= asSet(uniPropLt);
        target |= asSet(uniPropLo);
        target |= asSet(uniPropLm);
    }
    else if(ucmp(name,"LC") == 0 || ucmp(name,"Cased Letter")==0)
    {
        target |= asSet(uniPropLl);
        target |= asSet(uniPropLu);
        target |= asSet(uniPropLt);// Title case
    }
    else if(ucmp(name, "M") == 0 || ucmp(name, "Mark") == 0)
    {
        target |= asSet(uniPropMn);
        target |= asSet(uniPropMc);
        target |= asSet(uniPropMe);
    }
    else if(ucmp(name, "N") == 0 || ucmp(name, "Number") == 0)
    {
        target |= asSet(uniPropNd);
        target |= asSet(uniPropNl);
        target |= asSet(uniPropNo);
    }
    else if(ucmp(name, "P") == 0 || ucmp(name, "Punctuation") == 0)
    {
        target |= asSet(uniPropPc);
        target |= asSet(uniPropPd);
        target |= asSet(uniPropPs);
        target |= asSet(uniPropPe);
        target |= asSet(uniPropPi);
        target |= asSet(uniPropPf);
        target |= asSet(uniPropPo);
    }
    else if(ucmp(name, "S") == 0 || ucmp(name, "Symbol") == 0)
    {
        target |= asSet(uniPropSm);
        target |= asSet(uniPropSc);
        target |= asSet(uniPropSk);
        target |= asSet(uniPropSo);
    }
    else if(ucmp(name, "Z") == 0 || ucmp(name, "Separator") == 0)
    {
        target |= asSet(uniPropZs);
        target |= asSet(uniPropZl);
        target |= asSet(uniPropZp);
    }
    else if(ucmp(name, "C") == 0 || ucmp(name, "Other") == 0)
    {
        target |= asSet(uniPropCo);
        target |= asSet(uniPropLo);
        target |= asSet(uniPropNo);
        target |= asSet(uniPropSo);
        target |= asSet(uniPropPo);
    }
    else if(ucmp(name, "graphical") == 0){
        target |= asSet(uniPropAlphabetic);

        target |= asSet(uniPropMn);
        target |= asSet(uniPropMc);
        target |= asSet(uniPropMe);

        target |= asSet(uniPropNd);
        target |= asSet(uniPropNl);
        target |= asSet(uniPropNo);

        target |= asSet(uniPropPc);
        target |= asSet(uniPropPd);
        target |= asSet(uniPropPs);
        target |= asSet(uniPropPe);
        target |= asSet(uniPropPi);
        target |= asSet(uniPropPf);
        target |= asSet(uniPropPo);

        target |= asSet(uniPropZs);

        target |= asSet(uniPropSm);
        target |= asSet(uniPropSc);
        target |= asSet(uniPropSk);
        target |= asSet(uniPropSo);
    }
    else if(ucmp(name, "any") == 0)
        target = Set(0,0x110000);
    else if(ucmp(name, "ascii") == 0)
        target = Set(0,0x80);
    else
        return loadUnicodeSet!propsTab(name, target);
    return true;
}

// CTFE-only helper for checking property names at compile-time
@safe bool isPrettyPropertyName(C)(in C[] name)
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
    auto idx = names.countUntil!(x => comparePropertyName(x, name) == 0);
    return idx >= 0;
}

// ditto, CTFE-only, not optimized
@safe private static bool findSetName(alias table, C)(in C[] name)
{        
    return findUnicodeSet!table(name) >= 0;            
}

template SetSearcher(alias table, string kind)
{
    /// Run-time checked search.
    static auto opCall(C)(in C[] name)
        if(is(C : dchar))
    {
        CodepointSet set;
        if(loadUnicodeSet!table(name, set))
            return set;
        throw new Exception("No unicode set for "~kind~" by name "
            ~name.to!string~" was found.");
    }
    /// Compile-time checked search.
    static auto opDispatch(string name)()
    {
        static if(findSetName!table(name))
        {
            CodepointSet set;
            loadUnicodeSet!table(name, set);
            return set;
        }
        else
            static assert(false, "No unicode set for "~kind~" by name "
                ~name~" was found.");
    }
}

/**
    A single entry point to lookup Unicode $(CODEPOINT) sets by name or alias of 
    block, script or general category.

    It uses well defined standard rules of property name lookup.
    This includes fuzzy matching of names, so that
    'White_Space', 'white-SpAce' and 'whitespace' are all considered equal 
    and yield the same set of white space $(CHARACTERS).
*/
@safe public struct unicode
{
    /**
        Performs the lookup of set of $(CODEPOINTS)
        with compile-time correctness checking. 
        This short-cut version combines 3 searches:
        across blocks, scripts and common binary properties.

        Note that since scripts and blocks overlap the
        usual trick to disambiguate is used - to get a block use 
        $(D unicode.InBlockName), to search a script 
        use $(D unicode.ScriptName).

        See also $(LREF block), $(LREF script) 
        and not included in this search $(LREF hangulSyllableType).

        Example:
        ---            
        auto ascii = unicode.ASCII;
        assert(ascii['A']);
        assert(ascii['~']);
        assert(!ascii['\u00e0']);
        //matching is case-insensitive
        assert(ascii == unicode.ascII);
        assert(!ascii['à']);
        //underscores, '-' and whitespace in names are ignored too
        auto latin = unicode.in_latin1_Supplement;
        assert(latin['à']);
        assert(!latin['$']); 
        //BTW Latin 1 Supplement is a block, hence "In" prefix
        assert(latin == unicode("In Latin 1 Supplement"));
        import std.exception;
        // run-time look up throws if no such set is found
        assert(collectException(unicode("InCyrilliac")));
        ---
    */
    
    static auto opDispatch(string name)()
    {
        static if(findAny(name))
            return loadAny(name);
        else
            static assert(false, "No unicode set by name "~name~" was found.");
    }
    
    /**
        The same lookup across blocks, scripts or binary property
        but performed at run-time.
        This version is provided for cases where $(D name) 
        is not known beforehand otherwise compile-time
        checked $(LREF opDispatch) is typically a better choice.
    */
    static auto opCall(C)(in C[] name)
        if(is(C : dchar))
    {
        return loadAny(name);       
    }

    /**
        Narrows down the search for sets of $(CODEPOINTS) to all Unicode blocks.

        Note: 
        Here block names are unambiguous as no scripts are searched 
        and thus to search use simply $(D unicode.block.BlockName) notation. 

        Example:
        ---
        // use .block for explicitness
        assert(unicode.block.Greek_and_Coptic == unicode.InGreek_and_Coptic);        
        ---
    */
    struct block
    {
        mixin SetSearcher!(blocksTab, "block");
    }

    /**
        Narrows down the search for sets of $(CODEPOINTS) to all Unicode scripts.

        Example:
        ---
        auto arabicScript = unicode.script.arabic;
        auto arabicBlock = unicode.block.arabic;
        // there is an intersection between script and block
        assert(arabicBlock['؁']);
        assert(arabicScript['؁']);
        // but they are different
        assert(arabicBlock != arabicScript);
        assert(arabicBlock == unicode.inArabic);
        assert(arabicScript == unicode.arabic);
        ---
    */
    struct script
    {
        mixin SetSearcher!(scriptsTab, "script");
    }

    /**
        Fetch a set of $(CODEPOINTS) that have the given hangul syllable type.

        Other non-binary properties (once supported) are following the same 
        notation: $(D unicode.propertyName.propertyValue) for compile-time 
        checked access and $(D unicode.propertyName(propertyValue))
        for run-time checked one.

        Example:
        ---
        // L here is syllable type not Letter as in unicode.L short-cut
        auto leadingVowel = unicode.hangulSyllableType("L");
        //check that some leading vowels are present
        foreach(vowel; '\u1110'..'\u115F')
            assert(leadingVowel[vowel]);
        assert(leadingVowel == unicode.hangulSyllableType.L);
        ---
    */
    struct hangulSyllableType
    {
        mixin SetSearcher!(hangulTab, "hangul syllable type");
    }

private:
    alias ucmp = comparePropertyName;

    static bool findAny(string name)
    {
        return isPrettyPropertyName(name) 
            || findSetName!propsTab(name) || findSetName!scriptsTab(name) 
            || (ucmp(name[0..2],"In") == 0 && findSetName!blocksTab(name[2..$]));
    }

    static auto loadAny(Set=CodepointSet, C)(in C[] name)
    {
        Set set;
        bool loaded = loadProperty(name, set) || loadUnicodeSet!scriptsTab(name, set)
            || (ucmp(name[0..2],"In") == 0 
                && loadUnicodeSet!blocksTab(name[2..$], set));
        if(loaded)
            return set;
        throw new Exception("No unicode set by name "~name.to!string~" was found.");
    }
    
    /// Disabled to prevent the mistake of creating instances of this pseudo-struct.
    //@disable ~this();
}

unittest
{
    auto ascii = unicode.ASCII;
    assert(ascii['A']);
    assert(ascii['~']);
    assert(!ascii['\u00e0']);
    //matching is case-insensitive
    assert(ascii == unicode.ascII);
    assert(!ascii['à']);
    //underscores, '-' and whitespace in names are ignored too
    auto latin = unicode.Inlatin1_Supplement;
    assert(latin['à']);
    assert(!latin['$']); 
    //BTW Latin 1 Supplement is a block, hence "In" prefix
    assert(latin == unicode("In Latin 1 Supplement"));  
    import std.exception;
    // R-T look up throws if no such set is found
    assert(collectException(unicode("InCyrilliac")));

    assert(unicode.block.Greek_and_Coptic == unicode.InGreek_and_Coptic);

    // L here is explicitly syllable type not "Letter" as in unicode.L
    auto leadingVowel = unicode.hangulSyllableType("L");
    //check that some leading vowels are present
    foreach(vowel; '\u1110'..'\u115F'+1)
        assert(leadingVowel[vowel]);
    assert(leadingVowel == unicode.hangulSyllableType.L);  

    auto arabicScript = unicode.script.arabic;
    auto arabicBlock = unicode.block.arabic;
    // there is an intersection between script and block
    assert(arabicBlock['؁']);
    assert(arabicScript['؁']);
    // but they are different
    assert(arabicBlock != arabicScript);
    assert(arabicBlock == unicode.inArabic);
    assert(arabicScript == unicode.arabic);
}

unittest
{
    assert(unicode("InHebrew") == asSet(blockHebrew));
    assert(unicode("separator") == (asSet(uniPropZs) | asSet(uniPropZl) | asSet(uniPropZp)));
    assert(unicode("In-Kharoshthi") == asSet(blockKharoshthi));
}

enum EMPTY_CASE_TRIE = ushort.max;// from what gen_uni uses internally

// control - '\r'
enum controlSwitch = `
    case '\u0000':..case '\u0008':case '\u000E':..case '\u001F':case '\u007F':..case '\u0084':case '\u0086':..case '\u009F': case '\u0009':..case '\u000C': case '\u0085':
`;
// TODO: redo the most of hangul stuff algorithmically in case of Graphemes too
// kill unrolled switches

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
                else if(isHangL(ch))
                    state = L;
                else if(hangLV[ch] || isHangV(ch))
                    state = V;
                else if(hangLVT[ch])
                    state = LVT;
                else if(isHangT(ch))
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
                if(isHangL(ch))
                    mixin(eat);
                else if(isHangV(ch) || hangLV[ch])
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
                if(isHangV(ch))
                    mixin(eat);
                else if(isHangT(ch))
                {
                    state = LVT;
                    mixin(eat);
                }
                else 
                    goto L_End_Extend;
            break;
            case LVT:
                if(isHangT(ch))
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
            // extend & spacing marks
            if(!graphemeExtend[ch] && !spacingMark[ch])
                break;
            mixin(eat);
        }
    L_End:
        static if(getValue)
            return grapheme;
    }

}

@trusted:
public: // Public API continues

/++
    Returns the length of grapheme cluster starting at $(D index).
    Both the resulting length and the $(D index) are measured
    in $(S_LINK Code unit, code units).

    Example:
    ---
    //ASCII as usual is 1 code unit, 1 code point etc.
    assert(graphemeStride("  ", 1) == 1);
    //A + combing ring above
    string city = "A\u030Arhus";    
    size_t first = graphemeStride(city, 0);
    assert(first == 3); //\u030A has 2 UTF-8 code units
    assert(city[0..first] == "A\u030A");
    assert(city[first..$] == "rhus");
    ---
+/
size_t graphemeStride(C)(in C[] input, size_t index)
    if(is(C : dchar))
{
    auto src = input[index..$];
    auto n = src.length;
    genericDecodeGrapheme!(false)(src);
    return n - src.length;
}

// for now tested separately see test_grapheme.d
unittest
{
    assert(graphemeStride("  ", 1) == 1);
    //A + combing ring above
    string city = "A\u030Arhus";    
    size_t first = graphemeStride(city, 0);
    assert(first == 3); //\u030A has 2 UTF-8 code units
    assert(city[0..first] == "A\u030A");
    assert(city[first..$] == "rhus");
}

/++
    Reads one full grapheme cluster from an input range of dchar $(D inp). 

    For examples see the $(LREF Grapheme) below.

    Note: 
    This function modifies $(D inp) and thus $(D inp) 
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
    s = "\u0300\u0308\u1100";
    assert(equal(decodeGrapheme(s)[], "\u0300\u0308"));
    assert(equal(decodeGrapheme(s)[], "\u1100"));
    s = "\u11A8\u0308\uAC01";
    assert(equal(decodeGrapheme(s)[], "\u11A8\u0308"));
    assert(equal(decodeGrapheme(s)[], "\uAC01"));
}

/++
    $(P A structure designed to effectively pack $(CHARACTERS) 
    of a $(CLUSTER). 
    )

    $(P $(D Grapheme) has value semantics so 2 copies of a $(D Grapheme) 
    always refer to distinct objects. In the most actual scenarios a $(D Grapheme) 
    fits on stack and avoids memory allocation overhead for all but quite 
    long clusters. 
    )

    Example:
    ---
    import std.algorithm;
    string bold = "ku\u0308hn";

    // note that decodeGrapheme takes parameter by ref
    // slicing a grapheme yields a range of dchar
    assert(decodeGrapheme(bold)[].equal("k"));

    // the next grapheme is 2 characters long
    auto wideOne = decodeGrapheme(bold);
    assert(wideOne.length == 2);
    assert(wideOne[].equal("u\u0308"));
        
    // the usual range manipulation is possible
    assert(wideOne[].filter!isMark.equal("\u0308"));
    ---
    $(P See also $(LREF decodeGrapheme), $(LREF graphemeStride). )
+/
@trusted struct Grapheme
{
public:
    this(C)(in C[] chars...)
        if(is(C : dchar))
    {
        this ~= chars;
    }

    this(Input)(Input seq)
        if(!isDynamicArray!Input 
            && isInputRange!Input && is(ElementType!Input : dchar))
    {
        this ~= seq;
    }

    /// Gets a $(CODEPOINT) at the given index in this cluster.
    dchar opIndex(size_t index) const  pure nothrow
    {
        assert(index < length);
        return read24(isBig ? ptr_ : small_.ptr, index);
    }

    /++
        Writes a $(CODEPOINT) $(D ch) at given index in this cluster.

        Warning:
        Use of this facility may invalidate grapheme cluster, 
        see also $(LREF Grapheme.valid).

        Example:
        ---
        auto g = Grapheme("A\u0302");
        assert(g[0] == 'A');
        assert(g.valid);
        g[1] = '~'; //ASCII tilda is not a combining mark
        assert(g[1] == '~');
        assert(!g.valid);
        ---
    +/
    void opIndexAssign(dchar ch, size_t index)  pure nothrow
    {
        assert(index < length);
        write24(isBig ? ptr_ : small_.ptr, ch, index);
    }

    /++
        Random-access range over Grapheme's $(CHARACTERS).

        Warning: Invalidates when this Grapheme leaves the scope, 
        attempts to use it then would lead to memory corruption.
    +/
    @system auto opSlice(size_t a, size_t b) pure nothrow
    {
        return sliceOverIndexed(a, b, &this);
    }

    /// ditto
    @system auto opSlice() pure nothrow
    {
        return sliceOverIndexed(0, length, &this);
    }

    /// Grapheme cluster length in $(CODEPOINTS).
    @property size_t length() const  pure nothrow
    { 
        return isBig ? len_ : slen_ & 0x7F; 
    }

    /++
        Append $(CHARACTER) $(D ch) to this grapheme.
        Warning: 
        Use of this facility may invalidate grapheme cluster, 
        see also $(D valid).

        Example:
        ---
        auto g = Grapheme("A");
        assert(g.valid);
        g ~= '\u0301';
        assert(g[].equal("A\u0301"));
        assert(g.valid);
        g ~= "B";
        // not a valid grapheme cluster anymore
        assert(!g.valid);
        // still could be useful though
        assert(g[].equal("A\u0301B"));
        ---
        See also $(LREF Grapheme.valid) below.
    +/
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

    /// Append all of $(CHARACTERS) from the input range $(D inp) to this Grapheme.
    ref opOpAssign(string op, Input)(Input inp)
        if(isInputRange!Input && is(ElementType!Input : dchar))
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

        Appending to and direct manipulation of grapheme's $(CHARACTERS) may 
        render it no longer valid. Certain applications may chose to use 
        Grapheme as a "small string" of any $(CODEPOINTS) and ignore this property
        entirely.        
    +/
    @property bool valid()() /*const*/
    {
        auto r = this[];
        genericDecodeGrapheme!false(r);
        return r.length == 0;
    }

    this(this)
    {
        if(isBig)
        {// dup it
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
    // "out of the blue" grow rate, needs testing 
    // (though graphemes are typically small < 9)
    enum grow = 20;
    enum small_cap = small_bytes/3;
    enum small_flag = 0x80, small_mask = 0x7F;
    // 16 bytes in 32bits, should be enough for the majority of cases
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
        // now we can overwrite small array data
        ptr_ = p;
        len_ = slen_;
        assert(grow > len_);
        cap_ = grow;
        setBig();
    }

    void setBig(){ slen_ |= small_flag; }

    @property size_t smallLength() pure nothrow
    { 
        return slen_ & small_mask; 
    }
    @property ubyte isBig() const  pure nothrow 
    { 
        return slen_ & small_flag; 
    }
}

//verify the example
unittest 
{
    import std.algorithm;
    string bold = "ku\u0308hn";

    // note that decodeGrapheme takes parameter by ref
    auto first = decodeGrapheme(bold);
    
    assert(first.length == 1);
    assert(first[0] == 'k');

    // the next grapheme is 2 characters long
    auto wideOne = decodeGrapheme(bold);
    // slicing a grapheme yields a random-access range of dchar
    assert(wideOne[].equal("u\u0308"));
    assert(wideOne.length == 2);
    static assert(isRandomAccessRange!(typeof(wideOne[])));
    
    // all of the usual range manipulation is possible
    assert(wideOne[].filter!isMark.equal("\u0308"));

    auto g = Grapheme("A");
    assert(g.valid);
    g ~= '\u0301';
    assert(g[].equal("A\u0301"));
    assert(g.valid);
    g ~= "B";
    // not a valid grapheme cluster anymore
    assert(!g.valid);
    // still could be useful though
    assert(g[].equal("A\u0301B"));
}

unittest
{
    auto g = Grapheme("A\u0302");
    assert(g[0] == 'A');
    assert(g.valid);
    g[1] = '~'; //ASCII tilda is not a combining mark
    assert(g[1] == '~');
    assert(!g.valid);
}

unittest
{
    // not valid clusters (but it just a test)
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

    Grapheme h;
    foreach(dchar v; iota(cast(int)'A', cast(int)'Z'+1).map!"cast(dchar)a")
        h ~= v;
    assert(equal(h[], iota(cast(int)'A', cast(int)'Z'+1)));
}

/++
    $(P Does basic case-insensitive comparison of strings $(D str1) and $(D str2).
    This function uses simpler comparison rule thus achieving better performance 
    then $(LREF icmp). However keep in mind the warning below.)

    Warning: 
    This function only handles 1:1 $(CODEPOINT) mapping
    and thus is not sufficient for certain alphabets 
    like German, Greek and few others.

    Example:
    ---
    assert(sicmp("Август", "авгусТ") == 0);
    // Greek also works as long as there is no 1:M mapping in sight
    assert(sicmp("ΌΎ", "όύ") == 0);     
    // things like the following won't get matched as equal
    // Greek small letter iota with dialytika and tonos
    assert(sicmp("ΐ", "\u03B9\u0308\u0301" != 0);    

    // while icmp has no problem with that
    assert(icmp("ΐ", "\u03B9\u0308\u0301" == 0); 
    assert(icmp("ΌΎ", "όύ") == 0);    
    ---
+/
int sicmp(C1, C2)(const(C1)[] str1, const(C2)[] str2)
{
    alias simpleCaseTable sTable;
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
        // simpleCaseTrie is packed index table
        if(idx != EMPTY_CASE_TRIE)
        {
            if(idx2 != EMPTY_CASE_TRIE)
            {// both cased chars
                // adjust idx --> start of bucket
                idx = idx - sTable[idx].n;
                idx2 = idx2 - sTable[idx2].n;
                if(idx == idx2)// one bucket, equivalent chars
                    continue;
                else//  not the same bucket
                    diff = sTable[idx].ch - sTable[idx2].ch;
            }
            else
                diff = sTable[idx - sTable[idx].n].ch - rhs;
        }
        else if(idx2 != EMPTY_CASE_TRIE)
        {
            diff = lhs - sTable[idx2 - sTable[idx2].n].ch;
        }
        // one of chars is not cased at all
        return diff;
    }
    return ridx == str2.length ? 0 : -1;
}

private int fullCasedCmp(C)(dchar lhs, dchar rhs, ref const(C)[] rtail)
{
    alias fullCaseTable fTable;
    size_t idx = fullCaseTrie[lhs];
    // fullCaseTrie is packed index table
    if(idx == EMPTY_CASE_TRIE)
        return lhs;
    size_t start = idx - fTable[idx].n;
    size_t end = fTable[idx].size + start;
    assert(fTable[start].entry_len == 1);
    for(idx=start; idx<end; idx++)
    {
        if(fTable[idx].entry_len == 1)
        {
            if(fTable[idx].ch == rhs)
            {
                return 0;
            }
        }
        else 
        {// OK it's a long chunk, like 'ss' for German
            dstring seq = fTable[idx].seq;
            if(rhs == seq[0] 
                && rtail.skipOver(seq[1..$]))
            {
                // note that this path modifes rtail 
                // iff we managed to get there
                return 0;
            }
        }
    }
    return fTable[start].ch; // new remapped character for accurate diffs
}

/++
    $(P Does case insensitive comparison of $(D str1) and $(D str2).
    Follows the rules of full case-folding mapping.
    This includes matching as equal german ß with "ss" and 
    other 1:M $(CODEPOINT) mappings unlike $(LREF sicmp).
    The cost of $(D icmp) being pedantically correct is 
    slightly worse performance. 
    )

    Example:
    ---
    assert(icmp("Rußland", "Russland") == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9", "\u1F61\u03B9 -> ᾲ") == 0);    
    ---
+/
int icmp(C1, C2)(const(C1)[] str1, const(C2)[] str2)
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
        //first try to match lhs to <rhs,right-tail> sequence
        int cmpLR = fullCasedCmp(lhs, rhs, str2);
        if(!cmpLR)
            continue;
        //then rhs to <lhs,left-tail> sequence
        int cmpRL = fullCasedCmp(rhs, lhs, str1);
        if(!cmpRL)
            continue;
        // cmpXX contain remapped codepoints 
        // to obtain stable ordering of icmp
        diff = cmpLR - cmpRL;
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
            //Check example:
            assert(cfunc("Август".to!S1, "авгусТ".to!S2) == 0);
            assert(cfunc("ΌΎ".to!S1, "όύ".to!S2) == 0); 
        }
        // check that the order is properly agnostic to the case
        auto strs = [ "Apple", "ORANGE",  "orAcle", "amp", "banana"];
        sort!((a,b) => cfunc(a,b) < 0)(strs);    
        assert(strs == ["amp", "Apple",  "banana", "orAcle", "ORANGE"]);        
    }
    assert(icmp("ßb", "ssa") > 0);
    //Check example:
    assert(icmp("Russland", "Rußland") == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9", "\u1F61\u03B9 -> ᾲ") == 0);
    assert(icmp("ΐ"w, "\u03B9\u0308\u0301") == 0);
    assert(sicmp("ΐ", "\u03B9\u0308\u0301") != 0);    
}

/++
    Returns the combining class of $(D ch).
    
    $(P The combining class is a numerical value used by the 
    Unicode Canonical Ordering Algorithm to determine which sequences 
    of combining marks are to be considered canonically equivalent and
    which are not. )

    $(P Canonical equivalence is the criterion used to determine whether two 
    $(CODEPOINT) sequences are considered identical for interpretation. )

    Example:
    ---
    //shorten the code
    alias CC = combiningClass; 

    // combining tilda
    assert(CC('\u0303') == 230);
    // combining ring below
    assert(CC('\u0325') == 220);
    // the simple consequence is that  "tilda" should be 
    // placed after a "ring below" in a sequence
    ---
+/
ubyte combiningClass(dchar ch)
{
    return combiningClassTrie[ch];
}

unittest
{
    foreach(ch; 0..0x80)
        assert(combiningClass(ch) == 0);
    assert(combiningClass('\u05BD') == 22);
    assert(combiningClass('\u0300') == 230);
    assert(combiningClass('\u0317') == 220);
    assert(combiningClass('\u1939') == 222);
}

/// Unicode character decomposition type.
enum UnicodeDecomposition {
    /// Canonical decomposition. The result is canonically equivalent sequence.
    Canonical, 
    /**
         Compatibility decomposition. The result is compatibility equivalent sequence.
         Note: Compatibility decomposition is a $(B lossy) conversion, 
         typically suitable only for fuzzy matching and internal processing.
    */
    Compatibility
};

/**
    Shorthand aliases for character decomposition type, passed as a
    template parameter to $(LREF decompose).
*/
enum { 
    Canonical = UnicodeDecomposition.Canonical,
    Compatibility = UnicodeDecomposition.Compatibility
};

/++
    Try to canonically compose 2 $(CHARACTERS).
    Returns the composed $(CHARACTER) if they do compose and dchar.init otherwise.

    The assumption is that $(D first) comes before $(D second) in the original text, 
    usually meaning that the first is a starter.

    Note: Hangul syllables are not covered by this function. 
    See $(D composeJamo) below.

    Example:
    ---
    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);
    ---
+/
public dchar compose(dchar first, dchar second)
{
    size_t packed = compositionJumpTrie[first];
    if(packed == ushort.max)
        return dchar.init;
    // unpack offset and length
    size_t idx = packed & composeIdxMask, cnt = packed >> composeCntShift;
    // TODO: optimize this micro binary search (no more then 4-5 steps)
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
    Returns a full $(S_LINK Canonical decomposition, Canonical)
    (by default) or $(S_LINK Compatibility decomposition, Compatibility)
    decomposition of $(CHARACTER) $(D ch). 
    If no decomposition is available returns a $(LREF Grapheme) 
    with the $(D ch) itself.

    Note:
    This function also decomposes hangul syllables 
    as prescribed by the standard. 
    See also $(LREF decomposeHangul) for a restricted version
    that takes into account only hangul syllables  but  
    no other decompositions.

    Example:
    ---
    import std.algorithm;
    assert(decompose('Ĉ')[].equal("C\u0302"));
    assert(decompose('D')[].equal("D"));
    assert(decompose('\uD4DC')[].equal("\u1111\u1171\u11B7"));
    assert(decompose!Compatibility('¹').equal("1"));
    ---
+/
public Grapheme decompose(UnicodeDecomposition decompType=Canonical)(dchar ch)
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
    if(!idx) // not found, check hangul arithmetic decomposition
        return decomposeHangul(ch); 
    auto decomp = table[idx..$].until(0);
    return Grapheme(decomp);
}

unittest
{
    //verify examples
    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);

    import std.algorithm;
    assert(decompose('Ĉ')[].equal("C\u0302"));
    assert(decompose('D')[].equal("D"));
    assert(decompose('\uD4DC')[].equal("\u1111\u1171\u11B7"));
    assert(decompose!Compatibility('¹')[].equal("1"));
}

//----------------------------------------------------------------------------
// Hangul specific composition/decomposition
enum jamoSBase = 0xAC00;
enum jamoLBase = 0x1100;
enum jamoVBase = 0x1161;
enum jamoTBase = 0x11A7;
enum jamoLCount = 19, jamoVCount = 21, jamoTCount = 28;
enum jamoNCount = jamoVCount * jamoTCount;
enum jamoSCount = jamoLCount * jamoNCount;

// Tests if $(D ch) is a Hangul leading consonant jamo.
bool isJamoL(dchar ch)
{
    // first cmp rejects ~ 1M code points above leading jamo range
    return ch < jamoLBase+jamoLCount && ch >= jamoLBase;
}

// Tests if $(D ch) is a Hangul vowel jamo.
bool isJamoT(dchar ch)
{
    // first cmp rejects ~ 1M code points above trailing jamo range
    // Note: ch == jamoTBase doesn't indicate trailing jamo (TIndex must be > 0)
    return ch < jamoTBase+jamoTCount && ch > jamoTBase;
}

// Tests if $(D ch) is a Hangul trailnig consonant jamo.
bool isJamoV(dchar ch)
{
    // first cmp rejects ~ 1M code points above vowel range
    return  ch < jamoVBase+jamoVCount && ch >= jamoVBase;
}

int hangulSyllableIndex(dchar ch)
{
    int idxS = cast(int)ch - jamoSBase;
    return idxS >= 0 && idxS < jamoSCount ? idxS : -1;
}

// internal helper: compose hangul syllables leaving dchar.init in holes
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

/**
    Decomposes a Hangul syllable. If ($D ch) is not a composed syllable
    then this function returns $(LREF Grapheme) containing only $(D ch) as is. 

    Example:
    ---
    import std.algorithm;
    assert(decomposeHangul('\uD4DB')[].equal("\u1111\u1171\u11B6"));
    ---
*/
Grapheme decomposeHangul(dchar ch)
{
    int idxS = cast(int)ch - jamoSBase;
    if(idxS < 0 || idxS >= jamoSCount) return Grapheme(ch);
    int idxL = idxS / jamoNCount;   
    int idxV = (idxS % jamoNCount) / jamoTCount;
    int idxT = idxS % jamoTCount;

    int partL = jamoLBase + idxL;
    int partV = jamoVBase + idxV;
    if(idxT > 0) // there is a trailling consonant (T); <L,V,T> decomposition
        return Grapheme(partL, partV, jamoTBase + idxT);
    else // <L, V> decomposition
        return Grapheme(partL, partV);
}

/++
    Try to compose hangul syllable out of a leading consonant ($(D lead)), 
    a $(D vowel) and optional $(D trailing) consonant jamos.

    On success returns the composed LV or LVT hangul syllable.

    If any of $(D lead) and $(D vowel) are not a valid hangul jamo 
    of the respective $(CHARACTER) class returns dchar.init.

    Example:
    ---
    assert(composeJamo('\u1111', '\u1171', '\u11B6') == '\uD4DB');
    // leaving out T-vowel, or passing any codepoint 
    // that is not trailing consonant composes an LV-syllable
    assert(composeJamo('\u1111', '\u1171') == '\uD4CC'); 
    assert(composeJamo('\u1111', '\u1171', ' ') == '\uD4CC'); 
    assert(composeJamo('\u1111', 'A') == dchar.init);
    assert(composeJamo('A', '\u1171') == dchar.init);
    ---
+/
dchar composeJamo(dchar lead, dchar vowel, dchar trailing=dchar.init)
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

unittest
{
    void testDecomp(UnicodeDecomposition T)(dchar ch, string r)
    {
        assert(equal(decompose!T(ch)[], r), text(decompose(ch)[], " vs ", r));
    }
    testDecomp!Canonical('\u1FF4', "\u03C9\u0301\u0345");
    testDecomp!Canonical('\uF907', "\u9F9C");
    testDecomp!Compatibility('\u33FF', "\u0067\u0061\u006C");
    testDecomp!Compatibility('\uA7F9', "\u0153");
    //check examples
    assert(decomposeHangul('\uD4DB')[].equal("\u1111\u1171\u11B6"));
    assert(composeJamo('\u1111', '\u1171', '\u11B6') == '\uD4DB');
    assert(composeJamo('\u1111', '\u1171') == '\uD4CC'); //leave out T-vowel
    assert(composeJamo('\u1111', '\u1171', ' ') == '\uD4CC'); 
    assert(composeJamo('\u1111', 'A') == dchar.init);
    assert(composeJamo('A', '\u1171') == dchar.init);
}

/**
    Enumeration type for normalization forms,
    passed as template parameter for functions like $(LREF normalize). 
*/
enum NormalizationForm {
    NFC,
    NFD,
    NFKC,
    NFKD
}


enum { 
    /**
        Shorthand aliases from values indicating normalization forms.
    */
    NFC = NormalizationForm.NFC, 
    ///ditto
    NFD = NormalizationForm.NFD, 
    ///ditto
    NFKC = NormalizationForm.NFKC,
    ///ditto
    NFKD = NormalizationForm.NFKD
};

/++
    Returns $(D input) string normalized to the chosen form. 
    The Form C is used by default. 

    For more information on normalization forms see 
    the $(S_LINK Normalization, normalization section).

    Note:
    In cases where the string in question is already normalized, 
    it is returned unmodified and no memory allocation happens.

    Example:
    ---
    //any encoding works
    wstring greet = "Hello world"; 
    assert(normalize(greet) is greet); //the same exact slice

    // An example of a character with all 4 forms being different:
    // Greek upsilon with acute and hook symbol (code point 0x03D3)
    assert(normalize!NFC("ϓ") == "\u03D3");
    assert(normalize!NFD("ϓ") == "\u03D2\u0301");
    assert(normalize!NFKC("ϓ") == "\u038E");
    assert(normalize!NFKD("ϓ") == "\u03A5\u0301");
    ---
+/
inout(C)[] normalize(NormalizationForm norm=NFC, C)(inout(C)[] input)
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
            else // NFKD & NFKC
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
                // found a stable code point after unstable ones 
                sort!("a[0] < b[0]", SwapStrategy.stable)
                    (zip(ccc[firstNonStable..idx], decomposed[firstNonStable..idx]));
                firstNonStable = decomposed.length;
            }
            else if(clazz != 0 && lastClazz == 0)
            {
                // found first unstable code point after stable ones 
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
                // 2nd pass for hangul syllables
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
        // reset variables
        decomposed.length = 0;
        decomposed.assumeSafeAppend();
        ccc.length = 0;
        ccc.assumeSafeAppend();
        input = input[anchors[1]..$];
        // and move on
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

    //check example

    //any encoding works
    wstring greet = "Hello world"; 
    assert(normalize(greet) is greet); //the same exact slice

    // An example of a character with all 4 forms being different:
    // Greek upsilon with acute and hook symbol (code point 0x03D3)
    assert(normalize!NFC("ϓ") == "\u03D3");
    assert(normalize!NFD("ϓ") == "\u03D2\u0301");
    assert(normalize!NFKC("ϓ") == "\u038E");
    assert(normalize!NFKD("ϓ") == "\u03A5\u0301");
}

// canonically recompose given slice of code points, works in-place and mutates data
private size_t recompose(size_t start, dchar[] input, ubyte[] ccc)
{
    assert(input.length == ccc.length);
    int accumCC = -1;// so that it's out of 0..255 range
    bool foundSolidStarter = false;
    // writefln("recomposing %( %04x %)", input);
    // first one is always a starter thus we start at i == 1
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
                input[i] = dchar.init;// put a sentinel
                // current was merged so its CCC shouldn't affect 
                // composing with the next one 
            }
            else {
                // if it was a starter then accumCC is now 0, end of loop
                accumCC = curCC;
                if(accumCC == 0)
                    break;
            }
        }
        else{
            // ditto here
            accumCC = curCC;
            if(accumCC == 0)
                break;
        }
        i++;        
    }
    return i;
}

// returns tuple of 2 indexes that delimit:
// normalized text, piece that needs normalization and 
// the rest of input starting with stable code point
private auto splitNormalized(NormalizationForm norm, C)(const(C)[] input)
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

private auto seekStable(NormalizationForm norm, C)(size_t idx, in C[] input)
{
    auto br = input[0..idx];
    size_t region_start = 0;// default
    for(;;)
    {
        if(br.empty)// start is 0
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
    size_t region_end=input.length;// end is $ by default
    foreach(i, dchar ch; input[idx..$])
    {
        if(combiningClassTrie[ch] == 0 && allowedIn!norm(ch))
        {
            region_end = i+idx;
            break;
        }
    }
    // writeln("Region to normalize: ", input[region_start..region_end]);
    return tuple(region_start, region_end);
}

/**
    Tests if dchar $(D ch) is always allowed (Quick_Check=YES) in normalization
    form $(D norm).
    ---
    //e.g. Cyrillic is always allowed, so is ASCII
    assert(allowedIn!NFC('я'));
    assert(allowedIn!NFD('я'));
    assert(allowedIn!NFKC('я'));
    assert(allowedIn!NFKD('я'));
    assert(allowedIn!NFC('Z'));
    ---    
*/
public bool allowedIn(NormalizationForm norm)(dchar ch)
{
    return !notAllowedIn!norm(ch);
}

// not user friendly name but more direct 
private bool notAllowedIn(NormalizationForm norm)(dchar ch)
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

unittest
{
    assert(allowedIn!NFC('я'));
    assert(allowedIn!NFD('я'));
    assert(allowedIn!NFKC('я'));
    assert(allowedIn!NFKD('я'));
    assert(allowedIn!NFC('Z'));
}

}

version(std_uni_bootstrap)
{
    // old version used for bootstrapping of gen_uni.d that generates 
    // up to date optimal versions of all of isXXX functions
    @safe pure nothrow public bool isWhite(dchar c) 
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
    Whether or not $(D c) is a Unicode whitespace $(CHARACTER).
    (general Unicode category: Part of C0(tab, vertical tab, form feed,
    carriage return, and linefeed characters), Zs, Zl, Zp, and NEL(U+0085))
+/
@safe pure nothrow 
public bool isWhite(dchar c)
{
    return isWhiteGen(c); // call pregenerated binary search
}

deprecated ("Please use std.uni.isLower instead")
bool isUniLower(dchar c) @safe pure nothrow
{
    return isLower(c);
}

/++
    Return whether $(D c) is a Unicode lowercase $(CHARACTER).
+/
@safe pure nothrow
bool isLower(dchar c)
{
    if(std.ascii.isASCII(c))
        return std.ascii.isLower(c);

    return lowerCaseTrie[c];
}

@safe unittest
{
    foreach(v; 0..0x80)
        assert(std.ascii.isLower(v) == isLower(v));
    assert(isLower('я'));
    assert(isLower('й'));
    assert(!isLower('Ж'));
    //Greek HETA
    assert(!isLower('\u0370'));
    assert(isLower('\u0371'));
    assert(!isLower('\u039C')); //capital MU
    assert(isLower('\u03B2')); //beta
    //from extended Greek
    assert(!isLower('\u1F18'));
    assert(isLower('\u1F00'));
    foreach(v; unicode.lowerCase.byCodepoint)
        assert(isLower(v) && !isUpper(v));
}


deprecated ("Please use std.uni.isUpper instead")
@safe pure nothrow
bool isUniUpper(dchar c)
{
    return isUpper(c);
}

/++
    Return whether $(D c) is a Unicode uppercase $(CHARACTER).
+/
@safe pure nothrow
bool isUpper(dchar c)
{
    if(std.ascii.isASCII(c))
        return std.ascii.isUpper(c);

    return upperCaseTrie[c];
}

@safe unittest
{
    foreach(v; 0..0x80)
        assert(std.ascii.isLower(v) == isLower(v));
    assert(!isUpper('й'));
    assert(isUpper('Ж'));
    //Greek HETA
    assert(isUpper('\u0370'));
    assert(!isUpper('\u0371'));
    assert(isUpper('\u039C')); //capital MU
    assert(!isUpper('\u03B2')); //beta
    //from extended Greek
    assert(!isUpper('\u1F00'));
    assert(isUpper('\u1F18'));
    foreach(v; unicode.upperCase.byCodepoint)
        assert(isUpper(v) && !isLower(v));
}


deprecated ("Please use std.uni.toLower instead")
@safe pure nothrow
dchar toUniLower(dchar c) 
{
    return toLower(c);
}

/++
    If $(D c) is a Unicode uppercase $(CHARACTER), then its lowercase equivalent
    is returned. Otherwise $(D c) is returned.
    
    Warning: certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toLower which takes full string instead.
+/
@safe pure nothrow
dchar toLower(dchar c)
{
     // optimize ASCII case
    if(c < 0xAA)
    {
        if(c < 'A')
            return c;
        if(c <= 'Z')
            return c + 32;
    }    
    size_t idx = simpleCaseTrie[c];
    alias simpleCaseTable stab;
    if(idx != EMPTY_CASE_TRIE)
    {
        size_t sz = stab[idx].size;
        idx = idx - stab[idx].n;
        switch(sz){
        default:
            assert(false);// no even buckets of size 5 currently
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
    return c;
}

@trusted unittest //@@@BUG std.format is not @safe
{
    import std.string : format;
    foreach(ch; 0..0x80)
        assert(std.ascii.toLower(ch) == toLower(ch));
    assert(toLower('Я') == 'я');
    assert(toLower('Δ') == 'δ');
    foreach(ch; unicode.upperCase.byCodepoint)
    {
        dchar low = ch.toLower;
        assert(low == ch || isLower(low), format("%s -> %s", ch, low));
    }
}

deprecated("Please use std.uni.toUpper instead")
@safe pure nothrow
dchar toUniUpper(dchar c)
{
    return toUpper(c);
}

/++
    If $(D c) is a Unicode lowercase $(CHARACTER), then its uppercase equivalent
    is returned. Otherwise $(D c) is returned.
     
    Warning: 
    Certain alphabets like German, Greek have no 1:1
    upper-lower mapping. Use overload of toUpper which takes full string instead.
+/
@safe pure nothrow
dchar toUpper(dchar c)
{
    // optimize ASCII case
    if(c < 0xAA)
    {
        if(c < 'a')
            return c;
        if(c <= 'z')
            return c - 32;
    }
    size_t idx = simpleCaseTrie[c];
    alias simpleCaseTable stab;
    if(idx != EMPTY_CASE_TRIE)
    {
        size_t sz = stab[idx].size;
        idx = idx - stab[idx].n;
        switch(sz){
        default:
            assert(false);// no even buckets of size 5 currently
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
    return c;
}

@trusted unittest
{
    import std.string : format;
    foreach(ch; 0..0x80)
        assert(std.ascii.toUpper(ch) == toUpper(ch));
    assert(toUpper('я') == 'Я');
    assert(toUpper('δ') == 'Δ');
    foreach(ch; unicode.lowerCase.byCodepoint)
    {
        dchar up = ch.toUpper;
        assert(up == ch || isUpper(up), format("%s -> %s", ch, up));
    }
}

deprecated("Please use std.uni.isAlpha instead.")
@safe pure nothrow
bool isUniAlpha(dchar c)
{
    return isAlpha(c);
}

/++
    Returns whether $(D c) is a Unicode alphabetic $(CHARACTER)
    (general Unicode category: Alphabetic).
+/
@safe pure nothrow
bool isAlpha(dchar c)
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

@safe unittest
{
    auto alpha = unicode("Alphabetic");
    foreach(ch; alpha.byCodepoint)
        assert(isAlpha(ch));   
    foreach(ch; 0..0x4000)
        assert((ch in alpha) == isAlpha(ch)); 
}


/++
    Returns whether $(D c) is a Unicode mark
    (general Unicode category: Mn, Me, Mc).
+/
@safe pure nothrow
bool isMark(dchar c)
{
    return markTrie[c];
}

@safe unittest
{
    auto mark = unicode("Mark");
    foreach(ch; mark.byCodepoint)
        assert(isMark(ch));   
    foreach(ch; 0..0x4000)
        assert((ch in mark) == isMark(ch)); 
}

/++
    Returns whether $(D c) is a Unicode numerical $(CHARACTER)
    (general Unicode category: Nd, Nl, No).
+/
@safe pure nothrow
bool isNumber(dchar c)
{
    return numberTrie[c];
}

@safe unittest
{
    auto n = unicode("N");
    foreach(ch; n.byCodepoint)
        assert(isNumber(ch));
    foreach(ch; 0..0x4000)
        assert((ch in n) == isNumber(ch));
}


/++
    Returns whether $(D c) is a Unicode punctuation $(CHARACTER)
    (general Unicode category: Pd, Ps, Pe, Pc, Po, Pi, Pf).
+/
@safe pure nothrow
bool isPunctuation(dchar c)
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
    foreach(ch; unicode("P").byCodepoint)
        assert(isPunctuation(ch));
}

/++
    Returns whether $(D c) is a Unicode symbol $(CHARACTER)
    (general Unicode category: Sm, Sc, Sk, So).   
+/
@safe pure nothrow
bool isSymbol(dchar c)
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
    foreach(ch; unicode("S").byCodepoint)
        assert(isSymbol(ch), format("%04x", ch));
}

/++
    Returns whether $(D c) is a Unicode space $(CHARACTER)
    (general Unicode category: Zs)
    Note: This doesn't include '\n', '\r', \t' and other non-space $(CHARACTER).
    For commonly used less strict semantics see $(LREF isWhite).
+/
@safe pure nothrow
bool isSpace(dchar c)
{
    return isSpaceGen(c);
}

unittest
{
    assert(isSpace('\u0020'));
    auto space = unicode.Zs;
    foreach(ch; space.byCodepoint)
        assert(isSpace(ch));
    foreach(ch; 0..0x1000)
        assert(isSpace(ch) == space[ch]);
}


/++
    Returns whether $(D c) is a Unicode graphical $(CHARACTER)
    (general Unicode category: L, M, N, P, S, Zs).
 
+/
@safe pure nothrow
bool isGraphical(dchar c)
{
    return graphicalTrie[c];
}


unittest
{   
    auto set = unicode("Graphical");
    import std.string;
    foreach(ch; set.byCodepoint)
        assert(isGraphical(ch), format("%4x", ch));    
    foreach(ch; 0..0x4000)
        assert((ch in set) == isGraphical(ch));
}


/++
    Returns whether $(D c) is a Unicode control $(CHARACTER)
    (general Unicode category: Cc).
+/
@safe pure nothrow
bool isControl(dchar c)
{
    return isControlGen(c);
}

unittest
{
    assert(isControl('\u0000'));
    assert(isControl('\u0081'));
    assert(!isControl('\u0100'));
    auto cc = unicode.Cc;    
    foreach(ch; cc.byCodepoint)
        assert(isControl(ch));
    foreach(ch; 0..0x1000)
        assert(isControl(ch) == cc[ch]);
}


/++
    Returns whether $(D c) is a Unicode formatting $(CHARACTER)
    (general Unicode category: Cf).
+/
@safe pure nothrow
bool isFormat(dchar c)
{
    return isFormatGen(c);
}


unittest
{
    assert(isFormat('\u00AD'));
    foreach(ch; unicode("Format").byCodepoint)
        assert(isFormat(ch));
}

// code points for private use, surrogates are not likely to change in near feature
// if need be they can be generated from unicode data as well

/++
    Returns whether $(D c) is a Unicode Private Use $(CODEPOINT)
    (general Unicode category: Co).
+/
@safe pure nothrow
bool isPrivateUse(dchar c)
{
    return (0x00_E000 <= c && c <= 0x00_F8FF)
        || (0x0F_0000 <= c && c <= 0x0F_FFFD)
        || (0x10_0000 <= c && c <= 0x10_FFFD);
}

/++
    Returns whether $(D c) is a Unicode surrogate $(CODEPOINT)
    (general Unicode category: Cs).
+/
@safe pure nothrow
bool isSurrogate(dchar c)
{
    return (0xD800 <= c && c <= 0xDFFF);
}

/++
    Returns whether $(D c) is a Unicode high surrogate (lead surrogate).
+/
@safe pure nothrow
bool isSurrogateHi(dchar c)
{
    return (0xD800 <= c && c <= 0xDBFF);
}

/++
    Returns whether $(D c) is a Unicode low surrogate (trail surrogate).
+/
@safe pure nothrow
bool isSurrogateLo(dchar c)
{
    return (0xDC00 <= c && c <= 0xDFFF);
}

/++
    Returns whether $(D c) is a Unicode non-character i.e. 
    a $(CODEPOINT) with no assigned abstract character.
    (general Unicode category: Cn)
+/
@safe pure nothrow
bool isNonCharacter(dchar c)
{
    return nonCharacterTrie[c]; 
}

unittest
{
    auto set = unicode("Cn");
    foreach(ch; set.byCodepoint)
        assert(isNonCharacter(ch));
}

private:
// load static data from pre-generated tables into usable datastructures


@safe auto asSet(const (ubyte)[] compressed)
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
immutable nonCharacterTrie  = asTrie(nonCharacterTrieEntries);

immutable nfcQC = asTrie(nfcQCTrieEntries);
immutable nfdQC = asTrie(nfdQCTrieEntries);
immutable nfkcQC = asTrie(nfkcQCTrieEntries);
immutable nfkdQC = asTrie(nfkdQCTrieEntries);

immutable graphemeExtend = asTrie(graphemeExtendTrieEntries); 
immutable spacingMark = asTrie(mcTrieEntries);

// TODO: move sets below to Tries

__gshared CodepointSet hangLV;
__gshared CodepointSet hangLVT;

shared static this()
{
    hangLV = asSet(hangulLV);
    hangLVT = asSet(hangulLVT);
}

immutable combiningClassTrie = asTrie(combiningClassTrieEntries);
immutable canonMapping = asTrie(canonMappingTrieEntries);
immutable compatMapping = asTrie(compatMappingTrieEntries);
immutable simpleCaseTrie = asTrie(simpleCaseTrieEntries);
immutable fullCaseTrie = asTrie(fullCaseTrieEntries);
immutable lowerCaseTrie = asTrie(lowerCaseTrieEntries);
immutable upperCaseTrie = asTrie(upperCaseTrieEntries);
immutable compositionJumpTrie = asTrie(compositionJumpTrieEntries);

}// version(!std_uni_bootstrap)

