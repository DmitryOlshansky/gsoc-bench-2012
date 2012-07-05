import uni, bench_suite, std.stdio, std.typetuple, std.algorithm;

ubyte[] table;
ubyte[] pktable;

pure auto genTable()
{
	ubyte[] table = new ubyte[128];
	char[2] s;
	for(size_t i=0; i<table.length; i++)
	{
		s[0] = cast(ubyte)(i+128);
		s[1] = 0;
		try{
			table[i] = cast(ubyte)stride(s[], 0);
		}
		catch(Exception ex)
		{
			table[i] = 0;
		}
	}
	return table;
}

//std.utf stride copied verbatim:

uint stride(S)(in S str, size_t index) @safe pure
    if (is(S : const(char[])))
{
    immutable c = str[index];
    if (c < 0x80)
        return 1;
    else
        return strideImpl(c, index);
 }

private uint strideImpl(char c, size_t index) @trusted pure
in { assert(c & 0x80); }
body
{
    static if (__traits(compiles, {import core.bitop; bsr(1);}))
    {
        import core.bitop;
        immutable msbs = 7 - bsr(~c);
        if (msbs >= 2 && msbs <= 6) return msbs;
    }
    else
    {
        if (!(c & 0x40)) goto Lerr;
        if (!(c & 0x20)) return 2;
        if (!(c & 0x10)) return 3;
        if (!(c & 0x08)) return 4;
        if (!(c & 0x04)) return 5;
        if (!(c & 0x02)) return 6;
    }
 Lerr:
    throw new UTFException("Invalid UTF-8 sequence", index);
}

//optimized version:
size_t optiStride(C)(in C[] str, size_t index)
{
    immutable c = str[index];
    if (c < 0x80)
        return 1;
    else
        return optiStrideImpl(c, index);
 }

private size_t optiStrideImpl(char c, size_t index) @trusted pure
in { assert(c & 0x80); }
body
{
    static if (__traits(compiles, {import core.bitop; bsr(1);}))
    {
        import core.bitop;
        immutable msbs = 7 - bsr(~c);
        immutable test = msbs - 2;
        if (test <= 4) return msbs;
    }
    else
    {
        if (!(c & 0x40)) goto Lerr;
        if (!(c & 0x20)) return 2;
        if (!(c & 0x10)) return 3;
        if (!(c & 0x08)) return 4;
        if (!(c & 0x04)) return 5;
        if (!(c & 0x02)) return 6;
    }
 Lerr:
    throw new UTFException("Invalid UTF-8 sequence", index);
}

//tabulated version
size_t old_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
    if (c < 0x80)
        return 1;
	size_t r = table[str[index]-0x80];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

//same but with packing of 2 4bit values in one byte
size_t packed_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
    if (c < 0x80)
        return 1;
	size_t r = (pktable[(str[index]-0x80)/2] >> (index&1)*4) & 0xF;
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

size_t trie_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
    if (c < 0x80)
        return 1;
	size_t r = trie1[str[index]-0x80];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

alias Trie!(size_t, char, sliceBits!(4, 7), sliceBits!(0, 4)) MyTrie;

MyTrie trie1;

size_t siftThrough(alias func)(in char[] data)
{
	size_t idx=0;
	while(idx < data.length)
		idx += func(data, idx);
	return idx;
}

shared static this()
{
	table = genTable();
	pktable = new ubyte[64];
	for(size_t i=0; i<table.length; i+=2)
		pktable[i/2]  = cast(ubyte)(table[i] | (table[i+1]<<4));
	trie1 = MyTrie(table.dup);                                          

	writeln(table);
}

void strideTest(Result[] data)
{
	foreach(mtd; TypeTuple!(stride, optiStride, old_table, packed_table/*, trie_table*/))
	{
		foreach(x; data)
			bench!(siftThrough!(mtd))(mtd.stringof[0..10]~"-", x.name, x.data);
	}
}

void main(string argv[])
{
	testAll!(strideTest)(argv);
}



// copied from std.utf
/++
    Exception thrown on errors in std.utf functions.
  +/
class UTFException : Exception
{
    uint[4] sequence;
    size_t  len;


    UTFException setSequence(uint[] data...) @safe pure nothrow
    {
        import std.algorithm;

        assert(data.length <= 4);

        len = min(data.length, 4);
        sequence[0 .. len] = data[0 .. len];

        return this;
    }


    this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        super(msg, file, line, next);
    }


    this(string msg, size_t index, string file = __FILE__, size_t line = __LINE__, Throwable next = null)
    {
        import std.string;
        super(msg ~ format(" (at index %s)", index), file, line, next);
    }


    override string toString()
    {
        import std.string;
        if(len == 0)
            return super.toString();

        string result = "Invalid UTF sequence:";

        foreach(i; sequence[0 .. len])
            result ~= format(" %02x", i);

        if(super.msg.length > 0)
        {
            result ~= " - ";
            result ~= super.msg;
        }

        return result;
    }
}
