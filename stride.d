import uni, bench_suite, std.stdio, std.typetuple, std.range, std.algorithm;

@system:

__gshared ubyte[] table;
__gshared ubyte[] pktable;
immutable ubyte[] full_table = 
[
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 0, 0
];
alias Trie!(ubyte, char, sliceBits!(4, 7), sliceBits!(0, 4)) MyTrie;
__gshared MyTrie trie1;

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

uint stride(S)(in S str, size_t index) pure
    if (is(S : const(char[])))
{
    immutable c = str[index];
    if (c < 0x80)
        return 1;
    else
        return strideImpl(c, index);
 }

private uint strideImpl(char c, size_t index) pure
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

private size_t optiStrideImpl(char c, size_t index) pure
in { assert(c & 0x80); }
body
{
    static if (__traits(compiles, {import core.bitop; bsr(1);}))
    {
        import core.bitop;
        size_t msbs = bsr(~c);
        size_t test = 5 - msbs;
        if (test <= 4) return 7 - msbs;
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

//tabulated versions
size_t plain_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
	size_t r = full_table[c];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

size_t if_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
    if (c < 0x80)
        return 1;
	size_t r = table[c-0x80];
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
	size_t r = (pktable[(c-0x80)/2] >> (index&1)*4) & 0xF;
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

size_t trie_table(C)(in C[] str, size_t index)
{
	immutable c = str[index];
    if (c < 0x80)
        return 1;
	size_t r = trie1[c-0x80];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", index);
	return r;
}

struct KillIndex
{
	uint index;
	ulong[4] pages;
}

immutable KillIndex kidx = prepKillIndex();
enum uint kill_reg = kidx.index;
enum mask3 = 0b111, mask4 = 0b1111;

pure KillIndex prepKillIndex()
{
	uint top=0;	
	ulong[4] pages;
	ulong tmp=0;
	size_t top_i=0;
	for(int i=0; i<128;){
		immutable val = full_table[i+0x80];		
		tmp = tmp | (cast(ulong)val << 4*(i%16));
		i++;
		if(i % 16 == 0)
		{
			size_t k=0;
			for( ;k<top_i;k++)
			{
				if(pages[k] == tmp)
					break;
			}
			//7 bits total, shift left 4  == 3 top bits for index register
			top = top | (k  << 3*(i>>4)-3);
			pages[k] = tmp; 
			if(k == top_i)
				top_i++;
			tmp = 0;
		}
	}
	assert(pages[3] != 0);
	return KillIndex(top, pages);
}

size_t case_stride(C)(in C[] str, size_t index)
{
	switch(str[index] >> 3)
	{
	default: // ASCII with 0x80 + 
		return 1;
	case 0x80: .. case 0x87:
	case 0x8F: //5 & 6 lengths are not supported any longer see unicode 6.0+
		throw new UTFException("Invalid UTF-8 sequence", index);
	case 0x88: .. case 0x8B:
		return 2; 
	case 0x8C: .. case 0x8D:
		return 3;
	case 0x8E:
		return 4;
	}
}

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
	for(int i=0;i<8;i++)
		writef("%1d ", (kill_reg>> (3*i))& mask3);
	writeln();
	writeln("While table contains:");
	for(int i=0;i<8;i++)
	{
		writefln("FULL TAB %d: %s", i, table[i*16..i*16+16]);
		writef("KILL TRIE %d: ", i);
		ulong t = kidx.pages[(kill_reg >> 3*(i)) & mask3];
		for(size_t j=0;j<16;j++){
			writef("%d, ", (t>>(4*j)) & mask4);
		}
		writeln();	
	}
	writefln("index: %x", kidx.index);
	writefln("pages: [%( 0x%x, %)]", kidx.pages);
	for(int i=0;i<128; i+=8)
		writeln(i/8,": ",table[i..i+8]);
}

void strideTest(Result[] data)
{
	foreach(mtd; TypeTuple!(stride, optiStride, plain_table, if_table, packed_table, trie_table, case_stride))
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
