import uni, std.stdio, std.typecons, std.typetuple, std.algorithm, std.utf, std.file, std.datetime;

auto bench(alias method)(string title, string name, in char[] data)
{
	size_t idx=0;	
	StopWatch sw;
	sw.start();
	while(idx < data.length)
		idx += stride(data, idx);
	sw.stop();
	auto spent = sw.peek().usecs;

	writefln("%16s [%6s], %9s, %.2f;"
		, title,  name[0..min($,6)], spent/1000.0,  data.length*1000000.0/spent/1024/1024);
}

ubyte[] table;
ubyte[] pktable;

pure genTable()
{
	ubyte[] table = new ubyte[256];
	char[2] s;
	foreach(i, ref v; table)
	{
		s[0] = cast(ubyte)i;
		try{
			v = cast(ubyte)stride(s[], 0);
		}
		catch(Exception ex)
		{
			v = 0;
		}
	}
	return table;
}



size_t old_table(C)(in C[] str, size_t idx)
{
	size_t r = table[str[idx]];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", idx);
	return r;
}

size_t packed_table(C)(in C[] str, size_t idx)
{
	size_t r = (pktable[str[idx]/2] >> (idx&1)*4) & 0xF;
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", idx);
	return r;
}

size_t trie_table(C)(in C[] str, size_t idx)
{
	size_t r = trie1[str[idx]];
	if(!r)
		throw new UTFException("Invalid UTF-8 sequence", idx);
	return r;
}

alias Trie!(size_t, char, sliceBits!(0, 4), sliceBits!(4, 8)) MyTrie;

MyTrie trie1;

static this()
{
	table = genTable();
	pktable = new ubyte[128];
	for(size_t i=0; i<table.length; i+=2)
		pktable[i/2]  = cast(ubyte)(table[i] | (table[i+1]<<4));
	trie1 = MyTrie(table);
}

void main(string argv[])
{
	auto datum = new Tuple!(char[], string)[argv.length-1];
	foreach(i, name; argv[1..$])
		datum[i] = tuple(cast(char[])std.file.read(name), name);
	foreach(mtd; TypeTuple!(stride, old_table, packed_table, trie_table))
	{
		foreach(x; datum)
			bench!(mtd)(mtd.stringof[0..10]~"-", x[1], x[0]);
	}
}

