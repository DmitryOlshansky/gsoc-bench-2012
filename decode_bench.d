import std.algorithm, std.range, std.file, std.datetime, std.stdio, 
    std.typetuple, std.conv, std.utf;

import alpha;

import std.uni;

alias Matcher = typeof(utfMatcher!char(CodepointSet.init));
__gshared Matcher u8mAlpha, u8mMark, u8mSymbol, u8mNumber;
alias u8Matchers = TypeTuple!(u8mAlpha, u8mMark, u8mSymbol, u8mNumber);

__gshared CodepointSetTrie!(8, 5, 8) triAlpha;


shared static this()
{
    CodepointSet invAlpha, invMark, invSymbol, invNumber;
    invAlpha = unicode("Alphabetic");
    invMark = unicode("Mark");
    invSymbol = unicode("Symbol");
    invNumber = unicode("number");
    
    triAlpha =  codepointSetTrie!(8,5,8)(invAlpha);

    u8mAlpha = utfMatcher!char(invAlpha);
    u8mMark = utfMatcher!char(invMark);
    u8mNumber = utfMatcher!char(invNumber);
    u8mSymbol = utfMatcher!char(invSymbol);
}

int countMatcher(alias matcher)(in char[] datum)
{
    int count;
    auto s = datum[];
    while(s.length) //sadly .empty is not inlined with LDC...
        if(matcher.skip(s))
            count++;
    return count;
}

int countDecodePlusTable(alias table)(in char[] datum)
{
    int count;
    size_t idx = 0;
    while(idx != datum.length)
    {
        dchar ch = decode(datum, idx);
        if(table[ch])
            count++;
    }
    return count;   
}

int decodeOnly(in char[] datum)
{
    int count;
    size_t idx = 0;
    while(idx != datum.length)
    {
        decode(datum, idx);
        count++;
    }
    return count;
}

int decodeNothrow(in char[] datum) nothrow
{
    int count;
    foreach(ch; datum.byUTF!dchar)
    {
        count++;
    }
    return count;
}

int noop(in char[] datum)
{
    int count;
    for(size_t i=0; i<datum.length; i++)
    {
        if(datum[i] > 0x20)
            count++;
    }
    return count;
}

alias m8List = staticMap!(countMatcher, u8mAlpha, u8mMark, u8mSymbol, u8mNumber);
alias oldList = staticMap!(countDecodePlusTable, triAlpha);
alias methods = TypeTuple!(m8List, oldList, decodeOnly, decodeNothrow, noop);

void main(string[] argv)
{
    import std.string;
    import core.memory;
    enum iters = 25;
    string[] titles = "m8-Alpha m8-Mark m8-Symbol m8-Number 
        trie-Alpha decode-only decode-nothrow noop".split;
    StopWatch sw;
    foreach(name; argv[1..$])
    {
        auto text = cast(char[])std.file.read(name);
        writeln("====================");
        GC.disable();        
        foreach(j, mtd; methods)
        {
            sw.start();
            int cnt;
            foreach(_; 0..iters)
                cnt += mtd(text);
            sw.stop();
            auto spent = sw.peek().usecs;
            //time in usecs - throughput number is thus in megabytes/s
            writefln("%16s [%6s], %9s hits, %9s, %.2f Mb/s", titles[j], 
                name[0..min($,6)], cnt/iters, spent/(iters*1000.0),
                text.length*iters*1.0/spent);
            sw.reset();
        }
        GC.enable();
    }
}
