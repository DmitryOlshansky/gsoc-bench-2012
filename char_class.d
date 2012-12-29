import bench_suite, std.algorithm, std.stdio, std.typetuple, std.conv, std.utf;

version(std_uni)
    import std.uni;
else{
    import uni;
    alias TypeTuple!(invAlpha, invMark, invNumber, invSymbol) invTests;
} 

alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol, isWhite) stdTests;

uint lastCount;

void clasifyCall(alias mtd)(in char[] str)
{
    uint count=0;
    size_t idx;
    while(idx < str.length)
    {
        dchar ch = decode(str, idx);
        if(mtd(ch))
            count++;
    }
    lastCount = count;
}

void clasifyIndex(alias mtd)(in char[] str)
{
    uint count=0;
    size_t idx;
    while(idx < str.length)
    {
        dchar ch = decode(str, idx);
        if(mtd[ch])
            count++;
    }
    lastCount = count;

}

bool noop(dchar ch){ return ch > 0; }

void myTest(Result[] data)
{
    foreach(x; data)
    {        
        version(std_uni){
            foreach(i, m; stdTests)
                bench!(clasifyCall!m)("std-"~to!string(i), x.name, x.data);
        }
        else
        {
            bench!(clasifyCall!noop)("noop", x.name, x.data);            
            foreach(i, m; stdTests){
                  bench!(clasifyCall!m)("new-std-"~to!string(i), x.name, x.data);
               writeln("CNT: ", lastCount);
            }
            bench!(clasifyIndex!invAlpha)("inv-uint-alpha", x.name, x.data);
            writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invMark)("inv-uint-mark", x.name, x.data);
            writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invNumber)("inv-uint-num", x.name, x.data);
            writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invSymbol)("inv-uint-sym", x.name, x.data);
            writeln("CNT: ", lastCount);
            bench!(clasifyIndex!triAlpha)("tri-uint-alpha", x.name, x.data);
            bench!(clasifyIndex!triMark)("tri-uint-mark", x.name, x.data);
            bench!(clasifyIndex!triNumber)("tri-uint-num", x.name, x.data);
            bench!(clasifyIndex!triSymbol)("tri-uint-sym", x.name, x.data); 
        }
    }    
}

void main(string[] argv)
{
    testAll!(myTest)(argv);
}

version(std_uni){}
else
{
    alias InversionList!(GcPolicy) InvList;

    __gshared InvList invAlpha, invMark, invNumber, invSymbol;
    __gshared MyTrie triAlpha, triMark, triNumber, triSymbol;

    //alias Trie!(bool, dchar, sliceBits!(16, 21), sliceBits!(0, 16)) MyTrie;
    alias Trie!(bool, dchar, sliceBits!(16, 21), sliceBits!(8, 16), sliceBits!(0, 8)) MyTrie;

    struct UtfTrie(Char)
        if(Char.sizeof == 1)
    {
        Trie!(bool, char, x => x) trie1;
        //TODO: adjust with UTF-8 encoding layout in mind
        //+ remove extra work by avoiding top-level sliceBits
        Trie!(bool, ushort, sliceBits!(8, 16), sliceBits!(0, 8)) trie2;
        Trie!(bool, uint,  sliceBis!(16, 24), sliceBits!(8, 16), sliceBits!(0, 8)) trie3;
        Trie!(bool, uint,  sliceBis!(20, 32), sliceBits!(8, 20), sliceBits!(0, 8)) trie4;

        this(Set)(in Set set)
        {
            
        }
    }

    shared static this()
    {

        invAlpha = unicode("Alphabetic");
        invMark = unicode("Mark");
        invSymbol = unicode("Symbol");
        invNumber = unicode("number");

        triAlpha = MyTrie(invAlpha);
        triMark = MyTrie(invMark);
        triNumber = MyTrie(invNumber);
        triSymbol = MyTrie(invSymbol);
    }

}
