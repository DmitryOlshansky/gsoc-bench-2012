import bench_suite, std.stdio, std.typetuple, std.conv;



version(std_uni)
	import std.uni;
else{
	import uni;
	alias TypeTuple!(rleAlpha, rleMark, rleNumber, rleSymbol) rleTests;
	alias TypeTuple!(invAlpha, invMark, invNumber, invSymbol) invTests;
} 

alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol, isWhite) stdTests;

uint lastCount;

void clasifyCall(alias mtd)(in char[] str)
{
	uint count=0;
	foreach(dchar ch; str)
	{
		if(mtd(ch))
			count++;
	}
	lastCount = count;
}

void clasifyIndex(alias mtd)(in char[] str)
{
	uint count=0;
	foreach(dchar ch; str)
	{
		if(mtd[ch])
			count++;
	}
	lastCount = count;
}

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
            /*bench!(clasifyIndex!invAlpha)("inv-uint-alpha", x.name, x.data);
            bench!(clasifyIndex!invMark)("inv-uint-mark", x.name, x.data);
            bench!(clasifyIndex!invNumber)("inv-uint-num", x.name, x.data);
            bench!(clasifyIndex!invSymbol)("inv-uint-sym", x.name, x.data);*/
            bench!(clasifyIndex!triAlpha)("tri-uint-alpha", x.name, x.data);
            bench!(clasifyIndex!triMark)("tri-uint-mark", x.name, x.data);
            bench!(clasifyIndex!triNumber)("tri-uint-num", x.name, x.data);
            bench!(clasifyIndex!triSymbol)("tri-uint-sym", x.name, x.data);                     
            //BUG with foreach over TypeTuple, uses only the first one i.e. rleAlpha or invAlpha
            //foreach(i, m; invTests)
            //	bench!(clasifyIndex!m)("invlist-"~to!string(i), x.name, x.data);

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

    RleBitSet!uint rleAlpha, rleMark, rleNumber, rleSymbol;
    InvList invAlpha, invMark, invNumber, invSymbol;
    MyTrie triAlpha, triMark, triNumber, triSymbol;

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

        rleAlpha = unicodeSetByName("Letter");
        rleMark = unicodeSetByName("Mark");
        rleSymbol = unicodeSetByName("Symbol");
        rleNumber = unicodeSetByName("number");


        invAlpha = InvList(rleAlpha);
        invMark = InvList(rleMark);
        invNumber = InvList(rleNumber);
        invSymbol = InvList(rleSymbol);

        triAlpha = MyTrie(rleAlpha);
        triMark = MyTrie(rleMark);
        triNumber = MyTrie(rleNumber);
        triSymbol = MyTrie(rleSymbol);

    }

}