import bench_suite, uni, std.stdio, std.typetuple, std.conv;

alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol, isWhite) stdTests;
alias TypeTuple!(rleAlpha, rleMark, rleNumber, rleSymbol) rleTests;
alias TypeTuple!(invAlpha, invMark, invNumber, invSymbol) invTests;
 
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
	//writeln(count);
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
	//writeln(count);
}

void myTest(Result[] data)
{
	foreach(x; data)
	{
		
		foreach(i, m; stdTests)
			bench!(clasifyCall!m)("std-"~to!string(i), x.name, x.data);

		bench!(clasifyIndex!invAlpha)("inv-uint-alpha", x.name, x.data);
		bench!(clasifyIndex!invMark)("inv-uint-mark", x.name, x.data);
		bench!(clasifyIndex!invNumber)("inv-uint-num", x.name, x.data);
		bench!(clasifyIndex!invSymbol)("inv-uint-sym", x.name, x.data);
                /*
		bench!(clasifyIndex!triAlpha)("tri-uint-alpha", x.name, x.data);
		bench!(clasifyIndex!triMark)("tri-uint-mark", x.name, x.data);
		bench!(clasifyIndex!triNumber)("tri-uint-num", x.name, x.data);
		bench!(clasifyIndex!triSymbol)("tri-uint-sym", x.name, x.data);*/
		//BUG with foreach over TypeTuple, uses only the first one i.e. rleAlpha or invAlpha
		//foreach(i, m; invTests)
		//	bench!(clasifyIndex!m)("invlist-"~to!string(i), x.name, x.data);
	}	
}

void main(string[] argv)
{
	testAll!(myTest)(argv);
}


alias InversionList!(GcPolicy) InvList;

RleBitSet!uint rleAlpha, rleMark, rleNumber, rleSymbol;
InvList invAlpha, invMark, invNumber, invSymbol;
MyTrie triAlpha, triMark, triNumber, triSymbol;

alias Trie!(bool, dchar, sliceBits!(16, 21), sliceBits!(0, 16)) MyTrie;

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
    rleAlpha = unicodeLu | unicodeLl | unicodeLt | 
				unicodeLo | unicodeLm;
    rleMark = unicodeMn| unicodeMc | unicodeMe;
    rleSymbol = unicodeSm | unicodeSc | unicodeSk | unicodeSo;
    rleNumber = unicodeNd | unicodeNl | unicodeNo;

	triAlpha = MyTrie(rleAlpha);
    triMark = MyTrie(rleMark);
    triNumber = MyTrie(rleNumber);
    triSymbol = MyTrie(rleSymbol);

    invAlpha = InvList(rleAlpha);
    invMark = InvList(rleMark);
    invNumber = InvList(rleNumber);
    invSymbol = InvList(rleSymbol);
    
}