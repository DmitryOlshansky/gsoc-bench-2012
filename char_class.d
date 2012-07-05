import bench_suite, uni, uni_tab, std.stdio, std.typetuple, std.conv;

alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol) stdTests;
alias TypeTuple!(rleAlpha, rleMark, rleNumber, rleSymbol) rleTests;

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
		foreach(i, m; stdTests)
			bench!(clasifyCall!m)("std-"~to!string(i), x.name, x.data);
		/*foreach(i, m; rleTests)
			bench!(clasifyIndex!m)("rle-uint-"~to!string(i), x.name, x.data);*/
	}	
}

void main(string[] argv)
{
	testAll!(myTest)(argv);
}


RleBitSet!uint rleAlpha;
RleBitSet!uint rleMark;
RleBitSet!uint rleNumber;
RleBitSet!uint rleSymbol;

shared static this()
{
    rleAlpha = unicodeLu | unicodeLl | unicodeLt | 
				unicodeLo | unicodeLm;
    rleMark = unicodeMn| unicodeMc | unicodeMe;
    rleSymbol = unicodeSm | unicodeSc | unicodeSk | unicodeSo;
    rleNumber = unicodeNd | unicodeNl | unicodeNo;
}