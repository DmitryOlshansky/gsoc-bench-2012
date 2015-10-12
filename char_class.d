import bench_suite, std.algorithm, std.stdio, std.typetuple, std.conv, std.utf;

import alpha;

import std.uni;

alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol, isWhite) stdTests;

uint lastCount;

void classifyCall(alias mtd)(in char[] str)
{
    uint count=0;
    foreach(dchar ch; str.byUTF!dchar)
    {
        if(mtd(ch))
            count++;
    }
    lastCount = count;
}

void classifyMatcher(alias matcher)(in char[] str)
{
	uint count=0;
	const(char)[] s = str;
	while(s.length)
	{
		auto m = matcher(str);
		if(m)
			count++;
		s = s[m.stride..$];
	}
}

void classifyIndex(alias mtd)(in char[] str)
{
    uint count=0;
    foreach(dchar ch; str.byUTF!dchar)
    {
        if(mtd[ch])
            count++;
    }
    lastCount = count;
}

bool noop(dchar ch){ return ch > 0; }
bool combiningClassOf(dchar ch){ return combiningClass(ch) > 0; }

void myTest(Result[] data)
{
    alias TypeTuple!("alpha", "mark", "num", "sym", "white") names;
    foreach(x; data)
    {
            bench!(classifyCall!noop)("noop-decode", x);
            bench!(classifyCall!(stdIsAlpha))("std-ascii-alpha", x);
            bench!(classifyCall!(myIsAlpha))("my-ascii-alpha", x);
            bench!(classifyCall!combiningClassOf)("combining class", x);
            version(none)
            {
                bench!(classifyIndex!invAlpha)("inv-alpha", x);
                //writeln("CNT: ", lastCount);
                bench!(classifyIndex!invMark)("inv-mark", x);
                //writeln("CNT: ", lastCount);
                bench!(classifyIndex!invNumber)("inv-num", x);
                //writeln("CNT: ", lastCount);
                bench!(classifyIndex!invSymbol)("inv-sym", x);
                //writeln("CNT: ", lastCount);
            }            
            foreach(idx, ref level; customTries)
            {
                enum label = to!string(idx+1);
                bench!(classifyIndex!(level.triAlpha))("trie-"~label~"alpha", x);
                bench!(classifyIndex!(level.triMark))("trie-"~label~"-mark", x);
                bench!(classifyIndex!(level.triNumber))("trie-"~label~"-num", x);
                bench!(classifyIndex!(level.triSymbol))("trie-"~label~"-sym", x); 
            }
            bench!(classifyMatcher!bstAlpha)("tbst-alpha", x);
            bench!(classifyMatcher!bstMark)("tbst-mark", x);
            bench!(classifyMatcher!bstNumber)("tbst-num", x);
            bench!(classifyMatcher!bstSymbol)("tbst-symbol", x);
    }
}

void main(string[] argv)
{
    testAll!(myTest)(argv);
}

__gshared typeof(utfMatcher!char(CodepointSet(0,10))) u8mAlpha, u8mMark, u8mSymbol, u8mNumber;
alias u8Matchers = TypeTuple!(u8mAlpha, u8mMark, u8mSymbol, u8mNumber);

__gshared TinyUtfBst!char bstAlpha, bstMark, bstSymbol, bstNumber;

alias CodepointSet Set;

__gshared Set invAlpha, invMark, invSymbol, invNumber;
//1st is a simple array of packed bools thus is exceptionally fast at the cost of ~262Kb of RAM
alias MySpec1 = TypeTuple!(21);
alias MySpec2 = TypeTuple!(10, 11);
alias MySpec3 = TypeTuple!(8, 5, 8);
alias MySpec4 = TypeTuple!(7, 4, 4, 6);

struct Level(spec...){
    //to avoid possible TLS overhead, typically immutable is better
    alias sizes = spec;
    __gshared CodepointSetTrie!spec triAlpha, triMark, triNumber, triSymbol;        
}
Level!(MySpec1) levelOne;
Level!(MySpec2) levelTwo;
Level!(MySpec3) levelThree;
Level!(MySpec4) levelFour;
alias customTries = TypeTuple!(levelOne, levelTwo, levelThree, levelFour);

shared static this()
{    
    invAlpha = unicode("Alphabetic");
    invMark = unicode("Mark");
    invSymbol = unicode("Symbol");
    invNumber = unicode("number");
    u8mAlpha = utfMatcher!char(invAlpha);
    u8mMark = utfMatcher!char(invMark);
    u8mNumber = utfMatcher!char(invNumber);
    u8mSymbol = utfMatcher!char(invSymbol);
    bstAlpha = tinyUtfBst!char(invAlpha);
    bstMark = tinyUtfBst!char(invMark);
    bstSymbol = tinyUtfBst!char(invSymbol);
    bstNumber = tinyUtfBst!char(invNumber);
    foreach(idx, ref level; customTries)
    {
        alias generate = codepointSetTrie!(level.sizes);
        //writefln("Creating level %s of Tries.", idx+1);
        level.triAlpha = generate(invAlpha);
        //writeln("Alpha:", level.triAlpha.bytes);
        level.triMark = generate(invMark);
        //writeln("Mark:", level.triMark.bytes);
        level.triNumber = generate(invNumber);
        //writeln("Number:", level.triNumber.bytes);
        level.triSymbol = generate(invSymbol);
        //writeln("Symbol:", level.triSymbol.bytes);
    }
}
