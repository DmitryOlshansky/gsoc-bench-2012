import bench_suite, std.algorithm, std.stdio, std.typetuple, std.conv, std.utf;

import alpha;

import std.uni;
static import std.internal.uni;
import std.internal.uni_tab;


alias TypeTuple!(isAlpha, isMark, isNumber, isSymbol, isWhite) stdTests;

uint lastCount;

void clasifyCall(alias mtd)(in dchar[] str)
{
    uint count=0;
    foreach(ch; str)
    {
        if(mtd(ch))
            count++;
    }
    lastCount = count;
}

void clasifyIndex(alias mtd)(in dchar[] str)
{
    uint count=0;
    foreach(ch; str)
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
            bench!(clasifyCall!combiningClassOf)("combining class", x.name, x.data);
            version(none)
            {
                bench!(clasifyIndex!invAlpha)("inv-alpha", x.name, x.data);
                //writeln("CNT: ", lastCount);
                bench!(clasifyIndex!invMark)("inv-mark", x.name, x.data);
                //writeln("CNT: ", lastCount);
                bench!(clasifyIndex!invNumber)("inv-num", x.name, x.data);
                //writeln("CNT: ", lastCount);
                bench!(clasifyIndex!invSymbol)("inv-sym", x.name, x.data);
                //writeln("CNT: ", lastCount);
            }            
            foreach(idx, ref level; customTries)
            {
                writeln("\nTries of level ", idx+1);
                bench!(clasifyIndex!(level.triAlpha))("trie-alpha", x.name, x.data);
                bench!(clasifyIndex!(level.triMark))("trie-mark", x.name, x.data);
                bench!(clasifyIndex!(level.triNumber))("trie-num", x.name, x.data);
                bench!(clasifyIndex!(level.triSymbol))("trie-sym", x.name, x.data); 
            }
            writeln("\nBaselines");
            bench!(clasifyCall!noop)("noop", x.name, x.data);  
            bench!(clasifyCall!(stdIsAlpha))("std-ascii-alpha", x.name, x.data);
            writeln(lastCount);
            bench!(clasifyCall!(myIsAlpha))("new-ascii-alpha", x.name, x.data);
            writeln(lastCount);
    }    
}

void main(string[] argv)
{
    testAll!(myTest)(argv);
}

__gshared Utf8Matcher u8mAlpha, u8mMark, u8mSymbol, u8mNumber;
alias u8Matchers = TypeTuple!(u8mAlpha, u8mMark, u8mSymbol, u8mNumber);

alias std.internal.uni.CodepointTrie!8 OldTrie;
__gshared OldTrie alphaTrie = OldTrie(unicodeAlphabetic);
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
    u8mAlpha = buildUtf8Matcher(invAlpha);
    u8mMark = buildUtf8Matcher(invMark);
    u8mNumber = buildUtf8Matcher(invNumber);
    u8mSymbol = buildUtf8Matcher(invSymbol);
    foreach(idx, ref level; customTries)
    {
        alias generate = codepointSetTrie!(level.sizes);
        writefln("Creating level %s of Tries.", idx+1);
        level.triAlpha = generate(invAlpha);
        writeln("Alpha:", level.triAlpha.bytes);
        level.triMark = generate(invMark);
        writeln("Mark:", level.triMark.bytes);
        level.triNumber = generate(invNumber);
        writeln("Number:", level.triNumber.bytes);
        level.triSymbol = generate(invSymbol);
        writeln("Symbol:", level.triSymbol.bytes);
    }
}
