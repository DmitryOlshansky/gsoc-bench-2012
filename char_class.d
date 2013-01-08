import bench_suite, std.algorithm, std.stdio, std.typetuple, std.conv, std.utf;

version(std_uni)
    import std.uni;
else{
    import uni;
    alias TypeTuple!(invAlpha, invMark, invNumber, invSymbol) invTests;
} 

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
version(std_uni){}
else
{
    bool combiningClassOf(dchar ch){ return combiningClass(ch) > 0; }
}

void myTest(Result[] data)
{
    alias names = TypeTuple!("alpha", "mark", "num", "sym", "white");
    foreach(x; data)
    {
        version(std_uni){
            writeln("\nBaselines");
            foreach(i, m; stdTests)
                bench!(clasifyCall!m)("std-"~names[i], x.name, x.data);
        }
        else
        {
            writeln("\nBaselines");
            bench!(clasifyCall!noop)("noop", x.name, x.data);  
            
            foreach(i, m; stdTests){
                  bench!(clasifyCall!m)("new-std-"~names[i], x.name, x.data);
               //writeln("CNT: ", lastCount);
            }
            bench!(clasifyCall!combiningClassOf)("combining class", x.name, x.data);
            
            /*bench!(clasifyIndex!invAlpha)("inv-uint-alpha", x.name, x.data);
            //writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invMark)("inv-uint-mark", x.name, x.data);
            //writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invNumber)("inv-uint-num", x.name, x.data);
            //writeln("CNT: ", lastCount);
            bench!(clasifyIndex!invSymbol)("inv-uint-sym", x.name, x.data);
            //writeln("CNT: ", lastCount);*/
            foreach(idx, ref level; customTries)
            {
                writeln("\nTries of level ", idx+1);
                bench!(clasifyIndex!(level.triAlpha))("trie-alpha", x.name, x.data);
                bench!(clasifyIndex!(level.triMark))("trie-mark", x.name, x.data);
                bench!(clasifyIndex!(level.triNumber))("trie-num", x.name, x.data);
                bench!(clasifyIndex!(level.triSymbol))("trie-sym", x.name, x.data); 
            }

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
    //1st is a simple array of packed bools thus is exceptionally fast at the cost of ~262Kb of RAM
    alias MyTrie1 = CodepointSetTrie!(21);
    alias MyTrie2 = CodepointSetTrie!(10, 11);
    alias MyTrie3 = CodepointSetTrie!(8, 5, 8);
    alias MyTrie4 = CodepointSetTrie!(7, 4, 4, 6);
    
    struct Level(T){
        __gshared T triAlpha, triMark, triNumber, triSymbol;
    }
    Level!(MyTrie1) levelOne;
    Level!(MyTrie2) levelTwo;
    Level!(MyTrie3) levelThree;
    Level!(MyTrie4) levelFour;
    alias customTries = TypeTuple!(levelOne, levelTwo, levelThree, levelFour);

    shared static this()
    {

        invAlpha = unicode("Alphabetic");
        invMark = unicode("Mark");
        invSymbol = unicode("Symbol");
        invNumber = unicode("number");
        foreach(idx, ref level; customTries)
        {
            alias T = typeof(level.triAlpha);
            writefln("Creating level %s of Tries.", idx+1);
            level.triAlpha = T(invAlpha);
            writeln("Alpha:", level.triAlpha.bytes);
            level.triMark = T(invMark);
            writeln("Mark:", level.triMark.bytes);
            level.triNumber = T(invNumber);
            writeln("Number:", level.triNumber.bytes);
            level.triSymbol = T(invSymbol);
            writeln("Symbol:", level.triSymbol.bytes);
        }
    }

}
