import std.stdio, std.typetuple, uni;

RleBitSet!uint rleAlpha, rleMark, rleNumber, rleSymbol
        , rlePunct, rleSpace, rleGraphical, rleControl
        , rleFormat, rleNonCharacter, rleWhite;

template GetBitSlicing(size_t Top, Sizes...)
{
    static if(Sizes.length > 0)
        alias TypeTuple!(sliceBits!(Top - Sizes[0], Top)
            , GetBitSlicing!(Top - Sizes[0], Sizes[1..$])) GetBitSlicing;
    else
        alias TypeTuple!()  GetBitSlicing;
}

template CodepointTrie(Sizes...)
{
    alias Trie!(bool, dchar, GetBitSlicing!(21, Sizes)) CodepointTrie;
}

shared static this()
{
    rleAlpha = unicodeSetByName("Letter");
    rleMark = unicodeSetByName("Mark");
    rleSymbol = unicodeSetByName("Symbol");
    rleNumber = unicodeSetByName("Number");
    rlePunct = unicodeSetByName("Punctuation");
   // rleSpace = unicodeZs;  
    rleGraphical =  rleAlpha | rleMark  | rleNumber | rlePunct | rleSpace | rleSymbol;
    //rleControl = unicodeCc;
    //rleFormat = unicodeCf;
    //rleNonCharacter = unicodeCn;
    //rleWhite = unicodeWhite_Space;
}

void test_all(alias fn)()
{
    fn(unicodeWhite_Space, "White");
    fn(rleAlpha, "Alpha");
    fn(rleMark, "Mark");
    fn(rleNumber, "Number");
    fn(rlePunct, "Punctuation");
    fn(rleSymbol, "Symbol");
    fn(unicodeZs, "Space");
    fn(rleGraphical, "Graphical");
    fn(unicodeCc, "Control");
    fn(unicodeCf, "Format");
    fn(unicodeCn, "Noncharacter");
}

void main(string[] argv)
{
    version(level4)
        test_all!test_4_level();
    else version(level3)
        test_all!test_3_level();
    else version(level2)
        test_all!test_2_level();
    else
        static assert(0, "Pick a version level3 or level4");
}

void test_2_level(Set)(in Set set, string name)
{
    writefln("%s\n\n", name);
    size_t best = size_t.max;
    uint[2] blvl;
    foreach(lvl_1; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    {
        enum lvl_2 = 21-lvl_1;
        alias CodepointTrie!(lvl_1, lvl_2) CurTrie;
        CurTrie t = CurTrie(set);
        writefln("%d_%d, %d", lvl_1, lvl_2, t.bytes);
        if(t.bytes < best)
        {
            best = t.bytes;
            blvl[] = [lvl_1, lvl_2];
        } 
    }
    stderr.writefln(`auto best%s2`
                `CodepointTrie!(%s, %s)(unicodeSetByname("%s");`
                , name, blvl[0], blvl[1],  name);
}

void test_3_level(Set)(in Set set, string name)
{
    writefln("%s\n\n", name);
    size_t best = size_t.max;
    uint[3] blvl;
    foreach(lvl_1; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        foreach(lvl_2; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        {
            static if(lvl_1 + lvl_2  <= 16)
            {
                enum lvl_3 = 21-lvl_2-lvl_1;
                alias CodepointTrie!(lvl_1, lvl_2, lvl_3) CurTrie;
                CurTrie t = CurTrie(set);
                writefln("%d_%d_%d, %d", lvl_1, lvl_2, lvl_3, t.bytes);
                if(t.bytes < best)
                {
                    best = t.bytes;
                    blvl[] = [lvl_1, lvl_2, lvl_3];
                }
            }
        }

    stderr.writefln(`auto best%s3`
                `CodepointTrie!(%s, %s, %s)(unicodeSetByname("%s");`
                , name, blvl[0], blvl[1], blvl[2], name);
}

void test_4_level(Set)(in Set set, string name)
{
    size_t best = size_t.max;
    uint[4] blvl;
    writefln("%s\n\n", name);
    foreach(lvl_1; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
        foreach(lvl_2; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
            foreach(lvl_3; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
        {
            static if(lvl_1 + lvl_2 + lvl_3  <= 16)
            {
                enum lvl_4 = 21-lvl_3-lvl_2-lvl_1;
                alias CodepointTrie!(lvl_1, lvl_2, lvl_3, lvl_4) CurTrie;
                CurTrie t = CurTrie(set);                
                writefln("%d_%d_%d_%d, %d", lvl_1, lvl_2, lvl_3, lvl_4, t.bytes);
                if(t.bytes < best)
                {
                    best = t.bytes;
                    blvl[] = [lvl_1, lvl_2, lvl_3, lvl_4];
                }
            }
        }
    stderr.writefln(`auto best%s4`
                `CodepointTrie!(%s, %s, %s, %s)(unicodeSetByname("%s");`
                , name, blvl[0], blvl[1], blvl[2], blvl[3], name);
}
