import std.stdio, std.typetuple, uni;

RleBitSet!uint rleAlpha, rleMark, rleNumber, rleSymbol
        , rlePunct, rleSpace, rleGraphical, rleControl
        , rleFormat, rleNonCharacter, rleWhite;

shared static this()
{
    rleAlpha = unicodeLu | unicodeLl | unicodeLt | 
            unicodeLo | unicodeLm;
    rleMark = unicodeMn| unicodeMc | unicodeMe;
    rleSymbol = unicodeSm | unicodeSc | unicodeSk | unicodeSo;
    rleNumber = unicodeNd | unicodeNl | unicodeNo;
    rlePunct = unicodePd | unicodePs | unicodePe | unicodePc | unicodePo | unicodePi | unicodePf;
    //rleSpace = unicodeZs;  
    rleGraphical =  rleAlpha | rleMark  | rleNumber | rlePunct | rleSpace | rleSymbol;
    //rleControl = unicodeCc;
    //rleFormat = unicodeCf;
    //rleNonCharacter = unicodeCn;
    //rleWhite = unicodeWhite_Space;
}

void test_all(alias fn)()
{
    writeln("White\n\n");
    fn(unicodeWhite_Space);
    writeln("Alpha\n\n");
    fn(rleAlpha);
    writeln("Mark\n\n");
    fn(rleMark);
    writeln("Number\n\n");
    fn(rleNumber);
    writeln("Punctuation\n\n");
    fn(rlePunct);
    writeln("Symbol\n\n");
    fn(rleSymbol);
    writeln("Space\n\n");
    fn(unicodeZs);
    writeln("Graphical\n\n");
    fn(rleGraphical);
    writeln("Control\n\n");
    fn(unicodeCc);
    writeln("Format\n\n");
    fn(unicodeCf);
    writeln("Noncharacter\n\n");
    fn(unicodeCn);
}

void main(string[] argv)
{
    version(level4)
        test_all!test_4_level();
    else version(level3)
        test_all!test_3_level();
    else
        static assert(0, "Pick a version level3 or level4");
}

void test_3_level(Set)(in Set set)
{
  foreach(lvl_1; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        foreach(lvl_2; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
        {
            static if(lvl_1 + lvl_2  <= 16)
            {
                enum lvl_3 = 21-lvl_2-lvl_1;
                alias Trie!(bool, dchar, sliceBits!(21-lvl_1, 21)
                    , sliceBits!(lvl_3, 21-lvl_1)
                    , sliceBits!(0, lvl_3)) CurTrie;
 
                    CurTrie t = CurTrie(set);
                    writefln("Trie %d_%d_%d bytes, %d", lvl_1, lvl_2, lvl_3, t.bytes);
            }
        }
}

void test_4_level(Set)(in Set set)
{
  foreach(lvl_1; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
        foreach(lvl_2; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
            foreach(lvl_3; TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12))
        {
            static if(lvl_1 + lvl_2 + lvl_3  <= 16)
            {
                enum lvl_4 = 21-lvl_3-lvl_2-lvl_1;
                alias Trie!(bool, dchar, sliceBits!(21-lvl_1, 21)
                    , sliceBits!(21-lvl_1-lvl_2, 21-lvl_1)
                    , sliceBits!(21-lvl_1-lvl_2-lvl_3, 21-lvl_1-lvl_2)
                    , sliceBits!(0,21-lvl_1-lvl_2-lvl_3)) CurTrie;
 
                    CurTrie t = CurTrie(set);
                    writefln("Trie %d_%d_%d_%d bytes, %d", lvl_1, lvl_2, lvl_3, lvl_4, t.bytes);
            }
        }
}
