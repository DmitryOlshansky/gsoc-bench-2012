import std.stdio, std.algorithm, std.string, std.conv
	, std.typetuple, std.format, uni;

RleBitSet!uint rleAlpha, rleMark, rleNumber, rleSymbol
        , rlePunct, rleSpace, rleGraphical, rleControl
        , rleFormat, rleNonCharacter, rleWhite;

shared static this()
{
    rleAlpha = unicodeSet("Alphabetic");
    rleMark = unicodeSet("Mark");
    rleSymbol = unicodeSet("Symbol");
    rleNumber = unicodeSet("Number");
    rlePunct = unicodeSet("Punctuation");
   // rleSpace = unicodeZs;  
    rleGraphical =  rleAlpha | rleMark  | rleNumber | rlePunct | rleSpace | rleSymbol;
    //rleControl = unicodeCc;
    //rleFormat = unicodeCf;
    //rleNonCharacter = unicodeCn;
    //rleWhite = unicodeWhite_Space;
}

void outputCode(size_t N)(uint[N] value, string name)
{
	auto args = join(map!(x => to!string(x))(value[]), ",");
	formattedWrite(stderr.lockingTextWriter,
		`auto t%s = CodepointTrie!(%s)(unicodeSet("%s"));
		 formattedWrite(stderr.lockingTextWriter, 
			"auto best%s%s = CodepointTrie!(%s).fromRawArray(");`
		,  name, args, name, name, N, args);
	
	formattedWrite(stderr.lockingTextWriter, `
		 t%s.store(stderr.lockingTextWriter); 
		 formattedWrite(stderr.lockingTextWriter, ");\n");
		 `, name);
}

void test_all(alias fn)()
{   
	stderr.writeln("import std.stdio, std.conv, std.format, uni;\n void main(){");
	outputCode(fn(unicodeWhite_Space, "White"), "White_Space");
    outputCode(fn(rleAlpha, "Alpha"), "Alphabetic");
    outputCode(fn(rleMark, "Mark"), "Mark");
    outputCode(fn(rleNumber, "Number"), "Number");
    outputCode(fn(rlePunct, "Punctuation"), "Punctuation");
    outputCode(fn(rleSymbol, "Symbol"), "Symbol");
    outputCode(fn(unicodeZs, "Space"), "Space_Separator");
    outputCode(fn(rleGraphical, "Graphical"), "Graphical");
    outputCode(fn(unicodeCc, "Control"), "Control");
    outputCode(fn(unicodeCf, "Format"), "Format");
    outputCode(fn(unicodeCn, "Noncharacter"), "Cn");
    stderr.writeln("\n}");
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
	{
		//test_all!test_4_level();
		//test_all!test_3_level();
		test_all!test_2_level();
	}
}

uint[2] test_2_level(Set)(in Set set, string name)
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
	return blvl;
}

uint[3] test_3_level(Set)(in Set set, string name)
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
	return blvl;
}

uint[4] test_4_level(Set)(in Set set, string name)
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
	return blvl;
}
