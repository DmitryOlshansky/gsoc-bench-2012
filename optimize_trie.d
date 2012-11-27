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
    rleGraphical =  unicodeSet("Graphical");
    //rleAlpha | rleMark  | rleNumber | rlePunct | rleSpace | rleSymbol;
}

void outputCode(size_t N)(uint[N] value, string name)
{
	auto args = join(map!(x => to!string(x))(value[1..$]), ",");
	formattedWrite(stdout.lockingTextWriter,
		`auto t%s%s = CodepointSetTrie!(%s)(unicodeSet("%s"));
		 formattedWrite(stdout.lockingTextWriter, 
			"//%d bytes
auto best%s%s = CodepointSetTrie!(%s).fromRawArray(");`
		,  name, N-1, args, name, value[0], name, N-1, args);
	
	formattedWrite(stdout.lockingTextWriter, `
		 t%s%s.store(stdout.lockingTextWriter); 
		 formattedWrite(stdout.lockingTextWriter, ");\n");
		 `, name, N-1);
}

void process(alias fn, T)(in T set, string name)
{
    auto results = fn(set, name);
    sort!"a[0] < b[0]"(results);
    outputCode(results[0], name);
}

void preambula()
{
    writeln("import std.stdio, std.conv, std.format, uni;\n void main(){");
}

void coda()
{
    writeln("\n}");
}

void test_all(alias fn)()
{   
	process!fn(unicodeWhite_Space, "White_Space");
    process!fn(rleAlpha, "Alphabetic");
    process!fn(rleMark, "Mark");
    process!fn(rleNumber, "Number");
    process!fn(rlePunct, "Punctuation");
    process!fn(rleSymbol, "Symbol");
    process!fn(unicodeZs, "Space_Separator");
    process!fn(rleGraphical, "Graphical");
    process!fn(unicodeCc,  "Control");
    process!fn(unicodeCf, "Format");
    process!fn(unicodeCn, "Cn");
    
}

void main(string[] argv)
{
    preambula();
    version(level4)
        test_all!test_4_level();
    else version(level3)
        test_all!test_3_level();
    else version(level2)
        test_all!test_2_level();
    else{
        test_all!test_2_level();
        test_all!test_3_level();
        test_all!test_4_level();
    }
    coda();
}

uint[3][] test_2_level(Set)(in Set set, string name)
{
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    uint[3][] data;
    foreach(lvl_1; List)
    {
        enum lvl_2 = 21-lvl_1;
        alias CodepointSetTrie!(lvl_1, lvl_2) CurTrie;
        CurTrie t = CurTrie(set);
        data ~= [t.bytes, lvl_1, lvl_2];        
    }
	return data;
}

uint[4][] test_3_level(Set)(in Set set, string name)
{
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    uint[4][] data;

    foreach(lvl_1; List)
        foreach(lvl_2; List)
        {
            static if(lvl_1 + lvl_2  <= 16)
            {
                enum lvl_3 = 21-lvl_2-lvl_1;
                alias CodepointSetTrie!(lvl_1, lvl_2, lvl_3) CurTrie;
                CurTrie t = CurTrie(set);
                data ~= [t.bytes, lvl_1, lvl_2, lvl_3];                
            }
        }
	return data;
}

uint[5][] test_4_level(Set)(in Set set, string name)
{
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12);
    uint[5][] data;
    
    foreach(lvl_1; List)
        foreach(lvl_2; List)
            foreach(lvl_3; List)
        {
            static if(lvl_1 + lvl_2 + lvl_3  <= 16)
            {
                enum lvl_4 = 21-lvl_3-lvl_2-lvl_1;
                alias CodepointSetTrie!(lvl_1, lvl_2, lvl_3, lvl_4) CurTrie;
                CurTrie t = CurTrie(set);
                data ~= [t.bytes, lvl_1, lvl_2, lvl_3, lvl_4];
            }
        }
	return data;
}
