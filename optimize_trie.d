//Written in the D programming language
/**
    Simple script used to pick the best sizes of Tries.
    The same logic is copied over to gen_uni.d so it's mostly redundant.
*/
import std.stdio, std.algorithm, std.string, std.conv,
     std.typetuple, std.format, uni;

void process(alias fn)(string name)
{
    auto results = fn(name);
    sort!"a[0] < b[0]"(results);
}


auto test_2_level(string name)
{
    RleBitSet!uint set = unicode(name);
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    size_t[3][] data;
    size_t min = size_t.max;
    void delegate() print;    
    foreach(lvl_1; List)
    {
        enum lvl_2 = 21-lvl_1;       
        alias CodepointSetTrie!(lvl_1, lvl_2) CurTrie;
        CurTrie t = CurTrie(set);
        
        data ~= [t.bytes, lvl_1, lvl_2];
        if(t.bytes < min)
        {
            min = t.bytes;
            print = createPrinter!(lvl_1, lvl_2)(name, t);
        }
    }
    print();
    return data;
}

auto test_3_level(string name)
{
    RleBitSet!uint set = unicode(name);
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    size_t[4][] data;
    size_t min = size_t.max;
    void delegate() print;
    foreach(lvl_1; List)
    foreach(lvl_2; List)
    {
        static if(lvl_1 + lvl_2  <= 16)
        {
            enum lvl_3 = 21-lvl_2-lvl_1;
            alias CodepointSetTrie!(lvl_1, lvl_2, lvl_3) CurTrie;
            CurTrie t = CurTrie(set);
            data ~= [t.bytes, lvl_1, lvl_2, lvl_3]; 
            if(t.bytes < min)
            {
                min = t.bytes;
                print = createPrinter!(lvl_1, lvl_2, lvl_3)(name, t);
            }
        }
    }
    print();
    return data;
}

auto test_4_level(string name)
{
    RleBitSet!uint set = unicode(name);
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13);
    size_t[5][] data;
    size_t min = size_t.max;
    void delegate() print;
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
            if(t.bytes < min)
            {
                min = t.bytes;
                print = createPrinter!(lvl_1, lvl_2, lvl_3, lvl_4)(name, t);
            }
        }
    }
    print();
    return data;
}


void test_all(alias fn)()
{   
	process!fn("white_Space");
    process!fn("alphabetic");
    process!fn("mark");
    process!fn("number");
    process!fn("punctuation");
    process!fn("symbol");
    process!fn("space_Separator");
    process!fn("graphical");
    process!fn("control");
    process!fn("format");
    process!fn("cn");    
}

void main(string[] argv)
{
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
}


template createPrinter(Params...)
{
    void delegate() createPrinter(T)(string name, T trie)
    {
        return { 
            writef("//%d bytes\nauto %sTrieEntries = TrieEntry!(bool", trie.bytes, name);
            foreach(lvl; Params[0..$])
                writef(", %d", lvl);
            write(")(");
            trie.store(stdout.lockingTextWriter());
            writeln(");");
        };
    }
}
