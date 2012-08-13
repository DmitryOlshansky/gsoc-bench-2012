//Written in the D programming language
/**
    gen_uni is a tool to automatically generate source code for unicode data structures.
    Call gen_uni > tables.d to generate based on latest and greatest from unicode.org.
    Notes:
        Uses std.net.curl thus needs curl shared lib & internet connection.

*/
import uni, std.stdio, std.traits, std.typetuple,
     std.exception, std.format, std.algorithm,
     std.regex, std.range, std.conv, std.net.curl;

import std.file:exists;
static import std.ascii;

alias RleBitSet!uint CodepointSet;

CodepointSet[int] casefold;//entries by delta
CodepointSet[string] props;
string[string] aliases;
CodepointSet[string] normalization;

mixin(mixedCCEntry);
//case folding mapping
SimpleCaseEntry[] simpleTable;
FullCaseEntry[] fullTable;   


string[] blacklist = [];

enum mixedCCEntry = q{
struct SimpleCaseEntry
{
    uint ch;
    ubyte n, size;// n - number in batch, size - size of batch
}

struct FullCaseEntry
{
    union
    {
        dchar ch;
        dstring seq;
    }
    ubyte n, size;// n number in batch, size - size of batch
    ubyte entry_len;// ==1 read ch, >1 - seq

    @property auto value()const
    { 
        return entry_len == 1 ? (&ch)[0..1] : cast(dstring)seq;
    }

    this(dstring value, ubyte num, ubyte batch_size)
    {
        assert(value.length < 255);
        entry_len = cast(ubyte)value.length;
        if(value.length == 1)
            ch = value[0];
        else{
            seq = value;
        }
        n = num;
        size = batch_size;
    }
}

};


//size optimal sets
RleBitSet!ubyte[string] tinyProps;
RleBitSet!ushort[string] smallProps;
RleBitSet!uint[string] fullProps;

void main(string[] argv)
{
    writeln("//Written in the D programming language
/**
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
//Automatically generated from Unicode Character Database files
import uni;\n");
    auto prefix = "http://www.unicode.org/Public/UNIDATA/";
    downloadIfNotExists(prefix~"CaseFolding.txt"
        , "Casefolding.txt");
    downloadIfNotExists(prefix~"Blocks.txt"
        , "Blocks.txt");
    downloadIfNotExists(prefix~"PropList.txt"
        , "PropList.txt");
    downloadIfNotExists(prefix~"extracted/DerivedGeneralCategory.txt"
        , "DerivedGeneralCategory.txt");
    downloadIfNotExists(prefix~"DerivedCoreProperties.txt"
        , "DerivedCoreProperties.txt");
    downloadIfNotExists(prefix ~ "Scripts.txt"
        , "Scripts.txt");
    downloadIfNotExists(prefix ~ "DerivedNormalizationProps.txt"
        , "DerivedNormalizationProps.txt");
    loadCaseFolding("CaseFolding.txt");
    loadBlocks("Blocks.txt");
    loadProperties("PropList.txt");
    loadProperties("DerivedGeneralCategory.txt");
    loadProperties("DerivedCoreProperties.txt");
    loadProperties("Scripts.txt");
    loadNormalization("DerivedNormalizationProps.txt");

    optimizeSets();
/*
    writeCaseFolding();
    writeProperties();
    writeNormalization();*/
}


void scanUniData(alias Fn)(string name, Regex!char r)
{
    foreach(line; File(name).byLine)
    {
        auto m = match(line, r);
        if(!m.empty)
            Fn(m);
    }
}


void downloadIfNotExists(string url, string path)
{
    if(!exists(path))
        download(url, path);
}


void loadCaseFolding(string f)
{
    dchar[dchar] simple;
    dstring[dchar] full;

    auto r = regex("([^;]*); ([CFS]);\\s*([^;]*);");
    scanUniData!((m){
            auto s1 = m.captures[1];
            auto code = m.captures[2].front;
            auto s2 = m.captures[3];
            auto left = parse!int(s1, 16);
            if(code == 'C')
            {
                auto right = parse!int(s2, 16);
                simple[left] = right;
                full[left] = [right];
            }     
            else if(code == 'S')                   
            {
                auto right = parse!int(s2, 16);
                simple[left] = right;
            }
            else if(code == 'F')
            {
                dstring right;
                foreach(x; match(s2, regex("[0-9A-Fa-f]+", "g")))
                {
                    right ~= to!int(x[0], 16);
                }
                full[left] = right.idup;
            }
    })(f, r);

    write(mixedCCEntry);
    foreach(ch; simple.keys()){
        dchar[8] entry;
        int size=0;
        entry[size++] = ch;
        dchar x = simple[ch];
        entry[size++] = x;
        //simple is many:1 mapping
        foreach(key, v; simple){
            if(v == x && !canFind(entry[], key)){
                entry[size++] = key;
            }
        }    
        foreach(i, value; entry[0..size]){
            simpleTable ~= SimpleCaseEntry(value, cast(ubyte)i, cast(ubyte)size);
        }
    }

    foreach(ch; full.keys()){
        dstring[8] entry;
        int size=0;
        entry[size++] = [ch];
        auto x = full[ch];
        entry[size++] = x;
        
        //full is many:1 mapping
        foreach(key, v; full){
            if(v == x && !canFind(entry[], [key])){
                entry[size++] = [key];
            }

        }    
        foreach(i, value; entry[0..size]){
            fullTable ~= FullCaseEntry(value, cast(ubyte)i, cast(ubyte)size);
        }
    }

    //handpicked, re-check later on (say with Unicode 7)
    //also hardcoded in a few places below
    alias uni.Trie!(ushort, dchar, sliceBits!(9, 21), sliceBits!(0, 9)) MyTrie;
    
    ushort[dchar] simpleIndices;
    foreach(i, v; array(map!(x => x.ch)(simpleTable)))
        simpleIndices[v] = cast(ushort)i;

    ushort[dchar] fullIndices;
    foreach(i, v; fullTable)
    {
        if(v.entry_len == 1)
            fullIndices[v.ch] = cast(ushort)i;
    }

    auto st = MyTrie(simpleIndices, ushort.max);
    auto ft = MyTrie(fullIndices, ushort.max);

    foreach(k, v; simpleIndices){
        assert(st[k] == simpleIndices[k]);
    }

    foreach(k, v; fullIndices){
        assert(ft[k] == fullIndices[k]);
    }

    writeln("immutable simpleCaseTable = [");
    foreach(i, v; simpleTable)
        writeln("    ", v, i == simpleTable.length-1 ? "" : ", ");
    writeln("];");
    
    writeln("immutable fullCaseTable = [");
    foreach(v; fullTable){
            if(v.entry_len > 1)
                assert(v.n >= 1); // meaning that start of bucket is always single char
            writefln("    FullCaseEntry(\"%s\", %s, %s),", v.value, v.n, v.size);
    }
    writeln("];");

    write("immutable simpleCaseTrie = Trie!(ushort, dchar, sliceBits!(9, 21), sliceBits!(0, 9)).fromRawArray(");
    st.store(stdout.lockingTextWriter);
    writeln(");");
    write("immutable fullCaseTrie = Trie!(ushort, dchar, sliceBits!(9, 21), sliceBits!(0, 9)).fromRawArray(");
    ft.store(stdout.lockingTextWriter);
    writeln(");");
}

void loadBlocks(string f)
{
    auto r = regex(`^([0-9A-F]+)\.\.([0-9A-F]+);\s*(.*)\s*$`);
    scanUniData!((m){
            auto s1 = m.captures[1];
            auto s2 = m.captures[2];
            auto a1 = parse!uint(s1, 16);
            auto a2 = parse!uint(s2, 16);
            props["In"~to!string(m.captures[3])] = CodepointSet([a1, a2+1]);
    })(f, r);
}

void loadProperties(string inp)
{
    auto r = regex(`^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([a-zA-Z_0-9]*)\s*#|# [a-zA-Z_0-9]+=([a-zA-Z_0-9]+))`);
    string aliasStr;
    scanUniData!((m){
        auto name = to!string(m.captures[4]);
        if(!m.captures[5].empty)
            aliasStr = to!string(m.captures[5]);
        else if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            if(name !in props)
                props[name] = CodepointSet.init;
            props[name].add(a,b+1); // unicode lists [a, b] we need [a,b)
            if(!aliasStr.empty)
            {
                aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in props)
                props[name] = CodepointSet.init;
            props[name] |= x;
            if(!aliasStr.empty)
            {
                aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
    })(inp, r);
}

void loadNormalization(string inp)
{
    auto r = regex(`^(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(NFC_QC)\s*;\s*([NM])|#\s*[a-zA-Z_0-9]+=([a-zA-Z_0-9]+)`);
    string aliasStr;
    scanUniData!((m){
        auto name = to!string(m.captures[4]) ~ to!string(m.captures[5]);
        /*if(!m.captures[6].empty)
            aliasStr = to!string(m.captures[6]);
        else*/ if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            if(name !in normalization)
                normalization[name] = CodepointSet.init;
            normalization[name].add(a,b+1);
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in normalization)
                normalization[name] = CodepointSet.init;
            normalization[name] |= x;
        }
        //stderr.writeln(m.hit);
    })(inp, r);
}

string charsetString(T)(in RleBitSet!T set, string sep=";\n")
{
    auto app = appender!(char[])();
    auto raw = appender!(T[]);
    set.store(raw);
    formattedWrite(app, "RleBitSet!"~T.stringof
        ~".fromRawArray([%(0x%x, %)]);", raw.data);
    return cast(string)app.data;
}

void optimizeSets()
{
    foreach(k, v; props)
    {
        if(countUntil(blacklist, k) < 0  && !k.startsWith("Changes"))
        {
            RleBitSet!ubyte tiny = v;
            RleBitSet!ushort small = v;
            if(tiny.bytes < small.bytes){
                if(tiny.bytes < v.bytes)
                    tinyProps[k] = tiny;
                else
                    fullProps[k] = v;
            }
            else if(small.bytes < v.bytes)
            {
                smallProps[k] = small;
            }
            else 
                fullProps[k] = v;
        }
    }
    /*stderr.writeln("TINY");
    foreach(k,v; tinyProps)
        stderr.writeln(k, ", ", v.bytes);
    stderr.writeln("\nSMALL");
    foreach(k,v; smallProps)
        stderr.writeln(k, ", ", v.bytes);
    stderr.writeln("\nFULL");
    foreach(k,v; fullProps)
        stderr.writeln(k, ", ", v.bytes);*/
}

void writeCaseFolding()
{

}

string identName(string s)
{
    auto app = appender!(char[])();
    foreach(c; s)
        if(c == '-' || c == ' ')
            app.put('_');
        else
            app.put(c);
    return cast(string)app.data;
}

string uniformName(string s)
{
    auto app = appender!(char[])();
    foreach(c; s)
        if(c != '-' && c != ' ' && c != '_')
            app.put(toLower(c));
    return cast(string)app.data;
}

void printSetTable(SetHash)(SetHash hash)
 {  
    foreach(k, v; hash)
    {
        writef("immutable unicode%s = ", identName(k));
        writeln(charsetString(v));
    }
}



void printPropertyTable(T)(RleBitSet!T[string] hash, string tabname)
{
    string tname = "UnicodeProperty!"~T.stringof;
    writef("\nimmutable %s[] %s = [\n", tname, tabname);
    string[] lines;
    string[] namesOnly;
    auto app = appender!(char[])();
    auto keys = hash.keys;
    foreach(k; keys)
    {
        formattedWrite(app, "%s(\"%s\", unicode%s),\n"
            , tname, k, identName(k));        
        lines ~= app.data.idup;
        namesOnly ~= uniformName(k);
        app.shrinkTo(0);
        if(k in aliases)
        {
            formattedWrite(app, "%s(\"%s\", unicode%s),\n"
                , tname, aliases[k], identName(k));
            lines ~= app.data.idup;
            namesOnly ~= uniformName(aliases[k]);
            app.shrinkTo(0);
        }
    }
    static bool ucmp(T)(T a, T b) { return propertyNameLess(a[0], b[0]); }
    sort!ucmp(zip(namesOnly, lines));

    foreach(i, v; lines)
    {
        write(lines[i]);
    }

    writeln("];");
}

void writeProperties()
{
    writeln("struct UnicodeProperty(T)
{
    string name;
    RleBitSet!T set;
}");
   
    printSetTable(tinyProps);
    printSetTable(smallProps);
    printSetTable(fullProps);
    printPropertyTable(tinyProps, "tinyUnicodeProps");
    printPropertyTable(smallProps, "smallUnicodeProps");
    printPropertyTable(fullProps, "fullUnicodeProps");
}


void writeNormalization()
{
    foreach(key, value; normalization)
    {
        writef("immutable %s = %s", key, charsetString(value));
    }
}


//fussy compare for unicode property names as per UTS-18
int comparePropertyName(Char)(const(Char)[] a, const(Char)[] b)
{
    for(;;)
    {
        while(!a.empty && (isWhite(a.front) || a.front == '-' || a.front =='_'))
        {
            a.popFront();
        }
        while(!b.empty && (isWhite(b.front) || b.front == '-' || b.front =='_'))
        {
            b.popFront();
        }
        if(a.empty)
            return b.empty ? 0 : -1;
        if(b.empty)
            return 1;
        auto ca = toLower(a.front), cb = toLower(b.front);
        if(ca > cb)
            return 1;
        else if( ca < cb)
            return -1;
        a.popFront();
        b.popFront();
    }
}

bool propertyNameLess(Char)(const(Char)[] a, const(Char)[] b)
{
    return comparePropertyName(a, b) < 0;
}
