//Written in the D programming language
/**
    gen_uni is a tool to automatically generate source code for unicode data structures.
    Call gen_uni > tables.d to generate based on latest and greatest from unicode.org.
    Notes:
        Uses std.net.curl thus needs curl shared lib & internet connection.

*/
import uni, std.stdio, std.traits, std.typetuple,
     std.exception, std.format, std.algorithm, std.typecons,
     std.regex, std.range, std.conv, std.net.curl;

import std.file:exists;
static import std.ascii;

alias RleBitSet!uint CodepointSet;

//common binary propertiy sets and their aliases
CodepointSet[string] props;
string[string] aliases;

//quick NO/MAYBE charaсter sets
CodepointSet[string] normalization;

//axuilary sets for case mapping
CodepointSet lowerCaseSet, upperCaseSet;

mixin(mixedCCEntry);

//case folding mapping
SimpleCaseEntry[] simpleTable;
FullCaseEntry[] fullTable;   

///canonical combining class
CodepointSet[256] combiningClass;
//same but packaged per dchar
ubyte[dchar] combiningMapping;

//unrolled decompositions
dstring[dchar] canonDecomp;
dstring[dchar] compatDecomp;

//canonical composition tables
dchar[] canonicalyComposableLeft;
dchar[] canonicalyComposableRight;

//canonical composition exclusions
CodepointSet compExclusions;

//property names to discard
string[] blacklist = [];

enum mixedCCEntry = `
struct SimpleCaseEntry
{
    uint ch;
    ubyte n, bucket;// n - number in bucket
    @property ubyte size() const
    {
        return bucket & 0x3F;
    }
    @property auto isLower() const
    {
        return bucket & 0x40;
    }
    @property auto isUpper() const
    {
        return bucket & 0x80;
    }
    this(uint dch, ubyte num, ubyte size, bool lower, bool upper)
    {
        ch = dch;
        n = num;
        bucket = size;
        if(lower)
            bucket |= 0x40;
        if(upper)
            bucket |= 0x80;
        
    }
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

struct CompEntry
{
    dchar rhs, composed;
}
`;

//size optimal sets
RleBitSet!ubyte[string] tinyProps;
RleBitSet!ushort[string] smallProps;
RleBitSet!uint[string] fullProps;

enum { 
    caseFoldingSrc = "CaseFolding.txt",
    blocksSrc = "Blocks.txt",
    propListSrc = "PropList.txt",
    generalPropSrc = "DerivedGeneralCategory.txt",
    corePropSrc = "DerivedCoreProperties.txt",
    normalizationPropSrc = "DerivedNormalizationProps.txt",
    scriptsSrc = "Scripts.txt",
    hangulSyllableSrc = "HangulSyllableType.txt",
    combiningClassSrc = "DerivedCombiningClass.txt",
    unicodeDataSrc = "UnicodeData.txt",
    compositionExclusionsSrc = "CompositionExclusions.txt"
};

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
    downloadIfNotExists(prefix~caseFoldingSrc, caseFoldingSrc);
    downloadIfNotExists(prefix~blocksSrc, blocksSrc);
    downloadIfNotExists(prefix~propListSrc, propListSrc);
    downloadIfNotExists(prefix~"extracted/"~generalPropSrc, generalPropSrc);
    downloadIfNotExists(prefix~corePropSrc, corePropSrc);
    downloadIfNotExists(prefix~scriptsSrc, scriptsSrc);
    downloadIfNotExists(prefix~normalizationPropSrc, normalizationPropSrc);
    downloadIfNotExists(prefix~hangulSyllableSrc, hangulSyllableSrc);
    downloadIfNotExists(prefix~compositionExclusionsSrc,compositionExclusionsSrc);
    downloadIfNotExists(prefix~"extracted/"~combiningClassSrc, combiningClassSrc);
    downloadIfNotExists(prefix~unicodeDataSrc, unicodeDataSrc);

    loadBlocks(blocksSrc);
    loadProperties(propListSrc);
    loadProperties(corePropSrc);
    loadProperties(generalPropSrc);
    loadProperties(scriptsSrc);
    loadProperties(hangulSyllableSrc);
    
    loadDecompositions(unicodeDataSrc);
    loadExclusions(compositionExclusionsSrc);
    loadCaseFolding(caseFoldingSrc);
    loadNormalization(normalizationPropSrc);
    loadCombining(combiningClassSrc);
    optimizeSets();


    writeProperties();
    writeNormalization();
    writeBeginPlatformDependent();
    writeTries();
    writeCombining();
    writeDecomposition();
    writeCompositionTable();
    writeEndPlatformDependent();
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

    //make some useful sets by hand

    lowerCaseSet = props["Lowercase"];
    upperCaseSet = props["Uppercase"];

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
            simpleTable ~= SimpleCaseEntry(value, cast(ubyte)i
                , cast(ubyte)size, value in lowerCaseSet, value in upperCaseSet);
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

    writeln("immutable simpleCaseTable = [");
    foreach(i, v; simpleTable)
        writefln("    SimpleCaseEntry(0x%04x, %s, %s, %s, %s)%s", v.ch, v.n, v.size, cast(bool)v.isLower, cast(bool)v.isUpper
                , i == simpleTable.length-1 ? "" : ",");
    writeln("];");
    
    writeln("immutable fullCaseTable = [");
    foreach(v; fullTable){
            if(v.entry_len > 1)
                assert(v.n >= 1); // meaning that start of bucket is always single char
            writefln("    FullCaseEntry(\"%s\", %s, %s),", v.value, v.n, v.size);
    }
    writeln("];");

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
            if(name !in props){
                props[name] = CodepointSet.init;
            }
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
    auto r = regex(`^(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(NFK?[CD]_QC)\s*;\s*([NM])|#\s*[a-zA-Z_0-9]+=([a-zA-Z_0-9]+)`);
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

void loadDecompositions(string inp)
{
    auto f = File(inp);
    foreach(line; f.byLine)
    {
        auto fields = split(line, ";");
        //codepoint, name, General_Category, Canonical_Combining_Class, Bidi_Class,
        //Decomp_Type&Mapping, 
        auto codepoint = fields[0];
        auto decomp = fields[5];
        if(!decomp.empty)
        {
            //stderr.writeln(codepoint, " ---> ", decomp);
            dchar src = parse!uint(codepoint, 16);
            dstring dest;
            bool compat = false;
            std.string.munch(decomp, " ");
            if(decomp.front == '<')
            {
                decomp = findSplitAfter(decomp, ">")[1];
                compat = true;
            }
            auto vals = split(decomp, " ");
            foreach(v; vals)
            {
                if(!v.empty)
                    dest ~= cast(dchar)parse!uint(v, 16);
            }
            if(!compat){
                assert(dest.length <= 2, "cannonical decomposition has more then 2 codepoints?!");
                canonDecomp[src] = dest;
            }
            compatDecomp[src] = dest;
        }
    }
}

auto recursivelyDecompose(dstring[dchar] decompTable)
{
    //apply recursively:
    dstring[dchar] full;
    foreach(k, v; decompTable)
    {
        dstring old, decomp=v;
        do
        {
            old = decomp;
            decomp = "";
            foreach(dchar ch; old)
                if(ch in decompTable)
                    decomp ~= decompTable[ch];
                else
                    decomp ~= ch;
        }while(old != decomp);
        full[k] = decomp;
    }
    return full;
}

void loadCombining(string inp)
{
    auto r = regex(`^(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([0-9]+)`);
    scanUniData!((m){
        auto clazz = m.captures[4];
        auto value = parse!uint(clazz);
        enforce(value <= 255, text("Corrupt combining class: ", clazz));
        if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            combiningClass[value].add(a, b+1);
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            combiningClass[value] |= x;
        }
    })(inp, r);
    
    foreach(i, clazz; combiningClass[1..255])//0 is a default for all of 1M+ codepoints
    {
        foreach(ch; clazz.byChar)
            combiningMapping[ch] = cast(ubyte)(i+1);
    }
}

void loadExclusions(string inp)
{
    auto r = regex(`^([0-9A-F]+)`);
    scanUniData!((m){
        auto piece = m.captures[1];
        uint a = parse!uint(piece, 16);
        compExclusions |= cast(dchar)a;   
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
    string tname = "immutable(UnicodeProperty!"~T.stringof~")";
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

void writeBeginPlatformDependent()
{
    version(LittleEndian)
        string endian = "LittleEndian";
    else
        string endian = "BigEndian";
    writefln("version (%s)
{
    static if(size_t.sizeof == %d)
    {
", endian, size_t.sizeof);
}

void writeEndPlatformDependent()
{
    version(LittleEndian)
        string endian = "LittleEndian";
    else
        string endian = "BigEndian";
    writeln("
    }
}");
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

void writeTries()
{
    
        
    ushort[dchar] simpleIndices;
    foreach(i, v; array(map!(x => x.ch)(simpleTable)))
        simpleIndices[v] = cast(ushort)i;

    ushort[dchar] fullIndices;
    foreach(i, v; fullTable)
    {
        if(v.entry_len == 1)
            fullIndices[v.ch] = cast(ushort)i;
    }

    //handpicked sizes, re-check later on (say with Unicode 7)
    //also hardcoded in a few places below
    auto st = CodepointTrie!(ushort, 12, 9)(simpleIndices, ushort.max);
    auto ft = CodepointTrie!(ushort, 12, 9)(fullIndices, ushort.max);

    foreach(k, v; simpleIndices){
        assert(st[k] == simpleIndices[k]);
    }

    foreach(k, v; fullIndices){
        assert(ft[k] == fullIndices[k]);
    }
    
    auto lowerCase = CodepointSetTrie!(10, 11)(lowerCaseSet);
    write("immutable lowerCaseTrie = CodepointSetTrie!(10, 11).fromRawArray(");
    lowerCase.store(stdout.lockingTextWriter());
    writeln(");");
    auto upperCase = CodepointSetTrie!(10, 11)(upperCaseSet);
    write("immutable upperCaseTrie = CodepointSetTrie!(10, 11).fromRawArray(");
    upperCase.store(stdout.lockingTextWriter());
    writeln(");");

    write("immutable simpleCaseTrie = CodepointTrie!(ushort, 12, 9).fromRawArray(");
    st.store(stdout.lockingTextWriter);
    writeln(");");
    write("immutable fullCaseTrie = CodepointTrie!(ushort, 12, 9).fromRawArray(");
    ft.store(stdout.lockingTextWriter);
    writeln(");");


}

void writeNormalization()
{
    foreach(key, value; normalization)
    {
        writefln("immutable %s = %s", key, charsetString(value));
    }
}

void writeDecomposition()
{
    auto fullCanon = recursivelyDecompose(canonDecomp);
    auto fullCompat = recursivelyDecompose(compatDecomp);
    auto decompCanonTable = assumeSorted(uniq(array(""d ~ fullCanon.values).sort()).array());
    auto decompCompatTable = assumeSorted(uniq(array(""d ~ fullCompat.values).sort()).array());
    
    ushort[dchar] mappingCanon;
    ushort[dchar] mappingCompat;
    //0 serves as doesn't decompose falue
    foreach(k, v; fullCanon)
    {
        size_t idx = decompCanonTable.lowerBound(v).length;
        assert(decompCanonTable[idx] == v);
        assert(idx != 0);
        mappingCanon[k] = cast(ushort)idx;
    }
    foreach(k, v; fullCompat)
    {
        size_t idx = decompCompatTable.lowerBound(v).length;
        assert(decompCompatTable[idx] == v);
        assert(idx != 0);
        mappingCompat[k] = cast(ushort)idx;
    }
    assert(decompCanonTable.length < 2^^16);
    assert(decompCompatTable.length < 2^^16);


    auto compatTrie = CodepointTrie!(ushort, 12, 9)(mappingCompat, 0);
    auto canonTrie =  CodepointTrie!(ushort, 12, 9)(mappingCanon, 0);
    
    foreach(k, v; fullCompat)
        assert(decompCompatTable[compatTrie[k]] == v);
    foreach(k, v; fullCanon)
    {
        //stderr.writefln("Index %d", canonTrie[k]);
        //stderr.writefln("%04X ~~~~> %( %04X %)", k, decompCanonTable[canonTrie[k]]);
        assert(decompCanonTable[canonTrie[k]] == v);
    }
    write("immutable compatMapping = CodepointTrie!(ushort, 12, 9).fromRawArray(");
    compatTrie.store(stdout.lockingTextWriter());
    writeln(");");
    write("immutable canonMapping = CodepointTrie!(ushort, 12, 9).fromRawArray(");
    canonTrie.store(stdout.lockingTextWriter());
    writeln(");");
    writeln("immutable decompCanonTable = ", decompCanonTable, ";");
    writeln("immutable decompCompatTable = ", decompCompatTable, ";");
}

void writeCompositionTable()
{
    dchar[dstring] composeTab;
    //construct compositions table
    foreach(dchar k, dstring v; canonDecomp)
    {
        if(v.length != 2)//singleton
            continue;
        if(v[0] in combiningMapping) //non-starter
            continue; 
        if(k in combiningMapping) //combines to non-starter
            continue; 
        if(compExclusions[k]) // non-derivable exclusions
            continue;
        composeTab[v] = k;
    }

    Tuple!(dchar, dchar, dchar)[] triples;
    foreach(dstring key, dchar val; composeTab)
        triples ~= Tuple!(dchar, dchar, dchar)(key[0], key[1], val);    
    multiSort!("a[0] < b[0]", "a[1] < b[1]")(triples);
    //map to the triplets array
    ushort[dchar] trimap;
    dchar old = triples[0][0]; 
    size_t idx = 0;
    auto r = triples[];
    for(;;){
        int cnt = countUntil!(x => x[0] != old)(r);
        if(cnt == -1)//end of input
            cnt = r.length;
        assert(idx < 2048);
        assert(cnt < 32);
        trimap[old] = to!ushort(idx | (cnt<<11));
        idx += cnt;
        if(idx == triples.length)
            break;
        old = r[cnt][0];
        r = r[cnt..$];
    }
    auto triT = CodepointTrie!(ushort, 12, 9)(trimap, ushort.max);
    auto dupletes = triples.map!(x => tuple(x[1], x[2])).array;
    foreach(dstring key, dchar val; composeTab)
    {
        size_t pack = triT[key[0]];
        assert(pack != ushort.max);        
        size_t idx = pack & ((1<<11)-1), cnt = pack>>11;
        auto f = dupletes[idx..idx+cnt].find!(x => x[0] == key[1]);
        assert(!f.empty);
        // & starts with the right value
        assert(f.front[1] == val);
    }
    //stderr.writeln("triT bytes: ", triT.bytes);
    //stderr.writeln("duplets bytes: ", dupletes.length*dupletes[0].sizeof);
    writeln("enum composeIdxMask = (1<<11)-1, composeCntShift = 11;");
    write("immutable compositionJumpTrie = CodepointTrie!(ushort, 12, 9).fromRawArray(");
    triT.store(stdout.lockingTextWriter());
    writeln(");");
    write("immutable compositionTable = [");
    foreach(pair; dupletes)
        writef("CompEntry(0x%05x, 0x%05x),", pair[0], pair[1]);
    writeln("];");
}

void writeCombining()
{
    auto ct = CodepointTrie!(ubyte, 7, 5, 9)(combiningMapping);
    foreach(i, clazz; combiningClass[1..255])//0 is a default for all of 1M+ codepoints
    {
        foreach(ch; clazz.byChar)
            assert(ct[ch] == i+1);
    }
    write("immutable combiningClassTrie = CodepointTrie!(ubyte, 7, 5, 9).fromRawArray(");
    ct.store(stdout.lockingTextWriter());
    writeln(");");

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
