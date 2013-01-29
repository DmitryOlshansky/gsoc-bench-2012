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

//common binary property sets and their aliases
struct PropertyTable
{
    CodepointSet[string] table;
    string[string] aliases;
}

PropertyTable general;
PropertyTable blocks;
PropertyTable scripts;
PropertyTable hangul;

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
    @property ubyte size() const @safe pure nothrow
    {
        return bucket & 0x3F;
    }
    @property auto isLower() const @safe pure nothrow
    {
        return bucket & 0x40;
    }
    @property auto isUpper() const @safe pure nothrow
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

    @property auto value() const  @trusted pure nothrow
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

struct UnicodeProperty
{
    string name;
    ubyte[] compressed;
}

struct TrieEntry(T...)
{
    size_t[] offsets;
    size_t[] sizes;
    size_t[] data;
}

`;

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
    try
    {
        writeln("//Written in the D programming language
/**
 * License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
 *
 * Authors: Dmitry Olshansky
 *
 */
//Automatically generated from Unicode Character Database files\n");
        auto prefix = "http://www.unicode.org/Public/UNIDATA/";
        downloadIfNotCached(prefix~caseFoldingSrc, caseFoldingSrc);
        downloadIfNotCached(prefix~blocksSrc, blocksSrc);
        downloadIfNotCached(prefix~propListSrc, propListSrc);
        downloadIfNotCached(prefix~"extracted/"~generalPropSrc, generalPropSrc);
        downloadIfNotCached(prefix~corePropSrc, corePropSrc);
        downloadIfNotCached(prefix~scriptsSrc, scriptsSrc);
        downloadIfNotCached(prefix~normalizationPropSrc, normalizationPropSrc);
        downloadIfNotCached(prefix~hangulSyllableSrc, hangulSyllableSrc);
        downloadIfNotCached(prefix~compositionExclusionsSrc,compositionExclusionsSrc);
        downloadIfNotCached(prefix~"extracted/"~combiningClassSrc, combiningClassSrc);
        downloadIfNotCached(prefix~unicodeDataSrc, unicodeDataSrc);
        
        loadBlocks(blocksSrc, blocks);
        loadProperties(propListSrc, general);
        loadProperties(corePropSrc, general);
        loadProperties(generalPropSrc, general);
        loadProperties(scriptsSrc, scripts);
        loadProperties(hangulSyllableSrc, hangul);
        
        loadDecompositions(unicodeDataSrc);
        loadExclusions(compositionExclusionsSrc);        
        loadCaseFolding(caseFoldingSrc);
        loadNormalization(normalizationPropSrc);
        loadCombining(combiningClassSrc);
        
        static void writeTableOfSets(string prefix, 
            string name, PropertyTable tab)
        {
            writeln();
            writeSets(prefix, tab);
            writeAliasTable(prefix, name, tab);   
        }

        writeTableOfSets("uniProp", "propsTab", general);
        writeTableOfSets("block", "blocksTab", blocks);
        writeTableOfSets("script", "scriptsTab", scripts);
        writeTableOfSets("hangul", "hangulTab", hangul);

        writefln("
static if(size_t.sizeof == %d) {", size_t.sizeof);        
        writeTries();
        writeCombining();
        writeDecomposition();
        writeCompositionTable();
        writeln("
}\n");
        writeFunctions();
    }
    catch(Exception e)
    {
        stderr.writeln(e.msg);
    }
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


void downloadIfNotCached(string url, string path)
{
    //TODO: check current date vs file data before using cache
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

    lowerCaseSet = general.table["Lowercase"];
    upperCaseSet = general.table["Uppercase"];

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

void loadBlocks(string f, ref PropertyTable target)
{
    auto r = regex(`^([0-9A-F]+)\.\.([0-9A-F]+);\s*(.*)\s*$`);
    scanUniData!((m){
            auto s1 = m.captures[1];
            auto s2 = m.captures[2];
            auto a1 = parse!uint(s1, 16);
            auto a2 = parse!uint(s2, 16);
            //@@@BUG 6178 memory corruption with
            //target[to!string(m.captures[3])] = CodepointSet(a1, a2+1);
            auto set = CodepointSet(a1, a2+1);
            target.table[to!string(m.captures[3])] = set;
    })(f, r);
}

void loadProperties(string inp, ref PropertyTable target)
{
    auto acceptProp = (string name) => countUntil(blacklist, name) < 0  && !name.startsWith("Changes");
    auto r = regex(`^(?:(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*([a-zA-Z_0-9]*)\s*#|# [a-zA-Z_0-9]+=([a-zA-Z_0-9]+))`);
    string aliasStr;
    auto set = CodepointSet.init;  //workaround @@@BUG 6178
    scanUniData!((m){
        auto name = to!string(m.captures[4]);
        if(!acceptProp(name)) 
            return;
        if(!m.captures[5].empty)
            aliasStr = to!string(m.captures[5]);
        else if(!m.captures[1].empty)
        {
            auto sa = m.captures[1];
            auto sb = m.captures[2];
            uint a = parse!uint(sa, 16);
            uint b = parse!uint(sb, 16);
            if(name !in target.table)
            {         
                target.table[name] = set;
            }
            target.table[name].add(a,b+1); // unicode lists [a, b] we need [a,b)
            if(!aliasStr.empty)
            {
                target.aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in target.table)
            {
                target.table[name] = set;
            }
            target.table[name] |= x;
            if(!aliasStr.empty)
            {
                target.aliases[name] = aliasStr;
                aliasStr = "";
            }
        }
    })(inp, r);
}

void loadNormalization(string inp)
{
    auto r = regex(`^(?:([0-9A-F]+)\.\.([0-9A-F]+)|([0-9A-F]+))\s*;\s*(NFK?[CD]_QC)\s*;\s*([NM])|#\s*[a-zA-Z_0-9]+=([a-zA-Z_0-9]+)`);
    string aliasStr;
    CodepointSet set; //workaround @@@BUG 6178
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
            {                
                normalization[name] = set;
            }
            normalization[name].add(a,b+1);
        }
        else if(!m.captures[3].empty)
        {
            auto sx = m.captures[3];
            uint x = parse!uint(sx, 16);
            if(name !in normalization)
            {
                normalization[name] = set;
            }
            normalization[name] |= x;
        }
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
    auto arr = combiningClass[1..255];
    foreach(i, clazz; arr)//0 is a default for all of 1M+ codepoints
    {
        auto y = clazz.byCodepoint;
        foreach(ch; y)
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

string charsetString(CodepointSet set, string sep=";\n")
{
    auto app = appender!(char[])();
    ubyte[] data = compressIntervals(set.byInterval);
    assert(CodepointSet(decompressIntervals(data)) == set);
    formattedWrite(app, "[%(0x%x, %)];", data);
    return cast(string)app.data;
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
            app.put(std.ascii.toLower(c));
    return cast(string)app.data;
}

void writeSets(string prefix, PropertyTable src)
 {  
    foreach(k, v; src.table)
    {
        writef("immutable ubyte[] %s%s = ", prefix, identName(k));
        writeln(charsetString(v));
    }
}

void writeAliasTable(string prefix, string tabname, PropertyTable src)
{
    string tname = "immutable(UnicodeProperty)";
    writef("\nimmutable %s[] %s = [\n", tname, tabname);
    string[] lines;
    string[] namesOnly;
    auto app = appender!(char[])();
    auto keys = src.table.keys;
    foreach(k; keys)
    {
        formattedWrite(app, "%s(\"%s\", %s%s),\n",
            tname, k, prefix, identName(k));        
        lines ~= app.data.idup;
        namesOnly ~= uniformName(k);
        app.shrinkTo(0);
        if(k in src.aliases)
        {
            formattedWrite(app, "%s(\"%s\", %s%s),\n",
                tname, src.aliases[k], prefix, identName(k));
            lines ~= app.data.idup;
            namesOnly ~= uniformName(src.aliases[k]);
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

    //these 2 only for verification of Trie code itself
    auto st = codepointTrie!(ushort, 12, 9)(simpleIndices, ushort.max);
    auto ft = codepointTrie!(ushort, 12, 9)(fullIndices, ushort.max);

    foreach(k, v; simpleIndices){
        assert(st[k] == simpleIndices[k]);
    }

    foreach(k, v; fullIndices){
        assert(ft[k] == fullIndices[k]);
    }

    writeBest3Level("lowerCase", lowerCaseSet);
    writeBest3Level("upperCase", upperCaseSet);
    writeBest3Level("simpleCase", simpleIndices, ushort.max);
    writeBest3Level("fullCase", fullIndices, ushort.max);

    //common isXXX properties
    auto props = general.table;
    CodepointSet alpha = props["Alphabetic"]; //it includes some numbers, symbols & marks
    CodepointSet mark = props["Mn"] | props["Me"] | props["Mc"];
    CodepointSet number = props["Nd"] | props["Nl"] | props["No"];
    CodepointSet punctuation = props["Pd"] | props["Ps"] | props["Pe"]
         | props["Pc"] | props["Po"] | props["Pi"] | props["Pf"];
    CodepointSet symbol = props["Sm"] | props["Sc"] | props["Sk"] | props["So"];
    CodepointSet graphical = alpha | mark | number | punctuation | symbol | props["Zs"];
    CodepointSet nonCharacter = props["Cn"];
 
    CodepointSet nfcQC = normalization["NFC_QCN"] | normalization["NFC_QCM"];
    CodepointSet nfdQC = normalization["NFD_QCN"];
    CodepointSet nfkcQC = normalization["NFKC_QCN"] | normalization["NFKC_QCM"];
    CodepointSet nfkdQC = normalization["NFKD_QCN"];
    writeBest3Level("alpha", alpha);
    writeBest3Level("mark", mark);
    writeBest3Level("number", number);
    writeBest3Level("punctuation", punctuation);
    writeBest3Level("symbol", symbol);
    writeBest3Level("graphical", graphical);
    writeBest4Level("nonCharacter", nonCharacter);

    writeBest3Level("nfcQC", nfcQC);
    writeBest3Level("nfdQC", nfdQC);
    writeBest3Level("nfkcQC", nfkcQC);
    writeBest3Level("nfkdQC", nfkdQC);

    //few specifics for grapheme cluster breaking algorithm
    writeBest3Level("mc", props["Mc"]);
    writeBest3Level("graphemeExtend", props["Grapheme_Extend"]);
}

void writeDecomposition()
{
    auto fullCanon = recursivelyDecompose(canonDecomp);
    auto fullCompat = recursivelyDecompose(compatDecomp);
    dstring decompCanonFlat = "\0"~array(fullCanon.values).sort.uniq.join("\0")~"\0";
    dstring decompCompatFlat = "\0"~array(fullCompat.values).sort.uniq.join("\0")~"\0";
    stderr.writeln("Canon flattened: ", decompCanonFlat.length);
    stderr.writeln("Compat flattened: ", decompCompatFlat.length);
   
    ushort[dchar] mappingCanon;
    ushort[dchar] mappingCompat;
    //0 serves as doesn't decompose value
    foreach(k, v; fullCanon)
    {
        size_t idx = decompCanonFlat.countUntil(v~"\0");          
        enforce(idx != 0);
        enforce(decompCanonFlat[idx..idx+v.length] == v);        
        mappingCanon[k] = cast(ushort)idx;
    }
    foreach(k, v; fullCompat)
    {
        size_t idx = decompCompatFlat.countUntil(v~"\0");
        enforce(idx != 0);
        enforce(decompCompatFlat[idx..idx+v.length] == v);        
        mappingCompat[k] = cast(ushort)idx;
    }
    enforce(decompCanonFlat.length < 2^^16);
    enforce(decompCompatFlat.length < 2^^16);

    //these 2 are just self-test for Trie template code
    auto compatTrie = codepointTrie!(ushort, 12, 9)(mappingCompat, 0);
    auto canonTrie =  codepointTrie!(ushort, 12, 9)(mappingCanon, 0);
    import std.string;
    foreach(k, v; fullCompat)
    {
        auto idx = compatTrie[k];
        enforce(idx == mappingCompat[k], "failed on compat");
        size_t len = decompCompatFlat[idx..$].countUntil(0);
        enforce(decompCompatFlat[idx..idx+len] == v, 
            format("failed on compat: '%( 0x0%5x %)' not found", v));
    }
    foreach(k, v; fullCanon)
    {
        auto idx = canonTrie[k];
        enforce(idx == mappingCanon[k], "failed on canon");
        size_t len = decompCanonFlat[idx..$].countUntil(0);
        enforce(decompCanonFlat[idx..idx+len] == v,
            format("failed on canon: '%( 0x%5x %)' not found", v));
    }

    writeBest3Level("compatMapping", mappingCompat, cast(ushort)0);
    writeBest3Level("canonMapping", mappingCanon, cast(ushort)0);    
    writefln("immutable dchar[] decompCanonTable = [%( 0x%x, %)];", decompCanonFlat);
    writefln("immutable dchar[] decompCompatTable = [%( 0x%x, %)];", decompCompatFlat);
}

void writeFunctions()
{
    auto format = general.table["Cf"];
    auto space = general.table["Zs"];
    auto control = general.table["Cc"];    
    auto whitespace = general.table["White_Space"];

    //hangul L, V, T
    auto hangL = hangul.table["L"];
    auto hangV = hangul.table["V"];
    auto hangT = hangul.table["T"];

    writeln(format.toSourceCode("isFormatGen"));
    writeln(control.toSourceCode("isControlGen"));
    writeln(space.toSourceCode("isSpaceGen"));
    writeln(whitespace.toSourceCode("isWhiteGen"));
    writeln(hangL.toSourceCode("isHangL"));
    writeln(hangV.toSourceCode("isHangV"));
    writeln(hangT.toSourceCode("isHangT"));
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
        ptrdiff_t cnt = countUntil!(x => x[0] != old)(r);
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
    auto triT = codepointTrie!(ushort, 12, 9)(trimap, ushort.max);
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
    writeln("enum composeIdxMask = (1<<11)-1, composeCntShift = 11;");
    write("immutable compositionJumpTrieEntries = TrieEntry!(ushort, 12, 9)(");
    triT.store(stdout.lockingTextWriter());
    writeln(");");
    write("immutable compositionTable = [");
    foreach(pair; dupletes)
        writef("CompEntry(0x%05x, 0x%05x),", pair[0], pair[1]);
    writeln("];");
}

void writeCombining()
{
    auto ct = codepointTrie!(ubyte, 7, 5, 9)(combiningMapping);
    foreach(i, clazz; combiningClass[1..255])//0 is a default for all of 1M+ codepoints
    {
        foreach(ch; clazz.byCodepoint)
            assert(ct[ch] == i+1);
    }
    writeBest3Level("combiningClass", combiningMapping);
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
		// names are all in ASCII either way though whitespace might be unicode
        auto ca = std.ascii.toLower(a.front), cb = std.ascii.toLower(b.front);
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

//meta helpers to generate and pick the best trie by size & levels

void writeBest2Level(Set)( string name, Set set)
    if(isCodepointSet!Set)
{
    alias List = TypeTuple!(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    size_t min = size_t.max;
    void delegate() write;    
    foreach(lvl_1; List)
    {
        enum lvl_2 = 21-lvl_1;       
        auto t = codepointSetTrie!(lvl_1, lvl_2)(set);
        if(t.bytes < min)
        {
            min = t.bytes;
            write = createPrinter!(lvl_1, lvl_2)(name, t);
        }
    }
    write();
}

void writeBest2Level(V, K)(string name, V[K] map, V defValue=V.init)
{
    alias List = TypeTuple!(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
    size_t min = size_t.max;
    void delegate() write;    
    foreach(lvl_1; List)
    {
        enum lvl_2 = 21-lvl_1;       
        alias codepointTrie!(V, lvl_1, lvl_2) CurTrie;
        CurTrie t = CurTrie(map, defValue);
        if(t.bytes < min)
        {
            min = t.bytes;
            write = createPrinter!(lvl_1, lvl_2)(name, t);
        }
    }
    write();
}

alias List_1 = TypeTuple!(4, 5, 6, 7, 8);

auto writeBest3Level(Set)(string name, Set set)
    if(isCodepointSet!Set)
{
    // access speed trumps size, power of 2 is faster to access
    // e.g. 9, 5, 7 is far slower then 8, 5, 8 because of how bits breakdown:
    // 8-5-8: indexes are 21-8 = 13 bits, 13-5 = 8 bits, fits into a byte
    // 9-5-7: indexes are 21-7 = 14 bits, 14-5 = 9 bits, doesn't fit into a byte (!)

    // e.g. 8-5-8 is one of hand picked that is a very close match 
    // to the best packing
    void delegate() write;
    
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    size_t min = size_t.max;        
    foreach(lvl_1; List_1)//to have the first stage index fit in byte
    foreach(lvl_2; List)
    {
        static if(lvl_1 + lvl_2  <= 16)//so that 2nd stage fits in ushort
        {
            enum lvl_3 = 21-lvl_2-lvl_1;
            auto t = codepointSetTrie!(lvl_1, lvl_2, lvl_3)(set);
            if(t.bytes < min)
            {
                min = t.bytes;
                write = createPrinter!(lvl_1, lvl_2, lvl_3)(name, t);
            }
        }
    }
    write();
}

void writeBest3Level(V, K)(string name, V[K] map, V defValue=V.init)
{
    void delegate() write;
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    size_t min = size_t.max;
    foreach(lvl_1; List_1)//to have the first stage index fit in byte
    foreach(lvl_2; List)
    {
        static if(lvl_1 + lvl_2  <= 16)// into ushort
        {
            enum lvl_3 = 21-lvl_2-lvl_1;
            auto t = codepointTrie!(V, lvl_1, lvl_2, lvl_3) (map, defValue);
            if(t.bytes < min)
            {
                min = t.bytes;
                write = createPrinter!(lvl_1, lvl_2, lvl_3)(name, t);
            }
        }
    }
    write();
}

void writeBest4Level(Set)(string name, Set set)
{
    alias List = TypeTuple!(4, 5, 6, 7, 8, 9, 10, 11, 12, 13);
    size_t min = size_t.max;
    void delegate() write;
    foreach(lvl_1; List_1)//to have the first stage index fit in byte
    foreach(lvl_2; List)
    foreach(lvl_3; List)
    {
        static if(lvl_1 + lvl_2 + lvl_3  <= 16)
        {
            enum lvl_4 = 21-lvl_3-lvl_2-lvl_1;
            auto t = codepointSetTrie!(lvl_1, lvl_2, lvl_3, lvl_4)(set);
            if(t.bytes < min)
            {
                min = t.bytes;
                write = createPrinter!(lvl_1, lvl_2, lvl_3, lvl_4)(name, t);
            }
        }
    }
    write();
}

template createPrinter(Params...)
{
    void delegate() createPrinter(T)(string name, T trie)
    {
        return { 
            writef("//%d bytes\nimmutable %sTrieEntries = TrieEntry!(%s", 
                trie.bytes, name, Unqual!(typeof(T.init[0])).stringof);
            foreach(lvl; Params[0..$])
                writef(", %d", lvl);
            write(")(");
            trie.store(stdout.lockingTextWriter());
            writeln(");");
        };
    }
}
