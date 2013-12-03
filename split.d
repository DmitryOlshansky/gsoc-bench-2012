/*
    Benchmarking strign split performance 
    decode + isWhite vs UTF-matcher(White_Space).
*/
import std.traits, std.uni;

S[] stdSplit(S)(S s)
{
    size_t istart;
    bool inword = false;
    S[] result;

    foreach (i, dchar c ; s)
    {
        if (std.uni.isWhite(c))
        {
            if (inword)
            {
                result ~= s[istart .. i];
                inword = false;
            }
        }
        else
        {
            if (!inword)
            {
                istart = i;
                inword = true;
            }
        }
    }
    if (inword)
        result ~= s[istart .. $];
    return result;
}

typeof(utfMatcher!char(unicode.White_Space)) mWhite8;

static this()
{
    mWhite8 = utfMatcher!char(unicode.White_Space);
}

S[] newSplit(S)(S s)
    if (isSomeString!S)
{
    S[] result;
    bool inword=false;
    size_t restLen;
    auto r = s;
    while (s.length)
    {
        if (mWhite8.skip(s))
        {
            if (inword)
            {
                result ~= r[0..$-restLen];
            }
            inword = false;
            r = s;
        }
        else
        {
            inword = true;
            restLen = s.length;
        }
    }
    if (inword)
        result ~= r[0..$-restLen];
    return result;
}

unittest
{
    import std.typecons, std.typetuple, std.conv, std.string;
    static auto makeEntry(S)(string l, string[] r)
    {return tuple(l.to!S(), r.to!(S[])());}

    foreach (S; TypeTuple!(string))
    {
        auto entries =
        [
            makeEntry!S("", []),
            makeEntry!S(" ", []),
            makeEntry!S("hello", ["hello"]),
            makeEntry!S(" hello ", ["hello"]),
            makeEntry!S("  h  e  l  l  o ", ["h", "e", "l", "l", "o"]),
            makeEntry!S("peter\t\npaul\rjerry", ["peter", "paul", "jerry"]),
            makeEntry!S(" \t\npeter paul\tjerry \n", ["peter", "paul", "jerry"]),
            makeEntry!S("\u2000日\u202F本\u205F語\u3000", ["日", "本", "語"]),
            makeEntry!S("　　哈・郎博尔德｝　　　　___一个", ["哈・郎博尔德｝", "___一个"])
        ];
        foreach (entry; entries)
        {
            assert(entry[0].stdSplit() == entry[1], 
                format("got: %s, expected: %s.", entry[0].split(), entry[1]));
            assert(entry[0].newSplit() == entry[1], 
                format("got: %s, expected: %s.", entry[0].newSplit(), entry[1]));
        }
    }

    //Just to test that an immutable is split-able
    immutable string s = " \t\npeter paul\tjerry \n";
    assert(split(s) == ["peter", "paul", "jerry"]);
}


void main(string argv[])
{
    import std.file :  read;
    import std.stdio, std.datetime, core.memory;
    if (argv.length != 3)
    {
        writefln("Usage %s {std|new} <file>", argv[0]);
        return;
    }
    char[] buf = cast(char[])read(argv[2]);
    bool useStd = false;
    switch (argv[1])
    {
    case "std":
        useStd = true;
        break;
    case "new":
        break;
    default:
        writefln("Usage %s {std|new} <file>", argv[0]);
        return;
    }
    StopWatch sw;
    GC.disable();
    size_t count;
    sw.start();
    if (useStd)
    {
        foreach(_; 0..10)
            count += stdSplit(buf).length;
    }
    else
    {
        foreach(_; 0..10)
            count += stdSplit(buf).length;
    }
    sw.stop();
    writefln("Done %s pieces in %s us", count, sw.peek().usecs);
}