module bench_suite;
import std.stdio, std.conv, std.typetuple, std.algorithm, std.utf, std.file, std.datetime;

public:
struct Result
{
    char[] raw;
    dchar[] data;
    string name;
}

struct Measures
{
    ulong usec;
    double throughput; //codepoints per/sec
}

Measures[string][string] measures;

//a tool to use in testAll
auto bench(alias func)(string title, in Result data)
{
    size_t idx=0;    
    size_t cdpts=0;
    StopWatch sw;
    sw.start();
    static if(is(typeof(func(data.raw))))
        func(data.raw);
    else
        func(data.data);
    sw.stop();
    auto spent = sw.peek().usecs;
    //time in usecs - throughput number is thus in millions of codepoints/sec
    measures[data.name][title] = Measures(spent, data.data.length*1.0/spent);
}

//
void testAll(alias func)(string argv[])
{
    import core.memory;
    auto datum = new Result[argv.length-1];
    foreach(i, name; argv[1..$])
    {
        auto text = cast(char[])std.file.read(name);
        StopWatch sw;
        sw.start();
        version(builder)
        {
            import build;
            auto b = builder!dchar();
            copy(text, &b);
            datum[i] = Result(b.build(), name);
        }
        else
        {
            import std.array;
            auto a = appender!(dchar[])();
            copy(text, a);
            datum[i] = Result(text, a.data, name);
        }
        sw.stop();
        auto spent = sw.peek().msecs;  
    }
    GC.disable();
    func(datum);
    GC.enable();
    foreach(filename, store; measures)
    {
        writefln("%s,", filename);
        writefln("method,time(ms),throughput(M/s)");
        auto methods = store.keys().sort().release;
        foreach(m; methods)
        {
            auto measure = store[m];
            writefln("%16s, %s, %.3f", m, measure.usec, measure.throughput); 
        }
    }
}

