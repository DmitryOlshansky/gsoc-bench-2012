module bench_suite;
import std.stdio, std.conv, std.typetuple, std.algorithm, std.utf, std.file, std.datetime;

public:
struct Result
{
    dchar[] data;
    string name;
}

//a tool to use in testAll
auto bench(alias func)(string title, string name, in dchar[] data)
{
    size_t idx=0;    
    size_t cdpts=0;
    StopWatch sw;
    sw.start();
    func(data);
    sw.stop();
    auto spent = sw.peek().usecs;
    //time in usecs - throughput number is thus in millions/s
    writefln("%16s [%6s], %9s, %.2fM/s"
        , title,  name[0..min($,6)], spent/1000.0,  data.length*1.0/spent); 
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
            datum[i] = Result(a.data, name);
        }
        sw.stop();
        auto spent = sw.peek().msecs;
        writefln("Time to build dchar array: %d ms", spent);        
    }
    GC.disable();
    func(datum);
    GC.enable();
}

