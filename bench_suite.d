module bench_suite;
import std.stdio, std.typetuple, std.algorithm, std.utf, std.file, std.datetime;

public:
struct Result
{
	char[] data;
	string name;
}

//a tool to use in testAll
auto bench(alias func)(string title, string name, in char[] data)
{
	size_t idx=0;	
	size_t cdpts=0;
	StopWatch sw;
	sw.start();
	func(data);
	sw.stop();
	auto spent = sw.peek().usecs;

	writefln("%16s [%6s], %9s, %.2f"
		, title,  name[0..min($,6)], spent/1000.0,  data.length*1_000_000.0/spent/1024/1024);
}


//
void testAll(alias func)(string argv[])
{
	auto datum = new Result[argv.length-1];
	foreach(i, name; argv[1..$])
		datum[i] = Result(cast(char[])std.file.read(name), name);
	func(datum);
}

