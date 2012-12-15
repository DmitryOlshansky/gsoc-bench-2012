import std.stdio, std.random, std.exception, std.format, std.conv;

int main(string args[])
{
    dchar[] buffer = new dchar[1_000]; //1K of codepoints
    if(args.length < 3 || args.length > 4){
        stderr.writeln("Usage: monkey <codepoint interval> <x> [seed]\n
    where:
    codepoint interval is a pair of hex integers separated by dash e.g. 1000-2000; 
    x: thosands of codepoints to generate in this range.

    **Don't forget to redirect std output to some file.");
        return 1;
    }
    string spec = args[1];    
    uint start, end;
    formattedRead(spec, "%x-%x", &start, &end);
    enforce(start >= 0 && start <= dchar.max);
    enforce(end >= 0 && end <= dchar.max && end >= start);    
    uint amount = to!uint(args[2]);
    uint seed = args.length == 4 ? to!uint(args[3]) : unpredictableSeed();
    auto rng =  Xorshift(seed);
    foreach(dummy; 0..amount){
        foreach(ref v; buffer){
            v = cast(dchar)uniform!"[]"(start, end);            
        }
        write(buffer);
    }
    return 0;
}