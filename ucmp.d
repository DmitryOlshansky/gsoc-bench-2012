import uni, std.file, std.algorithm, std.range, std.stdio, std.utf;

//dump the vicinity of idx
void dump(char[] buf, size_t idx)
{
    auto bwd = find!(x => combiningClass(x) == 0)(buf[0..idx].retro).source.length;
    auto fwd = buf.length - find!(x => combiningClass(x) == 0)(buf[idx..$]).length;
    writefln("back: %d; fwd %d", bwd, fwd);
    writeln("[%( 0x%05x, %)]", buf[bwd..fwd]);
}


int main(string[] args)
{
    if(args.length < 3)
    {
        writeln("Usage: ucmp <file-1> <file2>\nFinds first combining sequence that differs.");
        return 1;
    }
    auto first = cast(char[])read(args[1]);
    auto second = cast(char[])read(args[2]);
    size_t idx1, idx2;
    while(idx1<first.length)
    {
        dchar lhs = decode(first, idx1);
        if(idx2 == second.length)
        {
            writeln("2nd is shorter!");
            write(first[idx1..$]);
            break;
        }
        dchar rhs = decode(second, idx2);
        if(lhs != rhs)
        {
            dump(first, idx1);
            dump(second, idx2);            
            break;
        }
    }
    if(idx2 != second.length)
    {
        writeln("1st is shorter!");
        write(second[idx2..$]);
    }
    return 0;
}