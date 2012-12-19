import uni, std.file, std.algorithm, std.range, std.stdio, std.utf;

//dump the vicinity of idx
void dump(char[] buf, size_t idx)
{
    size_t back = idx, fwd = idx;
    for(size_t i=0; i<10 && back > 0; i++)
        back -= strideBack(buf, back);
    for(size_t i=0; i<20 && fwd < buf.length; i++)
        fwd += std.utf.stride(buf, fwd);
    writefln("back: %d; fwd %d", back, fwd);
    writefln("[%( 0x%05x, %)]", buf[back..fwd]);
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
            break;
        }
        dchar rhs = decode(second, idx2);
        if(lhs != rhs)
        {
            dump(first, idx1-codeLength!char(lhs));
            dump(second, idx2-codeLength!char(rhs));            
            return 1;
        }
    }
    if(idx2 != second.length)
    {
        writeln("1st is shorter!");
    }
    return 0;
}