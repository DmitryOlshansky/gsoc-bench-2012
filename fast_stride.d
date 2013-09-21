import std.traits, std.utf : UTFException;


uint stride(S)(auto ref S str, size_t index)
    if (is(S : const char[]) ||
        (isRandomAccessRange!S && is(Unqual!(ElementType!S) == char)))
{
    immutable c = str[index];
    return c < 0x80 ? 1 : strideImpl(c, index);
}

private uint strideImpl(char c, size_t index)
in { assert(c & 0x80); }
body
{
    import core.bitop;
    immutable msbs = 7 - bsr(~c);
    if (msbs < 2 || msbs > 4)
        throw new UTFException("Invalid UTF-8 sequence", index);
    return msbs;
}

uint myStride(S)(auto ref S src, size_t idx)
    if (is(S : const char[]) ||
        (isRandomAccessRange!S && is(Unqual!(ElementType!S) == char)))
{	
    immutable c = src[idx];
    return c < 0x80 ? 1 : myStrideImpl(c, idx);
}

uint myStrideImpl(ubyte c, size_t idx)
in
{
    assert(c & 0x80);
}
body
{
    enum mask = calcTable();
    //pick bits 0b0xxx_0000, get xxx * 4
    static if(size_t.sizeof == 4)
    {
        size_t shift = (c & 0b0111_1000) >> 2; //xxxx*2
        uint ret = (mask >> shift) & 0x3;
        if(ret == 0)
            throw new UTFException("Invalid UTF-8 sequence", idx);
        return ret+1;
    }
    else
    {
        size_t shift = (c & 0b0111_1000) >> 1; //xxxx*4
        uint ret = (mask >> shift) & 0xf;
        if(ret == 0)
            throw new UTFException("Invalid UTF-8 sequence", idx);
        return ret;
    }    
}

size_t calcTable()
{
    size_t mask = 0;    
    foreach(int top; 0..16) //4 bits
    {
        static if(size_t.sizeof == 4)
        {
            size_t val = utfValue(top);
            // 0 - wrong code point
            mask |= (val ?  val-1 : 0)<<(top*2);
        }
        else 
            mask |= utfValue(top)<<(top*4);
    }
    return mask;
}

//UTF stride for 0b1xxx_x...  where .... is anything, xxxx is 4-bit val
size_t utfValue(int val)
{
    switch(val){
    case 0b1110:
        return 4;
    case 0b1101:
    case 0b1100:
        return 3;
    default: 
        //case 0b10xx:
        if((val & 0b1100) == 0b1000)
            return 2;
        //others
        return 0;  //wrong UTF-8 sequence
    }
}

unittest
{
    foreach(uint ch; 0..0x100)
    {
        char[1] buf;
        buf[0] = cast(char)ch;
        size_t std, my;
        bool stdThrown = false, myThrown = false;
        try{
            std = stride(buf, 0);
        }
        catch(UTFException e){
            stdThrown = true;
        }
        try{
           my = myStride(buf, 0);
        }
        catch(UTFException e){
            myThrown = true;
        }
        writefln("%d - %d [%d %d]", std, my, stdThrown, myThrown);
        assert((myThrown && stdThrown) || std == my);
    }
}

import std.stdio, std.datetime, std.file, std.string;

int bench(alias fn)(char[] buffer)
{
    int i;  
    for(i=0; i<buffer.length; ){
        i += fn(buffer, i);
    }
    return i;
}

int main(string argv[])
{

    if(argv.length < 2)
    {
        writeln("Usage: ./fast_stride <file>");
        return 1;
    }
    
    auto buffer = cast(char[])read(argv[1]);
    int len = 0;
    StopWatch sw;
    sw.start();  
    for(int iter=0;iter<10_000; iter++){        
        //test one order
        version(std)
            len += bench!stride(buffer);
        else version(my)
            len +=  bench!myStride(buffer);
        else
            static assert(0);
    }
    sw.stop();
    writeln(sw.peek().usecs);
    return len;
}
