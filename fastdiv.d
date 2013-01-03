struct Magic
{
    uint mul;    
    uint shift;
    bool add;
}

struct RQ{
    uint r;
    uint q;
}

Magic magicNumbers(uint d)
{
    static double dumbPow2(int n)
    {
        double x = 1.0;
        for(int i=0; i<n; i++)
            x *= 2;
        return x;
    }
    import core.bitop;
    Magic m;
    assert(d > 1);
    uint r = 32 + bsr(d);
    double f = dumbPow2(r) / d;    
    double frac = f - cast(long)f;
    if(frac < 0.5)
    {
        m.mul = cast(uint)f;
        m.add = true;
    }
    else
    {
        m.mul = cast(uint)f + 1;
        m.add = false;
    }
    m.shift = r - 32;
    return m;
}

RQ fastModDiv(uint d)(uint n)
{
    static string genFastModDiv(uint div, string rName)
    {
        import std.string;
        Magic m = magicNumbers(div);
        if(m.mul == 0)
            return format("%s.q = (n >> %s);\n%s.r = n & 0x%X;\n", 
                rName, m.shift, rName, (1<<m.shift)-1);
        else
            return format("%s.q = ((n%s) * 0x%XUL)>>%s;\n", 
                rName, m.add ? "+1" : "", m.mul, m.shift+32)
            ~ format("%s.r = n - %s.q * %s;\n", rName, rName, div);
    }
    RQ rq;
    mixin(genFastModDiv(d, "rq"));
    return rq;
}



import std.math, std.typetuple, std.range;
import std.conv, std.stdio, std.datetime, std.random;


void main(){
    version(verify)
    {
        foreach(divisor; TypeTuple!(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15))
        {
            foreach(v; 0..uint.max)
            {
                auto rq = fastModDiv!divisor(v);
                assert(v / divisor == rq.q && v % divisor == rq.r, 
                    text("fastDiv!", divisor," failed v=", v, " q=", rq.q));
            }
            writeln("All passed for fastDiv!", divisor);
        }
    }

    StopWatch sw;
    Xorshift rng = Xorshift();
    uint[] data = array(take(rng, 1000*1000));
    uint rem = 0, quot = 0;
    writefln("\nBenching baseline (div by 2)");
    sw.start();
    foreach(x; 0..1000)
    foreach(v; data)
    {
        quot += v / 2;
        rem += v % 2;
    }
    sw.stop();
    writeln("reminder: ", rem, " division: ", quot);
    writeln("Miliseconds to compute: ", sw.peek().msecs);
    foreach(divisor; TypeTuple!(3, 5, 7, 9, 11, 12))
    {
        writefln("\nBenching div by %d", divisor);
        rem = 0; 
        quot = 0;
        sw.reset();
        sw.start();
        foreach(x; 0..1000)
        foreach(v; data)
        {
            auto rq = fastModDiv!divisor(v);
            quot += rq.q;
            rem += rq.r;
        }
        sw.stop();
        writeln("reminder: ", rem, " division: ", quot);
        writeln("Miliseconds to compute: ", sw.peek().msecs);
        rem = 0; 
        quot = 0;
        sw.reset();
        sw.start();
        foreach(x; 0..1000)
        foreach(v; data)
        {
            quot += v / divisor;
            rem += v % divisor;
        }
        sw.stop();
        writeln("reminder: ", rem, " division: ", quot);
        writeln("Miliseconds to compute: ", sw.peek().msecs);
    }
}