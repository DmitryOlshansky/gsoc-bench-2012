import std.stdio, std.algorithm, std.random, std.exception, std.format, std.conv;
import uni;
import icu;

//dump the vicinity of idx
void dump(C)(C[] buf, size_t idx)
{
    size_t back = idx > 10 ? idx-10 : 0;
    size_t fwd = idx + 10 < buf.length ? idx+10 : buf.length;    
    writefln("back: %d; fwd %d; len %d", back, fwd, fwd-back);
    writefln("[%( 0x%05x, %)]", buf[back..fwd]);
}

int main(string args[])
{
    if(args.length < 5 || args.length > 6){
        stderr.writeln("Usage: monkey {nfc|nfd|nfkc|nfkd} <codepoint interval> <sample> <x> [seed]\n
    where:
    codepoint interval is a pair of hex integers separated by dash e.g. 1000-2000; 
    sample: number of codepoints per sample
    x: number of samples to test in this range.");
        return 1;
    }
    UNormalizer2* icu;
    auto normalizer(string kind){
        int code;
        switch(kind){
            case "nfc":
                icu = unorm2_getNFCInstance_50(&code);
                assert(code <= 0, text("ICU get norm:", code)); 
                return (dchar[] inp){ return normalize!NFC(inp); };
            case "nfd":
                icu = unorm2_getNFDInstance_50(&code);
                assert(code <= 0, text("ICU get norm:", code));
                return (dchar[] inp){ return normalize!NFD(inp); };
            case "nfkd":
                icu = unorm2_getNFKDInstance_50(&code);
                assert(code <= 0, text("ICU get norm:", code));
                return (dchar[] inp){ return normalize!NFKD(inp); };
            case "nfkc":
                icu = unorm2_getNFKCInstance_50(&code);
                assert(code <= 0, text("ICU get norm:", code));
                return (dchar[] inp){ return normalize!NFKC(inp); };
            default:
                enforce(false, "no normalization form: " ~ kind);
                assert(false);
        }
    }
    auto fn = normalizer(args[1]);
    string spec = args[2];
    uint start, end;
    formattedRead(spec, "%x-%x", &start, &end);
    enforce(start >= 0 && start <= dchar.max);
    enforce(end >= 0 && end <= dchar.max && end >= start);    
    uint sample = to!uint(args[3]);
    uint count = to!uint(args[4]);
    uint seed = args.length == 6 ? to!uint(args[5]) : unpredictableSeed();
    auto rng =  Xorshift(seed);

    dchar[] buffer = new dchar[sample];
    dchar[] d_buffer = new dchar[sample];
    foreach(dummy; 0..count){
        foreach(ref v; buffer){
            v = cast(dchar)uniform!"[]"(start, end);            
        }
        d_buffer[] = buffer[];//to avoid any aliasing to original
        auto d_result = fn(d_buffer);
        int icu_code;
        auto icu_buf = to!(wchar[])(buffer);
        wchar[] icu_dest = new wchar[icu_buf.length*18];
        int total = unorm2_normalize_50(icu, icu_buf.ptr, cast(int)icu_buf.length, icu_dest.ptr, cast(int)icu_dest.length, &icu_code);
        assert(icu_code <=0, text("ICU norm:", icu_code));
        icu_dest = icu_dest[0..total];

        auto icu_result = to!(dchar[])(icu_dest);
        if(!equal(d_result,icu_result)){
            writefln("ORIGINAL: [%(%05x,%)]", buffer);
            writefln("D: [%(%05x,%)]", d_result);
            writefln("ICU: [%(%05x,%)]", icu_result);
        }
    }
    return 0;
}