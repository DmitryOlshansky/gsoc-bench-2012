import uni, std.file, std.exception;
import std.stdio: writeln;

int main(string args[]){
    auto normalizer(string kind){
        switch(kind){
            case "nfc":
                return (const(char[]) inp){ return normalize!NFC(inp); };
            case "nfd":
                return (const(char[]) inp){ return normalize!NFD(inp); };
            case "nfkd":
                return (const(char[]) inp){ return normalize!NFKD(inp); };
            case "nfkc":
                return (const(char[]) inp){ return normalize!NFKC(inp); };
            default:
                enforce(false, "no normalization form: " ~ kind);
                assert(false);
        }
    }
    if(args.length < 3)
    {
        writeln("Usage: norm {nfc|nfd|nfkc|nfkd} <input-files>");
        return 1;
    }
    auto fn = normalizer(args[1]);
    foreach(name; args[2..$]){
        char[] data = cast(char[])read(name);  
        write(name~"_d_"~args[1], fn(data));
    }
    return 0;
}