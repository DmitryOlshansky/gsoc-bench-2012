import std.algorithm, std.stdio, std.range, std.conv, std.string, uni;

//piece-wise CCC analyzis of 
void analyze(C)(in C[] str)
{
    foreach(dchar ch; str)
    {
        writefln("code=0x%04x ccc=%2d", ch, combiningClass(ch));
    }
}

void main(){
    size_t number = 0;    
    foreach(char[] line; stdin.byLine){
        number++;
        if(!isNumber(line.front))
            continue;
        //From the test case file:
        //   Columns (c1, c2,...) are separated by semicolons
        //   They have the following meaning:
        //      source; NFC; NFD; NFKC; NFKD
        dchar[][]  samples = new dchar[][5];
        for(int i=0; i<5; i++){
            do{
                line.munch(" \t");
                uint val = parse!uint(line, 16);
                samples[i] ~= cast(dchar)val;
            }while(line.front != ';');
            line.munch( ";");
        }

        auto normD = array(map!(x => normalize!NFD(x))(samples));
        auto normC = array(map!(x => normalize!NFC(x))(samples));
        auto normKC = array(map!(x => normalize!NFKC(x))(samples));
        auto normKD = array(map!(x => normalize!NFKD(x))(samples));

        void test(const(dchar)[] norm, const(dchar)[] expected, string hint="~~~")
        {
            if(norm != expected){
                writeln("Normalization form "~hint~" failed on line: ", number);
                analyze(norm);
            }
        }
        test(normC[0], samples[1], "NFC-c1");
        test(normC[1], samples[1], "NFC-c2");
        test(normC[2], samples[1], "NFC-c3");
        test(normC[3], samples[3], "NFC-c4");
        test(normC[4], samples[3], "NFC-c5");

        test(normD[0], samples[2], "NFD-c1");
        test(normD[1], samples[2], "NFD-c2");
        test(normD[2], samples[2], "NFD-c3");
        test(normD[3], samples[4], "NFD-c4");
        test(normD[4], samples[4], "NFD-c5");

        test(normKC[0], samples[3], "NFKC-c1");
        test(normKC[1], samples[3], "NFKC-c2");
        test(normKC[2], samples[3], "NFKC-c3");
        test(normKC[3], samples[3], "NFKC-c4");
        test(normKC[4], samples[3], "NFKC-c5");

        test(normKD[0], samples[4], "NFKD-c1");
        test(normKD[1], samples[4], "NFKD-c2");
        test(normKD[2], samples[4], "NFKD-c3");
        test(normKD[3], samples[4], "NFKD-c4");
        test(normKD[4], samples[4], "NFKD-c5");
    }
}