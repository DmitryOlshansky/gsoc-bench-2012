import std.stdio, std.range, std.conv, std.string, uni;

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
        number ++ ;
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
        auto normD = normalize!NFD(samples[0]);
        if(normD !=  samples[2]){
            writeln("Normalization form NFD failed on line: ", number);
            analyze(normD);
        }
        auto normKD = normalize!NFKD(samples[0]);
        if(normKD !=  samples[4]){
            writeln("Normalization form NFKD failed on line: ", number);
            analyze(normKD);
        }

    }
}