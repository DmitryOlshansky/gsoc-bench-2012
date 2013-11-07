module alpha;

auto calculateTableReg()
{    
    uint table;
    foreach(c; 64..96)
    {
        if(c >= 'A' && c <= 'Z')
            table |= 1<<(c-64);
    }
    return table;
}

/++
    Returns whether $(D c) is an ASCII letter (A..Z, a..z).
  +/
bool stdIsAlpha(dchar c) pure nothrow
{
    return c <= 0x7F ? cast(bool)(_ctype[c] & _ALP) : false;
}


bool myIsAlpha(dchar ch)
{ 
    enum table = calculateTableReg();
    uint page = ch & 0x1f;
    //return (head >> book) & (table >> page) & 1; //1
    return cast(bool)((cast(uint)ch - 64U < 64 ? 1 : 0) & (table>>page)); //2
}

unittest 
{
    assert(myIsAlpha('a'));
    assert(myIsAlpha('z'));
    assert(myIsAlpha('A'));
    assert(myIsAlpha('Z'));
    assert(!myIsAlpha('0'));
    assert(!myIsAlpha('a'+2^^16));
}


immutable ubyte[128] _ctype =
[
        _CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
        _CTL,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL,_CTL,
        _CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
        _CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
        _SPC|_BLK,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
        _PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
        _DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,
        _DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,
        _PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
        _PNC,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC,
        _UC,_UC,_UC,_UC,_UC,_UC,_UC,_UC,
        _UC,_UC,_UC,_UC,_UC,_UC,_UC,_UC,
        _UC,_UC,_UC,_PNC,_PNC,_PNC,_PNC,_PNC,
        _PNC,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC,
        _LC,_LC,_LC,_LC,_LC,_LC,_LC,_LC,
        _LC,_LC,_LC,_LC,_LC,_LC,_LC,_LC,
        _LC,_LC,_LC,_PNC,_PNC,_PNC,_PNC,_CTL
];

enum
{
    _SPC =      8,
    _CTL =      0x20,
    _BLK =      0x40,
    _HEX =      0x80,
    _UC  =      1,
    _LC  =      2,
    _PNC =      0x10,
    _DIG =      4,
    _ALP =      _UC|_LC,
}
