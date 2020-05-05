gsoc-bench-2012
===============

The implementation of GSOC 2012 proposal for better Unicode support in Phobos.

Originally it's a separate repo for benchmark(s) but they get squashed to exactly one:

    rdmd -O -release -noboundscheck -inline char_class.d <files> 
  
to test various lookup strategies of the new std.uni.

Also this repo containts tests for grapheme breaking and normalization these need 
to be fed with proper data: 

http://unicode.org/Public/UNIDATA/NormalizationTest.txt

http://unicode.org/Public/UNIDATA/auxiliary/GraphemeBreakTest.txt




