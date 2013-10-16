#!/bin/sh
sh get-uni.sh
echo building gen_uni_64 and gen_uni_32
$DMD -m64 -O -release -inline gen_uni.d randAA.d -ofgen_uni_64 &
$DMD -m32 -O -release -inline gen_uni.d randAA.d -ofgen_uni_32 &
wait
echo generating unicode_tables.d
./gen_uni_64 > unicode_tables.d &
./gen_uni_32 > tmp.d &
wait
sed -n '/^static if(size_t.sizeof == 4) {$/,$p' tmp.d >> unicode_tables.d
rm tmp.d
