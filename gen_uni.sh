#!/bin/sh
sh get-uni.sh
DMD=dmd
DFMT=dfmt
echo building gen_uni_64 and gen_uni_32
$DMD -m64 -O -release -inline gen_uni.d randAA.d -ofgen_uni_64 &
$DMD -m32 -O -release -inline gen_uni.d randAA.d -ofgen_uni_32 &
wait
echo generating unicode_tables
mkdir -p 64
./gen_uni_64
mv unicode_*.d 64
mkdir -p 32
./gen_uni_32
mv unicode_*.d 32
for name in 64/*.d ; do
	name32=`echo $name | sed 's/64/32/'`
	sed -n '/^static if(size_t.sizeof == 4) {$/,$p' $name32 >> $name
done
mv 64/*.d .
rm -rf 64/ 32/
$DFMT --inplace --max_line_length=80 unicode_*.d
