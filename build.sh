#!/bin/sh
DMD="dmd -O -release -noboundscheck -inline"
LDC="ldc2  -O3 -release -d-version=$ver  -disable-boundscheck fast_stride.d"

for CMD in DMD LDC
do
for arch in 32 64
do
    for ver in my std
    do
        ARGS="-m$arch -of$ver$arch"
        echo $CMD $ARGS
        $CMD $ARGS
    done
done
done
