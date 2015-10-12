#!/bin/sh
SRCS="char_class.d bench_suite.d alpha.d"
DMD="dmd -O -release -noboundscheck -inline $SRCS"
LDC="ldc2  -O3 -release -d-version=$ver  -disable-boundscheck $SRCS"

for CMD in "$DMD"
do
for arch in 32 64
do
    ARGS="-m$arch -ofchar-class-$arch"
    echo $CMD $ARGS
    $CMD $ARGS
done
done
