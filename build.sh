#!/bin/sh

for arch in 32 64
do
    for ver in my std
    do
	DMD="dmd -O -release -version=$ver -noboundscheck -inline fast_stride.d -m$arch "
	LDC="ldc2  -O3 -release -d-version=$ver  -disable-boundscheck fast_stride.d -m$arch "
        OUT="-ofdmd_$ver$arch"
        echo $DMD $OUT
        $DMD $OUT
	OUT="-ofldc_$ver$arch"
	echo $LDC $OUT
	$LDC $OUT
    done
done
