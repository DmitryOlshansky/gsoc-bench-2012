#!/bin/sh
for arch in 32 64
do
	for ver in my std
	do
		COMMAND="ldc2 -m$arch -of$ver$arch -O3 -release -d-version=$ver  -disable-boundscheck fast_stride.d"
		echo $COMMAND
		$COMMAND
	done
done
