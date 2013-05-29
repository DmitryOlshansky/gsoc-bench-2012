#!/bin/sh
for comp in dmd_ ldc_
do
for arch in 32 64
do
	for ver in my std
	do
		echo $comp$ver$arch
		for i in 1 2 3
		do
			./$comp$ver$arch $1
		done
	done
done
done
