#!/bin/sh
for arch in 32 64
do
	for ver in my std
	do
		echo $ver$arch
		for i in 1 2 3
		do
			./$ver$arch $1
		done
	done
done
