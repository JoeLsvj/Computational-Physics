#!/bin/bash

resultdir=.

rm CPUtimes_091.csv

for i in 1000 5000 10000
do
	for j in 8 12 24 48 96
	do
		printf "$i, $j," >> $resultdir/CPUtimes_091.csv
		for k in {1..5}
		do
			printf "%s," $(./a.out $j $j*10 0.91 $i) >> $resultdir/CPUtimes_091.csv
		done
		printf "\n" >> $resultdir/CPUtimes_091.csv
	done
done
