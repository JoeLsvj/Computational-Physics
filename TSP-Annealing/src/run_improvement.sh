#!/bin/bash

resultdir=.

rm improvement_24.csv

for i in 0.85 0.90 0.95 0.99
do
	for j in 10 50 100 500 1000 5000
	do
		printf "$i, $j," >> $resultdir/improvement_24.csv
		for k in {1..5}
		do
			printf "%s," $(./a.out 24 100. $i $j) >> $resultdir/improvement_24.csv
		done
		printf "\n" >> $resultdir/improvement_24.csv
	done
done
