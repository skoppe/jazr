#!/bin/bash
#

dub build --config=pgo-profile --build=release --compiler=ldc2
dub build --config=pgo-build --build=release --compiler=ldc2

if [ ! -f "perf.log" ]; then
	touch perf.log
fi;

if [ -f "perf.tmp" ]; then
	rm perf.tmp
fi;

echo "Running jazr performance tests"

while read p; do

  	echo $p;
	for i in `seq 1 100`;
	do
		`./es6-parse "--DRT-gcopt=disable:1" -i $p --time --minify -o /dev/null >> perf.tmp`
	done

	median=`sort perf.tmp | tail -n50 | head -n1`

	echo "\"$(date)\",\"$p\",${median:1:${#median}-2}" >> perf.log

	rm perf.tmp

done < files-to-test.list