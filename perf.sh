#!/bin/bash
#

dub build --config=pgo-profile --build=release --compiler=ldc2
dub build --config=pgo-build --build=release --compiler=ldc2

if [ -f "compare.csv" ]; then
	rm compare.csv
fi;

if [ ! -f "perf.log" ]; then
	touch perf.log
fi;

if [ -f "perf.tmp" ]; then
	rm perf.tmp
fi;

touch compare.csv

echo "Comparing jazr against uglifyjs"

echo "\"file\",\"jazr time\",\"jazr mem\",\"uglify time\",\"uglify mem\",\"size\",\"compressed jazr\",\"compressed uglify\"" >> compare.csv
while read p; do
	echo $p;
	resultJazr=`gtime --output=time.tmp --format="%E\",\"%M" ./es6-parse "--DRT-gcopt=disable:1" -i $p --minify -o compare.tmp 2>/dev/null && cat time.tmp`
	orgSize=`stat -f%z $p`
	compressedSizeJazr=`stat -f%z compare.tmp`
	resultUglify=`gtime --output=time.tmp --format="%E\",\"%M" uglifyjs --compress --mangle -- $p  > compare.tmp 2>/dev/null && cat time.tmp`
	compressedSizeUglify=`stat -f%z compare.tmp`
	echo -e "\"$p\",\"$resultJazr\",\"$resultUglify\",\"$orgSize\",\"$compressedSizeJazr\",\"$compressedSizeUglify\"" >> compare.csv
done < files-to-test.list

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

rm compare.tmp