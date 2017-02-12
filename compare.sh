#!/bin/bash
#

dub build --config=pgo-profile --build=release --compiler=ldc2
dub build --config=pgo-build --build=release --compiler=ldc2

if [ -f "compare.csv" ]; then
	rm compare.csv
fi;

touch compare.csv

echo "Comparing jazr against uglifyjs"

echo "\"file\",\"jazr time\",\"jazr mem\",\"uglify time\",\"uglify mem\",\"size\",\"compressed jazr\",\"compressed uglify\"" >> compare.csv
while read p; do
	echo $p;
	name=`basename "$p"`;
	resultJazr=`gtime --output=time.tmp --format="%E\",\"%M" ./es6-parse "--DRT-gcopt=disable:1" -i $p --minify -o compare/$name.jazr.min 2>/dev/null && cat time.tmp`
	orgSize=`stat -f%z $p`
	compressedSizeJazr=`stat -f%z compare/$name.jazr.min`
	resultUglify=`gtime --output=time.tmp --format="%E\",\"%M" uglifyjs --compress --mangle -- $p  > compare/$name.ugly.min 2>/dev/null && cat time.tmp`
	compressedSizeUglify=`stat -f%z compare/$name.ugly.min`
	echo -e "\"$p\",\"$resultJazr\",\"$resultUglify\",\"$orgSize\",\"$compressedSizeJazr\",\"$compressedSizeUglify\"" >> compare.csv
done < files-to-test.list