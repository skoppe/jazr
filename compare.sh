#!/bin/bash
#

dub build --config=pgo-profile-sse42 --build=release --compiler=ldc2
dub build --config=pgo-build-sse42 --build=release --compiler=ldc2
# dub build --compiler=ldc2 --build=release

firstArg=$1

if [ -f "compare.csv" ]; then
	rm compare.csv
fi;

touch compare.csv

echo "Comparing jazr against uglifyjs"

echo "\"file\",\"jazr time\",\"jazr mem\",\"uglify time\",\"uglify mem\",\"size\",\"compressed jazr\",\"compressed uglify\",\"gzip jazr\",\"gzip uglify\",\"gzip jazr+ugly\",\"gzip ugly+jazr\"" >> compare.csv
while read p; do
	name=`basename "$p"`;
	resultJazr=`gtime --output=time.tmp --format="%E\",\"%M" ./jazr "--DRT-gcopt=disable:1" -i $p --minify -o compare/$name.jazr.min 2>/dev/null && cat time.tmp`
	if [ $? -ne 0 ]; then
		echo "Error in jazr $p";
	fi
	orgSize=`stat -f%z $p`

	if [ "$firstArg" == "--diff" ]; then
		if [ -f "compare/$name.jazr.pretty.current" ]; then
			if [ -f "compare/$name.jazr.pretty.old" ]; then
				rm "compare/$name.jazr.pretty.old"
			fi;
			cp "compare/$name.jazr.pretty.current" "compare/$name.jazr.pretty.old"
		fi;

		./jazr "--DRT-gcopt=disable:1" -i $p --minify --pretty -o "compare/$name.jazr.pretty.current" 2>/dev/null

		if [ -f "compare/$name.jazr.pretty.old" ]; then
			echo -e "\033[01;28mdiff \"$name\":\033[01;0m"
			diff "compare/$name.jazr.pretty.current" "compare/$name.jazr.pretty.old"
		fi;
	fi;
	compressedSizeJazr=`stat -f%z compare/$name.jazr.min`
	# resultUglify=`gtime --output=time.tmp --format="%E\",\"%M" uglifyjs --compress --mangle -- $p  > compare/$name.ugly.min 2>/dev/null && cat time.tmp`
	compressedSizeUglify=`stat -f%z compare/$name.ugly.min`
	# resultJazrUgly=`uglifyjs --compress --mangle -- compare/$name.jazr.min  > compare/$name.jazr.ugly.min 2>/dev/null`
	# compressedSizeJazrUgly=`stat -f%z compare/$name.jazr.ugly.min`
	resultUglyJazr=`./jazr "--DRT-gcopt=disable:1" -i compare/$name.ugly.min --minify -o compare/$name.ugly.jazr.min 2>/dev/null`
	compressedSizeUglifyJazr=`stat -f%z compare/$name.ugly.jazr.min`
	gzip -k "compare/$name.jazr.min"
	gzip -k "compare/$name.ugly.min"
	# gzip -k "compare/$name.jazr.ugly.min"
	gzip -k "compare/$name.ugly.jazr.min"
	gzipSizeJazr=`stat -f%z compare/$name.jazr.min.gz`
	gzipSizeUgly=`stat -f%z compare/$name.ugly.min.gz`
	# gzipSizeJazrUgly=`stat -f%z compare/$name.jazr.ugly.min.gz`
	gzipSizeUglyJazr=`stat -f%z compare/$name.ugly.jazr.min.gz`
	rm "compare/$name.jazr.min.gz"
	rm "compare/$name.ugly.min.gz"
	# rm "compare/$name.jazr.ugly.min.gz"
	rm "compare/$name.ugly.jazr.min.gz"
	echo -e "Jazr: \033[01;28m$compressedSizeJazr\033[01;0m,\tUgly: \033[01;34m$compressedSizeUglify\033[01;0m,\tJazr+Ugly: \033[01;34m$compressedSizeJazrUgly\033[01;0m,\tUgly+Jazr: \033[01;35m$compressedSizeUglifyJazr\033[01;0m,\tGzip Jazr: \033[01;36m$gzipSizeJazr\033[01;0m,\tGzip Ugly: \033[01;31m$gzipSizeUgly\033[01;0m,\tGzip Jazr+Ugly: \033[01;33m$gzipSizeJazrUgly\033[01;0m,\tGzip Ugly+Jazr: \033[01;32m$gzipSizeUglyJazr\033[01;0m -- $p"
	echo -e "\"$p\",\"$resultJazr\",\"$resultUglify\",\"$orgSize\",\"$compressedSizeJazr\",\"$compressedSizeUglify\",\"$gzipSizeJazr\",\"$gzipSizeUgly\",\"$gzipSizeJazrUgly\",\"$gzipSizeUglyJazr\"" >> compare.csv
done < files-to-test.list