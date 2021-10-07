#!/bin/bash
clear
echo "Generating output of test runs of tests/regression/*"
mkdir  "/home/alex/Documents/git/analyzer/testResults"
dstpath=/home/alex/Documents/git/analyzer/testResults/
for folder in "/home/alex/Documents/git/analyzer/tests/regression"/*; do
	foldername=$(basename "$folder")
	echo "$foldername"
	mkdir "/home/alex/Documents/git/analyzer/testResults"/$foldername
	for entry in "/home/alex/Documents/git/analyzer/tests/regression"/$foldername/*; do
	  
	  basename=$(basename "$entry")
	  dst="${dstpath}"$foldername"/"${basename::-2}".sarif"
	  ../goblint --sarif -o "$dst" "$entry" 
	done
done
