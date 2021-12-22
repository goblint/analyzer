#!/bin/bash
#Run from root, e.g.,: ./scripts/fix_patch_headers.sh tests/incremental/11-restart/*.patch
for file in "$@"
do
  echo $file
  cfile="${file%.patch}.c"
  efile="${cfile//\//\\/}"
  perl -0777 -i -pe "s/.*?@/--- $efile\n+++ $efile\n@/is" $file
done
