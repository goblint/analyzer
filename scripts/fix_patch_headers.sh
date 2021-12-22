#!/bin/bash
#Run from root, e.g.,: ./scripts/fix_patch_headers.sh tests/incremental/11-restart/*.patch
for file in "$@"
do
  efile="${file//\//\\/}"
  echo "$file"
  perl -0777 -i -pe "s/.*?@/--- a\/$efile\n+++ b\/$efile\n@/is" $file
done
