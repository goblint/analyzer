#!/usr/bin/env bash

shopt -s extglob

MYBENCHDIR=//mnt/goblint-svcomp/sv-comp/goblint/sv-comp/my-bench-sv-comp
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/LoopsEnums
GOBLINTPARALLEL=3

mkdir $RESULTSDIR

# Run verification
cd /mnt/goblint-svcomp/sv-comp/goblint
# read-only and overlay dirs for Value too large for defined data type workaround
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $GOBLINTPARALLEL $MYBENCHDIR/goblint-all-fast.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint*.files`
echo $LOGDIR

# Generate table with merged results and witness validation results
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator-all-fast.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint*.logfiles.zip