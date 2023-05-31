#!/usr/bin/env bash

shopt -s extglob

MYBENCHDIR=/mnt/goblint-svcomp/benchexec/my-bench-sv-comp
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/data-race-results21-concurrencysafety-new
GOBLINTPARALLEL=14

mkdir $RESULTSDIR

# Run verification
cd /mnt/goblint-svcomp/sv-comp/goblint
# read-only and overlay dirs for Value too large for defined data type workaround
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $GOBLINTPARALLEL $MYBENCHDIR/goblint-data-race.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint*.files`
echo $LOGDIR

# Generate table with merged results and witness validation results
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator-data-race.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint*.logfiles.zip