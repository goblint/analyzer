#!/bin/bash

shopt -s extglob

MYBENCHDIR=/mnt/goblint-svcomp/benchexec/my-bench-sv-comp/yaml
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/yaml-xy-bench
GOBLINTPARALLEL=14
VALIDATEPARALLEL=14

mkdir $RESULTSDIR

# Run verification
cd /mnt/goblint-svcomp/sv-comp/goblint
# read-only and overlay dirs for Value too large for defined data type workaround
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $GOBLINTPARALLEL $MYBENCHDIR/goblint-bench.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint-bench.*.files`
echo $LOGDIR

# Construct validation XMLs
cd $MYBENCHDIR
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" goblint-bench-validate.xml > goblint-bench-validate-tmp.xml

# Run validation
cd /mnt/goblint-svcomp/sv-comp/goblint
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/goblint-bench-validate-tmp.xml

# Merge witness validation results
cd $RESULTSDIR
# python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py -o . goblint.*.results.*.xml.bz2 goblint-validate-tmp.*.results.*.xml.bz2
python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py -o . goblint-bench.*.results.all.Pthread.xml.bz2 goblint-bench-validate-tmp.*.results.all.Pthread.xml.bz2
python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py -o . goblint-bench.*.results.loop-head.Pthread.xml.bz2 goblint-bench-validate-tmp.*.results.loop-head.Pthread.xml.bz2

# Generate table with merged results and witness validation results
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator-bench.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint-bench.*.logfiles.zip
unzip -o goblint-bench-validate-tmp.*.logfiles.zip
