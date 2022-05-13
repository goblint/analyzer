#!/bin/bash

shopt -s extglob

MYBENCHDIR=/mnt/goblint-svcomp/benchexec/my-bench-sv-comp
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/new-results28-all-fast-systems-witness-linter
GOBLINTPARALLEL=15
VALIDATEPARALLEL=15

mkdir $RESULTSDIR

# Run verification
cd /mnt/goblint-svcomp/sv-comp/goblint
# read-only and overlay dirs for Value too large for defined data type workaround
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $GOBLINTPARALLEL $MYBENCHDIR/goblint-lint.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint*.files`
echo $LOGDIR

# Construct validation XMLs
cd $MYBENCHDIR
# witnesslint
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" witnesslint-validate.xml > witnesslint-validate-tmp.xml

# Run validation
# witnesslint
cd /mnt/goblint-svcomp/benchexec/tools/witnesslint
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/witnesslint-validate-tmp.xml

# Merge witness validation results
cd $RESULTSDIR
python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py goblint*.results.!(*merged*).xml.bz2 witnesslint-validate-tmp.*.results.*.xml.bz2

# Generate table with merged results and witness validation results
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator-lint.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint*.logfiles.zip
unzip -o witnesslint-validate-tmp.*.logfiles.zip