#!/bin/bash

shopt -s extglob

MYBENCHDIR=/mnt/goblint-svcomp/benchexec/my-bench-sv-comp/yaml
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/yaml-21-software-rebase
GOBLINTPARALLEL=8
VALIDATEPARALLEL=8

mkdir $RESULTSDIR

# Run verification
cd /mnt/goblint-svcomp/sv-comp/goblint
# read-only and overlay dirs for Value too large for defined data type workaround
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $GOBLINTPARALLEL $MYBENCHDIR/goblint.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint.*.files`
echo $LOGDIR

# Construct validation XMLs
cd $MYBENCHDIR
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" goblint-validate.xml > goblint-validate-tmp.xml

# Run validation
cd /mnt/goblint-svcomp/sv-comp/goblint
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/goblint-validate-tmp.xml

# Merge witness validation results
cd $RESULTSDIR
# python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py -o . goblint.*.results.*.xml.bz2 goblint-validate-tmp.*.results.*.xml.bz2
python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py -o . goblint.*.results.sv-comp_prop-reachsafety.xml.bz2 goblint-validate-tmp.*.results.sv-comp_prop-reachsafety.xml.bz2

# Generate table with merged results and witness validation results
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint.*.logfiles.zip
unzip -o goblint-validate-tmp.*.logfiles.zip
