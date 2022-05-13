#!/bin/bash

shopt -s extglob

MYBENCHDIR=/mnt/goblint-svcomp/benchexec/my-bench-sv-comp
RESULTSDIR=/mnt/goblint-svcomp/benchexec/results/new-results32-overflow
GOBLINTPARALLEL=15
VALIDATEPARALLEL=4 # not enough memory for more

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
# witnesslint
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" witnesslint-validate2.xml > witnesslint-validate2-tmp.xml
# CPAChecker
# sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" cpa-validate-correctness.xml > cpa-validate-correctness-tmp.xml
# sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" cpa-validate-violation.xml > cpa-validate-violation-tmp.xml
# Ultimate
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" uautomizer-validate-correctness.xml > uautomizer-validate-correctness-tmp.xml
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" uautomizer-validate-violation.xml > uautomizer-validate-violation-tmp.xml

# Run validation
# witnesslint
cd /mnt/goblint-svcomp/benchexec/tools/witnesslint
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/witnesslint-validate2-tmp.xml
# CPAChecker
# cd /home/simmo/benchexec/tools/CPAchecker-1.9-unix
# benchexec --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/cpa-validate-correctness-tmp.xml
# benchexec --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/cpa-validate-violation-tmp.xml
# Ultimate
cd /mnt/goblint-svcomp/benchexec/tools/UAutomizer-linux
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/uautomizer-validate-correctness-tmp.xml
benchexec --read-only-dir / --overlay-dir . --hidden-dir /home --outputpath $RESULTSDIR --numOfThreads $VALIDATEPARALLEL $MYBENCHDIR/uautomizer-validate-violation-tmp.xml

# Merge witness validation results
cd $RESULTSDIR
# python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py goblint.*.results.!(*merged*).xml.bz2 cpa-validate-correctness-tmp.*.results.*.xml.bz2 cpa-validate-violation-tmp.*.results.*.xml.bz2 uautomizer-validate-correctness-tmp.*.results.*.xml.bz2 uautomizer-validate-violation-tmp.*.results.*.xml.bz2
# python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py goblint.*.results.!(*merged*).xml.bz2 uautomizer-validate-correctness-tmp.*.results.*.xml.bz2 uautomizer-validate-violation-tmp.*.results.*.xml.bz2 witnesslint-validate2-tmp.*.results.*.xml.bz2
python3 /mnt/goblint-svcomp/benchexec/benchexec/contrib/mergeBenchmarkSets.py goblint.*.results.*no-overflow.xml.bz2 uautomizer-validate-correctness-tmp.*.results.*no-overflow.xml.bz2 uautomizer-validate-violation-tmp.*.results.*no-overflow.xml.bz2 witnesslint-validate2-tmp.*.results.*no-overflow.xml.bz2

# Generate table with merged results and witness validation results
# table-generator goblint.*.results.*.xml.bz2.merged.xml.bz2 cpa-validate-correctness-tmp.*.results.*.xml.bz2 cpa-validate-violation-tmp.*.results.*.xml.bz2 uautomizer-validate-correctness-tmp.*.results.*.xml.bz2 uautomizer-validate-violation-tmp.*.results.*.xml.bz2
sed -e "s/LOGDIR/$LOGDIR/" $MYBENCHDIR/table-generator-witness.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint.*.logfiles.zip
# unzip -o cpa-validate-correctness-tmp.*.logfiles.zip
# unzip -o cpa-validate-violation-tmp.*.logfiles.zip
unzip -o uautomizer-validate-correctness-tmp.*.logfiles.zip
unzip -o uautomizer-validate-violation-tmp.*.logfiles.zip
unzip -o witnesslint-validate2-tmp.*.logfiles.zip