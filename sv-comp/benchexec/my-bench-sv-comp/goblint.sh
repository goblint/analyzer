#!/bin/bash

shopt -s extglob

MYBENCHDIR=/home/simmo/benchexec/my-bench-sv-comp
RESULTSDIR=$MYBENCHDIR/results3-trivial

mkdir $RESULTSDIR

# Run verification
cd /home/simmo/sv-comp/goblint
benchexec --outputpath $RESULTSDIR $MYBENCHDIR/goblint.xml

# Extract witness directory
cd $RESULTSDIR
LOGDIR=`echo goblint.*.files`
echo $LOGDIR

# Construct validation XMLs
cd $MYBENCHDIR
# CPAChecker
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" cpa-validate-correctness.xml > cpa-validate-correctness-tmp.xml
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" cpa-validate-violation.xml > cpa-validate-violation-tmp.xml
# Ultimate
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" uautomizer-validate-correctness.xml > uautomizer-validate-correctness-tmp.xml
sed -e "s|RESULTSDIR|$RESULTSDIR|" -e "s/LOGDIR/$LOGDIR/" uautomizer-validate-violation.xml > uautomizer-validate-violation-tmp.xml

# Run validation
# CPAChecker
cd /home/simmo/benchexec/tools/CPAchecker-1.9-unix
benchexec --outputpath $RESULTSDIR $MYBENCHDIR/cpa-validate-correctness-tmp.xml
benchexec --outputpath $RESULTSDIR $MYBENCHDIR/cpa-validate-violation-tmp.xml
# Ultimate
cd /home/simmo/benchexec/tools/UAutomizer-linux
benchexec --outputpath $RESULTSDIR $MYBENCHDIR/uautomizer-validate-correctness-tmp.xml
benchexec --outputpath $RESULTSDIR $MYBENCHDIR/uautomizer-validate-violation-tmp.xml

# Merge witness validation results
cd $RESULTSDIR
python3 /home/simmo/benchexec/mergeBenchmarkSets.py goblint.*.results.!(*merged*).xml.bz2 cpa-validate-correctness-tmp.*.results.*.xml.bz2 cpa-validate-violation-tmp.*.results.*.xml.bz2 uautomizer-validate-correctness-tmp.*.results.*.xml.bz2 uautomizer-validate-violation-tmp.*.results.*.xml.bz2

# Generate table with merged results and witness validation results
# table-generator goblint.*.results.*.xml.bz2.merged.xml.bz2 cpa-validate-correctness-tmp.*.results.*.xml.bz2 cpa-validate-violation-tmp.*.results.*.xml.bz2 uautomizer-validate-correctness-tmp.*.results.*.xml.bz2 uautomizer-validate-violation-tmp.*.results.*.xml.bz2
sed -e "s/LOGDIR/$LOGDIR/" ../table-generator-witness.xml > table-generator.xml
table-generator -x table-generator.xml

# Decompress all tool outputs for table HTML links
unzip -o goblint.*.logfiles.zip
unzip -o cpa-validate-correctness-tmp.*.logfiles.zip
unzip -o cpa-validate-violation-tmp.*.logfiles.zip
unzip -o uautomizer-validate-correctness-tmp.*.logfiles.zip
unzip -o uautomizer-validate-violation-tmp.*.logfiles.zip