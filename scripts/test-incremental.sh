#!/usr/bin/env bash

# Example: ./scripts/test-incremental.sh 00-local
#
# Inspect/diff *.before.log, *.after.incr.log and *.after.scratch.log

test=$1

base=./tests/incremental
source=$base/$test.c
conf=$base/$test.json
patch=$base/$test.patch

args="--enable dbg.debug --enable printstats -v"

./goblint --conf $conf $args --enable incremental.save $source &> $base/$test.before.log

patch -p0 -b <$patch

./goblint --conf $conf $args --enable incremental.load --set save_run $base/$test-incrementalrun $source &> $base/$test.after.incr.log
./goblint --conf $conf $args --enable incremental.only-rename --set save_run $base/$test-originalrun $source &> $base/$test.after.scratch.log
./goblint --conf $conf --enable dbg.compare_runs.diff --compare_runs $base/$test-originalrun $base/$test-incrementalrun $source

patch -p0 -b -R <$patch
rm -r $base/$test-originalrun $base/$test-incrementalrun
