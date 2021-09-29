#!/usr/bin/env bash

test=$1

base=./tests/incremental/
source=$base$test.c
conf=$base$test.json
patch=$base$test.patch

args="--enable dbg.debug --enable printstats -v"

./goblint --conf $conf $args --enable incremental.save $source &> $base$test.before.log

patch -b $source $patch

./goblint --conf $conf $args --enable incremental.load $source &> $base$test.after.incr.log
./goblint --conf $conf $args $source &> $base$test.after.scratch.log

patch -b -R $source $patch
