#This file runs 3 incremental tests in total. As such it is similar to test-incremental.sh but performs an additional incremental run on top of it.
test=$1

base=./tests/incremental
source=$base/${test}_1.c
conf=$base/$test.json
patch1=$base/${test}_1.patch
patch2=$base/${test}_2.patch

args="--enable warn.debug --enable dbg.timing.enabled -v"

cat $source

./goblint --conf $conf $args --enable incremental.save $source &> $base/$test.before.log --html

patch -p0 -b <$patch1

cat $source

./goblint --conf $conf $args --enable incremental.load --enable incremental.save $source &> $base/$test.after.incr1.log --html

patch -p0 <$patch2

cat $source

./goblint --conf $conf $args --enable incremental.load --enable incremental.save --set save_run $base/$test-incrementalrun $source &> $base/$test.after.incr2.log --html


#./goblint --conf $conf $args --enable incremental.only-rename --set save_run $base/$test-originalrun $source &> $base/$test.after.scratch.log --html
#./goblint --conf $conf --enable solverdiffs --compare_runs $base/$test-originalrun $base/$test-incrementalrun $source --html

patch -p0 -b -R <$patch2
patch -p0 -b -R <$patch1
# rm -r $base/$test-originalrun $base/$test-incrementalrun
rm -r $base/$test-incrementalrun

cat $source
