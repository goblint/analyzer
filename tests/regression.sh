ana=${1-"file"}
for f in tests/regression/17-file/*.c;
	do ./test.sh $ana $f 2>/dev/null | python2 tests/regression.py $f;
done
