ana=${1-"file"}
r=0
for f in tests/regression/18-file/*.c; do
	./test.sh $ana $f 2>/dev/null | python2 tests/regression.py $f || r=1
	# r=$? && $r
done
exit $r
