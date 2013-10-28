ana=${1-"file"}
r=0
n=0
c=0
for f in tests/regression/18-file/*.c; do
	./test.sh $ana $f 2>/dev/null | python2 tests/regression.py $f && ((c++)) || r=1
	((n++))
done
echo "passed $c/$n tests"
exit $r
