ana=${1-"file"}
for f in tests/regression/17-file/*.c;
	do ./goblint --sets result html --sets ana.activated[0][+] $ana --set dbg.showtemps true $f 2>/dev/null | python2 tests/regression.py $f;
done
