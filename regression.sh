for f in tests/regression/17-file/*.c;
	do ./goblint --sets result html --sets ana.activated[0][+] file $f 2>/dev/null | python2 test.py $f;
done