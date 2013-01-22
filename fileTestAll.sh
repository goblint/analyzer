for f in tests/regression/17-file/*.c; do ./goblint --sets result html --sets ana.activated[0][+] file $f; done
./goblint --sets result html --sets ana.activated[0][+] file tests/regression/17-file/*.c
