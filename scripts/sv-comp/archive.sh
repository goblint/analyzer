#!/usr/bin/env bash

# must have goblint checked out into goblint not analyzer directory

make clean

eval $(opam env)

dune build --profile=release src/goblint.exe
rm -f goblint
cp _build/default/src/goblint.exe goblint
chmod +w goblint
# must replace absolute apron runpath to C library with relative
chrpath -r '$ORIGIN/lib' goblint
# copy just necessary apron C libraries
mkdir -p lib/
cp _opam/share/apron/lib/libapron.so lib/
cp _opam/share/apron/lib/liboctD.so lib/
cp _opam/share/apron/lib/libboxD.so lib/
cp _opam/share/apron/lib/libpolkaMPQ.so lib/
wget -O lib/LICENSE.APRON https://raw.githubusercontent.com/antoinemine/apron/master/COPYING
cp scripts/sv-comp/goblint_runner.py .
cp scripts/sv-comp/smoketest.sh .
cp -r scripts/sv-comp/smoketests .

# done outside to ensure archive contains goblint/ directory
cd ..

rm goblint/scripts/sv-comp/goblint.zip

zip -r goblint/scripts/sv-comp/goblint.zip \
    goblint/goblint \
    goblint/goblint_runner.py \
    goblint/lib/libapron.so \
    goblint/lib/liboctD.so \
    goblint/lib/libboxD.so \
    goblint/lib/libpolkaMPQ.so \
    goblint/lib/LICENSE.APRON \
    goblint/conf/svcomp26/ \
    goblint/lib/libc/stub/include/assert.h \
    goblint/lib/goblint/runtime/include/goblint.h \
    goblint/lib/libc/stub/src/stdlib.c \
    goblint/lib/sv-comp/stub/src/sv-comp.c \
    goblint/README.md \
    goblint/LICENSE \
    goblint/smoketest.sh \
    goblint/smoketests/
