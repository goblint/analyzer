#!/usr/bin/env bash

# must have goblint checked out into goblint not analyzer directory

make clean

git tag -m "SV-COMP 2025" svcomp25

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

# done outside to ensure archive contains goblint/ directory
cd ..

rm goblint/scripts/sv-comp/goblint.zip

zip goblint/scripts/sv-comp/goblint.zip \
    goblint/goblint \
    goblint/lib/libapron.so \
    goblint/lib/liboctD.so \
    goblint/lib/libboxD.so \
    goblint/lib/libpolkaMPQ.so \
    goblint/lib/LICENSE.APRON \
    goblint/conf/svcomp25.json \
    goblint/conf/svcomp25-validate.json \
    goblint/lib/libc/stub/include/assert.h \
    goblint/lib/goblint/runtime/include/goblint.h \
    goblint/lib/libc/stub/src/stdlib.c \
    goblint/lib/libc/stub/src/pthread.c \
    goblint/lib/sv-comp/stub/src/sv-comp.c \
    goblint/README.md \
    goblint/LICENSE
