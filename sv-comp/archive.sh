#!/usr/bin/env bash

# must have goblint checked out into goblint not analyzer directory

make clean

git tag -m "SV-COMP 2023" svcomp23

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
cp _opam/.opam-switch/sources/apron/COPYING lib/LICENSE.APRON

# done outside to ensure archive contains goblint/ directory
cd ..

rm goblint/sv-comp/goblint.zip

zip goblint/sv-comp/goblint.zip \
    goblint/goblint \
    goblint/lib/libapron.so \
    goblint/lib/liboctD.so \
    goblint/lib/libboxD.so \
    goblint/lib/libpolkaMPQ.so \
    goblint/lib/LICENSE.APRON \
    goblint/conf/svcomp23.json \
    goblint/lib/libc/stub/include/assert.h \
    goblint/lib/goblint/runtime/include/goblint.h \
    goblint/lib/libc/stub/src/stdlib.c \
    goblint/lib/libc/stub/src/pthread.c \
    goblint/lib/sv-comp/stub/src/sv-comp.c \
    goblint/README.md \
    goblint/LICENSE
