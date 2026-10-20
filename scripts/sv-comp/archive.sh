#!/usr/bin/env bash

# must have goblint checked out into goblint not analyzer directory

set -e # Make script fail if any command fails.

make clean

eval $(opam env)

dune build --profile=release src/goblint.exe
rm -f goblint
cp _build/default/src/goblint.exe goblint
chmod +w goblint
# must replace absolute apron runpath to C library with relative
patchelf --set-rpath '$ORIGIN/lib' goblint
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

# done outside to ensure archive contains goblint-par/ directory
cd ..

rm -f goblint-par/scripts/sv-comp/goblint-par.zip

zip -r goblint-par/scripts/sv-comp/goblint-par.zip \
    goblint-par/goblint \
    goblint-par/goblint_runner.py \
    goblint-par/lib/libapron.so \
    goblint-par/lib/liboctD.so \
    goblint-par/lib/libboxD.so \
    goblint-par/lib/libpolkaMPQ.so \
    goblint-par/lib/LICENSE.APRON \
    goblint-par/conf/svcomp27/ \
    goblint-par/lib/libc/stub/include/assert.h \
    goblint-par/lib/goblint-par/runtime/include/goblint.h \
    goblint-par/lib/libc/stub/src/stdlib.c \
    goblint-par/lib/sv-comp/stub/src/sv-comp.c \
    goblint-par/README.md \
    goblint-par/LICENSE \
    goblint-par/smoketest.sh \
    goblint-par/smoketests/
