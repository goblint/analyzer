#!/usr/bin/env bash

cd ..

make clean

make

rm goblint/sv-comp/goblint.zip

zip goblint/sv-comp/goblint.zip \
    goblint/goblint \
    goblint/conf/svcomp21.json \
    goblint/includes/sv-comp.c \
    goblint/README.md \
    goblint/LICENSE
