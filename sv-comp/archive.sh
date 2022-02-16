#!/usr/bin/env bash

# must have goblint checked out into goblint not analyzer directory

make clean
make

# done outside to ensure archive contains goblint/ directory
cd ..

rm goblint/sv-comp/goblint.zip

zip goblint/sv-comp/goblint.zip \
    goblint/goblint \
    goblint/conf/svcomp22.json \
    goblint/includes/stdlib.c \
    goblint/includes/pthread.c \
    goblint/includes/sv-comp.c \
    goblint/README.md \
    goblint/LICENSE
