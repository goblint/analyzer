#!/bin/bash
make
make install
./goblint --enable dbg.debug tests/regression/55-loop-unrolling/01-simple-cases.c --set "ana.activated[+]" signs --enable ana.int.interval --enable justcil > output.txt

