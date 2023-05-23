#!/bin/bash
make
#make install
options="--set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra"        
cfile="tests/regression/55-loop-unrolling/01-simple-cases.c"
#./goblint $cfile $options --enable justcil > output.txt
./goblint $cfile $options --html 
python3 -m http.server --directory result 8081
#./goblint --enable dbg.debug tests/regression/55-loop-unrolling/01-simple-cases.c --enable ana.int.interval --set "ana.activated[+]" signs --enable justcil > output.txt

