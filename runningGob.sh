#!/bin/bash
make
#make install

# set options and file for apron execution
options="--set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra"        
cfile="tests/regression/55-loop-unrolling/01-simple-cases.c"

# run analysis, write cil output to file and enable visualization via html
#./goblint $cfile $options --enable justcil > output.txt
#./goblint $cfile $options --html 

./goblint --enable warn.debug tests/regression/99-tutorials/01-first.c --set "ana.activated[+]" signs --html 

# set up server to see visualizatino
python3 -m http.server --directory result 8081
#./goblint --enable dbg.debug tests/regression/55-loop-unrolling/01-simple-cases.c --enable ana.int.interval --set "ana.activated[+]" signs --enable justcil > output.txt

