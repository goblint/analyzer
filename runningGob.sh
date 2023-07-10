#!/bin/bash
make
make install
clear

# set options and file for apron execution
options_apron="--set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra --enable warn.debug" #note: preprocessing first needs to be added to apron
options_signs="--set "ana.activated[+]" signs --enable warn.debug" 
options_term="--set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra" 

cfile_loop30="tests/regression/74-loop_termination/30-goto-out-of-inner-loop-terminating.c"      
cfile_loop26="tests/regression/74-loop_termination/26-enter-loop-goto-terminating.c"
cfile_loop28="tests/regression/74-loop_termination/28-do-while-continue-terminating.c"      
cfile_loop7="tests/regression/74-loop_termination/07-nested-for-loop-terminating.c"    
cfile_loop5="tests/regression/74-loop_termination/05-for-loop-terminating.c"          
cfile_loop1="tests/regression/74-loop_termination/01-simple-loop-terminating.c"      
cfile_signs="tests/regression/99-tutorials/01-first.c"
cfile_goto="tests/incremental/02-cfg-comparison/01-added-return-stmt.c"

# run analysis, write cil output to file and enable visualization via html
#./goblint $cfile_loops $options_apron --enable justcil > output.txt
#./goblint $cfile_loops $options_apron --html 

# run analysis, write cil output to file and enable visualization via html
./goblint $cfile_loop30 $options_term --enable justcil > output.txt
./goblint -v $cfile_loop30 $options_term --html 

# set up server to see visualizatino
python3 -m http.server --directory result 8080
#./goblint --enable dbg.debug tests/regression/55-loop-unrolling/01-simple-cases.c --enable ana.int.interval --set "ana.activated[+]" signs --enable justcil > output.txt

