Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.

  $ goblint --set ana.activated[+] extract-pthread 02-starve.c
  Live lines: 8
  No lines with dead code found by solver.
  Total lines (logical LoC): 8
  
  Summary for all memory locations:
  	safe:            2
  	vulnerable:      0
  	unsafe:          0
  	-------------------
  	total:           2
  ExtractPthread.Resource.Thread:mainfun
  ExtractPthread.Resource.Thread:id1_2766
  saved promela model as $TESTCASE_ROOT/pml-result/pthread.pml
  saved graph as $TESTCASE_ROOT/pml-result/pthread.dot
  Copy spin/pthread_base.pml to same folder and then do: spin -a pthread.pml && cc -o pan pan.c && ./pan -a

Run spin.
Inner subshell (parenthesis) to redirect output from all conjuncts.
Outer subshell (parenthesis) to avoid dune's cram redirects from interfering with ours.

  $ ((spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a) > out.txt 2>&1)

Starving, should have trail.

  $ grep pthread.pml.trail out.txt
  pan: wrote pthread.pml.trail
