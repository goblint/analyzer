Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.

  $ goblint --set ana.activated[+] extract-pthread 00-sanity.c
  Live lines: 4
  No lines with dead code found by solver.
  Total lines (logical LoC): 4
  ExtractPthread.Resource.Thread:mainfun
  saved promela model as $TESTCASE_ROOT/pml-result/pthread.pml
  saved graph as $TESTCASE_ROOT/pml-result/pthread.dot
  Copy spin/pthread_base.pml to same folder and then do: spin -a pthread.pml && cc -o pan pan.c && ./pan -a

Run spin.
Inner subshell (parenthesis) to redirect output from all conjuncts.
Outer subshell (parenthesis) to avoid dune's cram redirects from interfering with ours.

  $ ((spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a) > out.txt 2>&1)

Non-starving, should not have trail.

  $ grep pthread.pml.trail out.txt
  [1]
