Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.

  $ goblint --set ana.activated[+] extract-pthread 01-base.c
  Live lines: 21
  No lines with dead code found by solver.
  Total lines (logical LoC): 21
  [Warning][Race] Memory location g1@01-base.c:4:5-4:7 (race with conf. 110):
    write with [mhp:{tid=[main, t1@01-base.c:29:3-29:38]},
              lock:{mutex1, mutex2}, thread:[main, t1@01-base.c:29:3-29:38]] (conf. 110) (01-base.c:11:3-11:14)
    read with [mhp:{tid=[main]; created={[main, t1@01-base.c:29:3-29:38], [main, t2@01-base.c:30:3-30:38]}}, thread:[main]] (conf. 110) (01-base.c:34:3-34:47)
  
  Summary for all memory locations:
  	safe:            1
  	vulnerable:      0
  	unsafe:          1
  	-------------------
  	total:           2
  ExtractPthread.Resource.Thread:mainfun
  ExtractPthread.Resource.Thread:id1_2769
  ExtractPthread.Resource.Thread:id2_2770
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
