Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.
Subshell (parenthesis) to avoid dune's cram redirects from interfering with ours.

  $ (goblint --set ana.activated[+] extract-pthread 01-base.c > /dev/null 2>&1)

Run spin.
Inner subshell (parenthesis) to redirect output from all conjuncts.
Outer subshell (parenthesis) to avoid dune's cram redirects from interfering with ours.

  $ ((spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a) > out.txt 2>&1)

Starving, should have trail.

  $ grep pthread.pml.trail out.txt
  pan: wrote pthread.pml.trail
