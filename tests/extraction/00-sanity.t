Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.

  $ goblint --set ana.activated[+] extract-pthread 00-sanity.c > /dev/null 2>&1

Run spin.

  $ (spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a) > out.txt 2>&1

Non-starving, should not have trail.

  $ grep pthread.pml.trail out.txt
  [1]
