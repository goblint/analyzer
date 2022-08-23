Copy Promela model here.

  $ cp -r ../../spin ./spin

Run Goblint extraction.

  $ goblint --set ana.activated[+] extract-pthread 02-starve.c > /dev/null 2>&1

Run spin.

  $ (spin -a pml-result/pthread.pml && cc -o pan pan.c && ./pan -a) > out.txt 2>&1

Starving, should have trail.

  $ grep pthread.pml.trail out.txt
  pan: wrote pthread.pml.trail
