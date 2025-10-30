All of these disable def_exc to avoid it forcing extra widening iterations in its range.
This matches the var_eq extra widening iteration, so we couldn't test if var_eq unassumes at all.

## From scratch

  $ goblint --set ana.activated[+] var_eq --disable ana.int.def_exc --enable ana.int.interval 74-var_eq-unassume-interval.c
  [Success][Assert] Assertion "i == 100" will succeed (74-var_eq-unassume-interval.c:9:3-9:29)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

Easiest to run again to get evals.
  $ goblint --set ana.activated[+] var_eq --disable ana.int.def_exc --enable ana.int.interval 74-var_eq-unassume-interval.c -v 2>&1 | grep 'evals'
  [Info] vars = 9    evals = 12    narrow_reuses = 1

## Unassume

  $ goblint --set ana.activated[+] var_eq --disable ana.int.def_exc --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 74-var_eq-unassume-interval.yml 74-var_eq-unassume-interval.c
  [Info][Witness] unassume invariant: (long long )i >= 0LL && 100LL - (long long )i >= 0LL (74-var_eq-unassume-interval.c:5:7-5:12)
  [Info][Witness] unassume invariant: (long long )i >= 0LL && 100LL - (long long )i >= 0LL (74-var_eq-unassume-interval.c:7:5-7:8)
  [Success][Assert] Assertion "i == 100" will succeed (74-var_eq-unassume-interval.c:9:3-9:29)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

Evals less than from scratch.
If var_eq didn't do any unassume (and kept i == 0 in first iteration), then there would be extra widening iteration and 10 evals instead.
  $ goblint --set ana.activated[+] var_eq --disable ana.int.def_exc --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 74-var_eq-unassume-interval.yml 74-var_eq-unassume-interval.c -v 2>&1 | grep 'evals'
  [Info] vars = 9    evals = 8    narrow_reuses = 1
