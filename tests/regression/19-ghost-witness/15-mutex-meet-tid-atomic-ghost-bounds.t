Plain `mutex-meet-tid-atomic` does not keep the ghost phase in its digest, so the
ghost-conditioned atomically protected value remains out of reach.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 15-mutex-meet-tid-atomic-ghost-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid-atomic --set sem.int.signed_overflow assume_none --set colors never 15-mutex-meet-tid-atomic-ghost-bounds.c > 15-mutex-meet-tid-atomic.out 2>&1

  $ grep -F "invariant unconfirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2)" 15-mutex-meet-tid-atomic.out
  [Warning][Witness] invariant unconfirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2) (15-mutex-meet-tid-atomic-ghost-bounds.c:26:5)

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 15-mutex-meet-tid-atomic.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (15-mutex-meet-tid-atomic-ghost-bounds.c:26:5)

With `mutex-meet-tid-atomic-ghost`, the phase ghost becomes part of the digest and
the same atomically protected value invariant is validated.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 15-mutex-meet-tid-atomic-ghost-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid-atomic-ghost --set sem.int.signed_overflow assume_none --set colors never 15-mutex-meet-tid-atomic-ghost-bounds.c > 15-mutex-meet-tid-atomic-ghost.out 2>&1

  $ grep -F "invariant confirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2)" 15-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2) (15-mutex-meet-tid-atomic-ghost-bounds.c:26:5)

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 15-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (15-mutex-meet-tid-atomic-ghost-bounds.c:26:5)
