Plain `mutex-meet-tid` does not keep the ghost phase in its digest, so the
ghost-conditioned protected value remains out of reach.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 14-mutex-meet-tid-ghost-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization mutex-meet-tid --enable ana.int.interval --set colors never 14-mutex-meet-tid-ghost-bounds.c > 14-mutex-meet-tid.out 2>&1

  $ grep -F "invariant unconfirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2)" 14-mutex-meet-tid.out
  [Warning][Witness] invariant unconfirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2) (14-mutex-meet-tid-ghost-bounds.c:25:5)

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 14-mutex-meet-tid.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (14-mutex-meet-tid-ghost-bounds.c:25:5)

With `mutex-meet-tid-ghost`, the phase ghost becomes part of the digest and the
same protected value invariant is validated.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 14-mutex-meet-tid-ghost-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization mutex-meet-tid-ghost --enable ana.int.interval --set colors never 14-mutex-meet-tid-ghost-bounds.c > 14-mutex-meet-tid-ghost.out 2>&1

  $ grep -F "invariant confirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2)" 14-mutex-meet-tid-ghost.out
  [Success][Witness] invariant confirmed: (ghost_a == 0 && x == 0) || (ghost_a == 1 && x == 1) || (ghost_a == 2 && x == 2) (14-mutex-meet-tid-ghost-bounds.c:25:5)

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 14-mutex-meet-tid-ghost.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (14-mutex-meet-tid-ghost-bounds.c:25:5)
