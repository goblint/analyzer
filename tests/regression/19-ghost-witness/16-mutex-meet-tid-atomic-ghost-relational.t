With the interval Apron domain, `mutex-meet-tid-atomic-ghost` can keep the simple
ghost facts, but it cannot validate the linear relations between the phase and
the atomically protected values.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 16-mutex-meet-tid-atomic-ghost-relational.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid-atomic-ghost --set ana.apron.domain interval --set sem.int.signed_overflow assume_none --set colors never 16-mutex-meet-tid-atomic-ghost-relational.c > 16-mutex-meet-tid-atomic-ghost-interval.out 2>&1

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 16-mutex-meet-tid-atomic-ghost-interval.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant confirmed: x == ghost_a" 16-mutex-meet-tid-atomic-ghost-interval.out
  [Success][Witness] invariant confirmed: x == ghost_a (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant unconfirmed: y == 3 * x" 16-mutex-meet-tid-atomic-ghost-interval.out
  [Warning][Witness] invariant unconfirmed: y == 3 * x (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant unconfirmed: y == 3 * ghost_a" 16-mutex-meet-tid-atomic-ghost-interval.out
  [Warning][Witness] invariant unconfirmed: y == 3 * ghost_a (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

With the polyhedra domain, the same privatization validates the protected facts
as linear relations.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 16-mutex-meet-tid-atomic-ghost-relational.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid-atomic-ghost --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none --set colors never 16-mutex-meet-tid-atomic-ghost-relational.c > 16-mutex-meet-tid-atomic-ghost.out 2>&1

  $ grep -F "invariant confirmed: ghost_a >= 0 && ghost_a <= 2" 16-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: ghost_a >= 0 && ghost_a <= 2 (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant confirmed: x == ghost_a" 16-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: x == ghost_a (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant confirmed: y == 3 * x" 16-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: y == 3 * x (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)

  $ grep -F "invariant confirmed: y == 3 * ghost_a" 16-mutex-meet-tid-atomic-ghost.out
  [Success][Witness] invariant confirmed: y == 3 * ghost_a (16-mutex-meet-tid-atomic-ghost-relational.c:29:5)
