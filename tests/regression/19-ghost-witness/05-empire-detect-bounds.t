Run witness validation with `phaseGhost` on the empire example and confirm the extra `fun`-exit ghost invariant (but without tid privatization, so we don't get the invariants for free)

  $ goblint --enable ana.sv-comp.functions --set witness.yaml.validate 05-empire-detect-bounds.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet --set colors never 05-empire-detect-bounds.c > 05-empire-detect-bounds.out  2>&1

The witness invariants, including the new `fun`-exit disjunction, are confirmed.

  $ grep -F "invariant confirmed: ghost_b == 1" 05-empire-detect-bounds.out
  [Success][Witness] invariant confirmed: ghost_b == 1 (05-empire-detect-bounds.c:37:5)

  $ grep -F "invariant confirmed: ghost_b == 0 || ghost_b == 1" 05-empire-detect-bounds.out
  [Success][Witness] invariant confirmed: ghost_b == 0 || ghost_b == 1 (05-empire-detect-bounds.c:18:1)

Both witness ghosts are recognized as phase ghosts.

  $ grep -E "phaseGhost: global ghost_a is only accessed by unique thread .* and is only ever increased by one" 05-empire-detect-bounds.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread <tid> and is only ever increased by one

  $ grep -E "phaseGhost: global ghost_b is only accessed by unique thread .* and is only ever increased by one" 05-empire-detect-bounds.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread <tid> and is only ever increased by one
