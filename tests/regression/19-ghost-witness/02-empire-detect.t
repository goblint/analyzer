Run witness validation and `phaseGhost` on the witness-driven empire example.

  $ goblint --enable ana.sv-comp.functions --set witness.yaml.validate 02-empire-detect.yml --set ana.activated[+] phaseGhost --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.relation.privatization mutex-meet-tid --set colors never 02-empire-detect.c > 02-empire-detect.out 2>&1

The ghost invariants from the witness are confirmed.

  $ grep -F "invariant confirmed: ghost_a == 1" 02-empire-detect.out
  [Success][Witness] invariant confirmed: ghost_a == 1 (02-empire-detect.c:37:5)

  $ grep -F "invariant confirmed: ghost_b == 1" 02-empire-detect.out
  [Success][Witness] invariant confirmed: ghost_b == 1 (02-empire-detect.c:37:5)

Both witness ghosts are recognized as phase ghosts.

  $ grep -E "phaseGhost: global ghost_a is only accessed by unique thread .* and is only ever increased by one" 02-empire-detect.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread <tid> and is only ever increased by one

  $ grep -E "phaseGhost: global ghost_b is only accessed by unique thread .* and is only ever increased by one" 02-empire-detect.out | sed -E 's/unique thread .*/unique thread <tid> and is only ever increased by one/'
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread <tid> and is only ever increased by one
