Without phase ghosts, the atomic publications from both loops are joined and
the upper bound on `i` and `j` cannot be established.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --disable witness.yaml.enabled 61-alternating-increments.c 2>&1 | grep '^SV-COMP result:'
  SV-COMP result: unknown

Ghost assignments may refer to the local loop variable `k`. Both ghost updates
are instrumented and attributed to their respective threads (although their
bounds are not precise enough to prove the property).

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --set solvers.td3.side_widen never --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 61-alternating-increments.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 61-alternating-increments.c 2>&1 | grep -E '^SV-COMP result:|phaseGhost: global ghost_'
  SV-COMP result: unknown
  [Info][Witness] phaseGhost: global ghost_t1_phase is only accessed by unique thread [main, t1@61-alternating-increments.c:36:3-36:38] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_t2_phase is only accessed by unique thread [main, t2@61-alternating-increments.c:37:3-37:38] and has known lower and upper bounds
