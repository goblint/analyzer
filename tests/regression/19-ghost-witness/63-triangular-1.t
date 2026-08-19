With five increments per thread, each digest bucket may itself grow more often
than in the NUM=3 case. Dedicated per-digest globals keep this independent from
the number of newly discovered digests, so a per-bucket gas of ten suffices.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --set solvers.td3.side_widen_gas 10 --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set ana.activated[+] apron --set ana.apron.domain octagon --set ana.relation.privatization mutex-meet-tid-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 63-triangular-1.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 63-triangular-1.c 2>&1 | grep -E '^SV-COMP result:|phaseGhost: global ghost_'
  SV-COMP result: true
  [Info][Witness] phaseGhost: global ghost_t1_phase is only accessed by unique thread [main, t1@63-triangular-1.c:36:3-36:38] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_t2_phase is only accessed by unique thread [main, t2@63-triangular-1.c:37:3-37:38] and has known lower and upper bounds
