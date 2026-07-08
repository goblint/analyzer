Without phase information, Goblint is inconclusive.

  $ goblint --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --disable witness.yaml.enabled 30-pointer-lock-allocator.c 2>&1 | grep '^SV-COMP result:'
  SV-COMP result: unknown

The pthread-only phase witness makes the program provable.

  $ goblint --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --disable witness.yaml.enabled --set ana.path_sens[+] phaseGhostSplit --set witness.yaml.validate 30-pointer-lock-allocator.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit 30-pointer-lock-allocator.c 2>&1 | grep '^SV-COMP result:'
  SV-COMP result: true
