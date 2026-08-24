With ten increments per thread, advancing each phase ghost after its
corresponding write makes every write flow from an earlier digest to a later
one. Dedicated per-digest globals allow Base intervals to establish the scalar
upper bounds without a relational analysis.

The regression command is commented out because its roughly seven-minute
runtime is too long for CI.

# $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --set solvers.td3.side_widen_gas 10 --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set exp.architecture 64bit --set witness.yaml.validate 64-triangular-longer-1.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 64-triangular-longer-1.c 2>&1 | grep -E '^SV-COMP result:|phaseGhost: global ghost_'
# SV-COMP result: true
# [Info][Witness] phaseGhost: global ghost_t1_phase is only accessed by unique thread [main, t1@64-triangular-longer-1.c:36:3-36:38] and has known lower and upper bounds
# [Info][Witness] phaseGhost: global ghost_t2_phase is only accessed by unique thread [main, t2@64-triangular-longer-1.c:37:3-37:38] and has known lower and upper bounds
