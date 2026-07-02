Does not succeed without the counter ghosts.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit 22-pthread-demo-datarace.c --disable witness.yaml.enabled
  [Info] unrolling loop at 22-pthread-demo-datarace.c:39:5-48:5 with factor 3
  [Info] unrolling loop at 22-pthread-demo-datarace.c:68:5-73:5 with factor 3
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] myglobal, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, i, tmp, tmp___0, i, j, __x, __x, __x, cond
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:43:9-43:14)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:43:9-43:14)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:43:9-43:14)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:71:9-71:28)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:71:9-71:28)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (22-pthread-demo-datarace.c:71:9-71:28)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
  [Warning][Deadcode] Function 'main' does not return
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 27
    dead: 0
    total lines: 27
  [Info][Imprecise] Invalidating expressions: & tmp (22-pthread-demo-datarace.c:64:10-64:71)
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.c:75:10-75:41)

Track each increment at the corresponding `myglobal` assignment. With both
counter ghosts, `protection-atomic-ghost` retains the exact accumulated value.
The run with the full value of 20 takes around 12 minutes, so here we exercise it with three

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 22-pthread-demo-datarace.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (22-pthread-demo-datarace.c:27:1-27:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 1 (1 in uncalled functions)
    total lines: 27
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (22-pthread-demo-datarace.c:28:40-28:47)
  [Info][Imprecise] Invalidating expressions: & tmp (22-pthread-demo-datarace.c:64:10-64:71)
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.c:75:10-75:41)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.c:64:10-64:71] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

The second witness additionally contains final counter and value invariants.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 22-pthread-demo-datarace.c
  [Info] unrolling loop at 22-pthread-demo-datarace.c:39:5-48:5 with factor 3
  [Info] unrolling loop at 22-pthread-demo-datarace.c:68:5-73:5 with factor 3
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] myglobal, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, i, tmp, tmp___0, i, j, __x, __x, __x, cond
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (22-pthread-demo-datarace.c:27:1-27:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 1 (1 in uncalled functions)
    total lines: 27
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (22-pthread-demo-datarace.c:28:40-28:47)
  [Info][Imprecise] Invalidating expressions: & tmp (22-pthread-demo-datarace.c:64:10-64:71)
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.c:75:10-75:41)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.c:64:10-64:71] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_worker_increments == 3 && ghost_main_increments == 3 (22-pthread-demo-datarace.c:80:5)
  [Success][Witness] invariant confirmed: myglobal == 6 (22-pthread-demo-datarace.c:80:5)
  [Success][Witness] invariant confirmed: myglobal == ghost_worker_increments + ghost_main_increments (22-pthread-demo-datarace.c:80:5)
