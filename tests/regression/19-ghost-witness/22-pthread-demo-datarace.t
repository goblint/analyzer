Does not succeed without the counter ghosts.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit 22-pthread-demo-datarace.c --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences  --disable witness.yaml.enabled
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
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
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.c:75:10-75:41)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.c:64:10-64:71] and has known lower and upper bounds
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

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 22-pthread-demo-datarace.c
  [Info] unrolling loop at 22-pthread-demo-datarace.c:39:5-48:5 with factor 3
  [Info] unrolling loop at 22-pthread-demo-datarace.c:68:5-73:5 with factor 3
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
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.c:75:10-75:41)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.c:64:10-64:71] and has known lower and upper bounds
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

Validate the counter ghosts against the preprocessed input. The preprocessed
file already declares bsearch and qsort, so their libc stubs are disabled for
this run.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set pre.cppflags[+] -DGOBLINT_NO_BSEARCH --set pre.cppflags[+] -DGOBLINT_NO_QSORT --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace-i.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 22-pthread-demo-datarace.i
  [Info] unrolling loop at 22-pthread-demo-datarace.i:1323:5-1331:5 with factor 3
  [Info] unrolling loop at 22-pthread-demo-datarace.i:1347:5-1352:5 with factor 3
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:12:1-12:177)
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:66:1-70:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:71:1-75:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 3 (3 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (22-pthread-demo-datarace.i:13:40-13:47)
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.i:1353:10-1353:49)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.i:1343:10-1343:86] and has known lower and upper bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

The preprocessed input also accepts the final counter and value invariants.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set pre.cppflags[+] -DGOBLINT_NO_BSEARCH --set pre.cppflags[+] -DGOBLINT_NO_QSORT --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace-i-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 22-pthread-demo-datarace.i
  [Info] unrolling loop at 22-pthread-demo-datarace.i:1323:5-1331:5 with factor 3
  [Info] unrolling loop at 22-pthread-demo-datarace.i:1347:5-1352:5 with factor 3
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:12:1-12:177)
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:66:1-70:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (22-pthread-demo-datarace.i:71:1-75:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 3 (3 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (22-pthread-demo-datarace.i:13:40-13:47)
  [Info][Imprecise] Invalidating expressions: & tmp___0 (22-pthread-demo-datarace.i:1353:10-1353:49)
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.i:1343:10-1343:86] and has known lower and upper bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_worker_increments == 3 && ghost_main_increments == 3 (22-pthread-demo-datarace.i:1357:5)
  [Success][Witness] invariant confirmed: myglobal == 6 (22-pthread-demo-datarace.i:1357:5)
  [Success][Witness] invariant confirmed: myglobal == ghost_worker_increments + ghost_main_increments (22-pthread-demo-datarace.i:1357:5)

Run with the second witness, which additionally contains final counter and value invariants and unassume them.

  $ goblint  --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --disable warn.imprecise --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 22-pthread-demo-datarace-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 22-pthread-demo-datarace-invariants.yml --set ana.activated[+] unassume 22-pthread-demo-datarace.c 2>&1 | sed -E '/^\[Error\]\[Imprecise\]\[Unsound\] Function definition missing/d'
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
  [Info][Witness] phaseGhost: global ghost_main_increments is only accessed by unique thread [main] and has known lower and upper bounds
  [Info][Witness] phaseGhost: global ghost_worker_increments is only accessed by unique thread [main, thread_function_mutex@22-pthread-demo-datarace.c:64:10-64:71] and has known lower and upper bounds
  [Info][Witness] unassume invariant: (myglobal == 6 && myglobal == ghost_worker_increments + ghost_main_increments) && (ghost_worker_increments == 3 && ghost_main_increments == 3) (22-pthread-demo-datarace.c:75:10-75:41)
  [Info][Witness] unassume invariant: (myglobal == 6 && myglobal == ghost_worker_increments + ghost_main_increments) && (ghost_worker_increments == 3 && ghost_main_increments == 3) (22-pthread-demo-datarace.c:77:7-77:15)
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
