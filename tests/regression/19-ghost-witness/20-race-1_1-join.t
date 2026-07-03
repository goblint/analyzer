Does not succeed without witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit 20-race-1_1-join.c --disable witness.yaml.enabled
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] pdev, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, tmp, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 0
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! (pdev == 1)' is always false (20-race-1_1-join.c:30:8-30:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 3)' is always false (20-race-1_1-join.c:41:8-41:18)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.c:51:4-51:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.c:51:4-51:29)

Run with the second witness, which additionally contains phase/value invariants.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] pdev, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, tmp, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.c:10:1-10:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 1 (1 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! (pdev == 6)' is always false (20-race-1_1-join.c:21:8-21:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 1)' is always false (20-race-1_1-join.c:30:8-30:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 3)' is always false (20-race-1_1-join.c:41:8-41:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 5)' is always false (20-race-1_1-join.c:55:8-55:18)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.c:51:4-51:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.c:51:4-51:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.c:33:7-33:46] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.c:21:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.c:30:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.c:41:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.c:55:4)

The main and worker phase ghosts keep the values published in each phase
separate, allowing all four assertions to be verified.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] pdev, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, tmp, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.c:10:1-10:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 1 (1 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! (pdev == 6)' is always false (20-race-1_1-join.c:21:8-21:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 1)' is always false (20-race-1_1-join.c:30:8-30:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 3)' is always false (20-race-1_1-join.c:41:8-41:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 5)' is always false (20-race-1_1-join.c:55:8-55:18)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.c:51:4-51:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.c:51:4-51:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.c:33:7-33:46] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

Run with the second witness, which additionally contains phase/value invariants and unassume them

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled --enable ana.unassume.ghost --set witness.yaml.unassume 20-race-1_1-join-invariants.yml --set ana.activated[+] unassume 20-race-1_1-join.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] pdev, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, tmp, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.c:10:1-10:22)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 28
    dead: 1 (1 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition '! (pdev == 6)' is always false (20-race-1_1-join.c:21:8-21:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 1)' is always false (20-race-1_1-join.c:30:8-30:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 3)' is always false (20-race-1_1-join.c:41:8-41:18)
  [Warning][Deadcode][CWE-570] condition '! (pdev == 5)' is always false (20-race-1_1-join.c:55:8-55:18)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.c:51:4-51:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.c:51:4-51:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.c:33:7-33:46] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.c:21:8-21:18)
  [Info][Witness] unassume invariant: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.c:30:8-30:18)
  [Info][Witness] unassume invariant: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.c:41:8-41:18)
  [Info][Witness] unassume invariant: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.c:55:8-55:18)
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.c:21:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.c:30:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.c:41:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.c:55:4)
