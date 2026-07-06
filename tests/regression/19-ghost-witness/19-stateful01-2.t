Does not succeed without witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --sets ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions  --set ana.base.privatization protection-atomic-ghost --sets exp.architecture 64bit 19-stateful01-2.c --disable witness.yaml.enabled
  [Warning] --sets is deprecated, use --set instead.
  [Warning] --sets is deprecated, use --set instead.
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] data1, data2, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (19-stateful01-2.c:21:3-21:10)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (19-stateful01-2.c:24:3-24:10)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in + (19-stateful01-2.c:31:3-31:11)
  [Warning][Integer > Overflow][CWE-191] Signed integer underflow in binary - (19-stateful01-2.c:34:3-34:11)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 0
    total lines: 29

Run with the second witness, which additionally contains invariants

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 19-stateful01-2-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 19-stateful01-2.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] data1, data2, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (19-stateful01-2.c:13:1-13:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 51 (19-stateful01-2.c:51-51)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 27
    dead: 2 (1 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition 'data1 != 16' is always false (19-stateful01-2.c:49:7-49:28)
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@19-stateful01-2.c:45:3-45:37] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@19-stateful01-2.c:46:3-46:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 2
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
  [Success][Witness] invariant confirmed: data1 == 16 && data2 == 5 (19-stateful01-2.c:49:3)
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 2 && ghost_thread2_phase == 2 (19-stateful01-2.c:49:3)

Run with the second witness, which additionally contains invariants and unassume them

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 19-stateful01-2-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 19-stateful01-2-invariants.yml --set ana.activated[+] unassume 19-stateful01-2.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] data1, data2, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (19-stateful01-2.c:13:1-13:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 51 (19-stateful01-2.c:51-51)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 27
    dead: 2 (1 in uncalled functions)
    total lines: 29
  [Warning][Deadcode][CWE-570] condition 'data1 != 16' is always false (19-stateful01-2.c:49:7-49:28)
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@19-stateful01-2.c:45:3-45:37] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@19-stateful01-2.c:46:3-46:37] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: (data1 == 16 && data2 == 5) && (ghost_thread1_phase == 2 && ghost_thread2_phase == 2) (19-stateful01-2.c:48:3-48:22)
  [Info][Witness] unassume invariant: (data1 == 16 && data2 == 5) && (ghost_thread1_phase == 2 && ghost_thread2_phase == 2) (19-stateful01-2.c:49:7-49:28)
  [Info][Witness] witness validation summary:
    confirmed: 2
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
  [Success][Witness] invariant confirmed: data1 == 16 && data2 == 5 (19-stateful01-2.c:49:3)
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 2 && ghost_thread2_phase == 2 (19-stateful01-2.c:49:3)

Run with witness validation and recognize both thread-owned phase ghosts

  $ goblint --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 19-stateful01-2.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 19-stateful01-2.c
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] data1, data2, i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (19-stateful01-2.c:13:1-13:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 51 (19-stateful01-2.c:51-51)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 27
    dead: 2 (1 in uncalled functions)
    total lines: 29
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@19-stateful01-2.c:46:3-46:37] and is monotonically increased to known bounds
  [Warning][Deadcode][CWE-570] condition 'data1 != 16' is always false (19-stateful01-2.c:49:7-49:28)
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@19-stateful01-2.c:45:3-45:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
