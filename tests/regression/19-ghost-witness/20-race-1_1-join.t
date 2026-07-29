Does not succeed without witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit 20-race-1_1-join.c --disable witness.yaml.enabled --set ana.autotune.activated[-] congruence 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
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

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --disable warn.imprecise --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.c --set ana.autotune.activated[-] congruence 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
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

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.c --set ana.autotune.activated[-] congruence 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
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

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 20-race-1_1-join-invariants.yml --set ana.activated[+] unassume 20-race-1_1-join.c --set ana.autotune.activated[-] congruence 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
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

The preprocessed input does not succeed without witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit 20-race-1_1-join.i --disable witness.yaml.enabled 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (20-race-1_1-join.i:55:1-59:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (20-race-1_1-join.i:60:1-64:1)
  [Warning][Deadcode] Function '__uint16_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:65:1-69:1)
  [Warning][Deadcode] Function '__uint32_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:70:1-74:1)
  [Warning][Deadcode] Function '__uint64_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:75:1-79:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 30
    dead: 5 (5 in uncalled functions)
    total lines: 35
  [Warning][Deadcode][CWE-570] condition '0' is always false (20-race-1_1-join.i:1691:73-1691:74)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.i:1719:4-1719:29)

Run the preprocessed input with the second witness, which additionally contains phase/value invariants

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-i-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.i 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (20-race-1_1-join.i:55:1-59:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (20-race-1_1-join.i:60:1-64:1)
  [Warning][Deadcode] Function '__uint16_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:65:1-69:1)
  [Warning][Deadcode] Function '__uint32_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:70:1-74:1)
  [Warning][Deadcode] Function '__uint64_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:75:1-79:1)
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.i:1691:1-1691:166)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 6 (6 in uncalled functions)
    total lines: 35
  [Warning][Deadcode][CWE-570] condition '! expression' is always false (20-race-1_1-join.i:1693:39-1693:50)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.i:1709:7-1709:61] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.i:1700:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.i:1707:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.i:1713:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.i:1722:4)

Run the preprocessed input with the phase ghost witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-i.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled 20-race-1_1-join.i 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (20-race-1_1-join.i:55:1-59:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (20-race-1_1-join.i:60:1-64:1)
  [Warning][Deadcode] Function '__uint16_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:65:1-69:1)
  [Warning][Deadcode] Function '__uint32_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:70:1-74:1)
  [Warning][Deadcode] Function '__uint64_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:75:1-79:1)
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.i:1691:1-1691:166)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 6 (6 in uncalled functions)
    total lines: 35
  [Warning][Deadcode][CWE-570] condition '! expression' is always false (20-race-1_1-join.i:1693:39-1693:50)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.i:1709:7-1709:61] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

Run the preprocessed input with phase/value invariants and unassume them

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 20-race-1_1-join-i-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 20-race-1_1-join-i-invariants.yml --set ana.activated[+] unassume 20-race-1_1-join.i 2>&1 | sed -E 's/^\[Info\] pdev,.*/[Info] <octagon variables>/'
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] <octagon variables>
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function '__bswap_32' is uncalled: 1 LLoC (20-race-1_1-join.i:55:1-59:1)
  [Warning][Deadcode] Function '__bswap_64' is uncalled: 1 LLoC (20-race-1_1-join.i:60:1-64:1)
  [Warning][Deadcode] Function '__uint16_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:65:1-69:1)
  [Warning][Deadcode] Function '__uint32_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:70:1-74:1)
  [Warning][Deadcode] Function '__uint64_identity' is uncalled: 1 LLoC (20-race-1_1-join.i:75:1-79:1)
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (20-race-1_1-join.i:1691:1-1691:166)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 29
    dead: 6 (6 in uncalled functions)
    total lines: 35
  [Warning][Deadcode][CWE-570] condition '! expression' is always false (20-race-1_1-join.i:1693:39-1693:50)
  [Info][Unsound] Unknown address in status has escaped. (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (20-race-1_1-join.i:1719:4-1719:29)
  [Info][Witness] phaseGhost: global ghost_main_phase is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_thread1_phase is only accessed by unique thread [main, thread1@20-race-1_1-join.i:1709:7-1709:61] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.i:1700:4-1700:23)
  [Info][Witness] unassume invariant: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.i:1707:4-1707:23)
  [Info][Witness] unassume invariant: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.i:1708:7-1708:30)
  [Info][Witness] unassume invariant: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.i:1713:4-1713:23)
  [Info][Witness] unassume invariant: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.i:1722:4-1722:23)
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: ghost_thread1_phase == 1 && pdev == 6 (20-race-1_1-join.i:1700:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 1 && pdev == 1 (20-race-1_1-join.i:1707:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 2 && pdev == 3 (20-race-1_1-join.i:1713:4)
  [Success][Witness] invariant confirmed: ghost_main_phase == 3 && pdev == 5 (20-race-1_1-join.i:1722:4)
