Does not succeed without the ghost witness.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 40
    dead: 0
    total lines: 40
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.c:62:3-62:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.c:63:3-63:30)

Validate the phase ghost associated with assignments to `total` using the level-00 setup.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.c:16:1-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 39
    dead: 1 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (23-arithmetic-prog-ok.c:17:40-17:45)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.c:62:3-62:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.c:63:3-63:30)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.c:65:3-65:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

The second witness additionally states the final phase, value, and their relation.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --disable warn.imprecise --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.c:16:1-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 39
    dead: 1 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (23-arithmetic-prog-ok.c:17:40-17:45)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.c:65:3-65:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_total_phase == 5 (23-arithmetic-prog-ok.c:68:3)
  [Success][Witness] invariant confirmed: total == 10 (23-arithmetic-prog-ok.c:68:3)
  [Success][Witness] invariant confirmed: total == 2 * ghost_total_phase (23-arithmetic-prog-ok.c:68:3)


The second witness additionally states the final phase, value, and their relation and unassume them.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --disable warn.imprecise --set witness.yaml.unassume 23-arithmetic-prog-ok-invariants.yml --set ana.activated[+] unassume 23-arithmetic-prog-ok.c 2>&1 | sed -E '/^\[Error\]\[Imprecise\]\[Unsound\] Function definition missing/d'
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.c:16:1-16:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 39
    dead: 1 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-570] condition '! cond' is always false (23-arithmetic-prog-ok.c:17:40-17:45)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.c:65:3-65:37] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: (total == (unsigned long )(2 * ghost_total_phase) && total == 10UL) && ghost_total_phase == 5 (23-arithmetic-prog-ok.c:67:3-67:22)
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_total_phase == 5 (23-arithmetic-prog-ok.c:68:3)
  [Success][Witness] invariant confirmed: total == 10 (23-arithmetic-prog-ok.c:68:3)
  [Success][Witness] invariant confirmed: total == 2 * ghost_total_phase (23-arithmetic-prog-ok.c:68:3)

The preprocessed input does not succeed without the ghost witness.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.i
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 40
    dead: 0
    total lines: 40
  [Warning][Deadcode][CWE-570] condition '0' is always false (23-arithmetic-prog-ok.i:18:73-18:74)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.i:911:3-911:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.i:912:3-912:30)

Validate the phase ghost against the preprocessed input.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok-i.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.i
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.i:18:1-18:151)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 919 (23-arithmetic-prog-ok.i:919-919)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 38
    dead: 2 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-571] condition 'total == 10UL' is always true (23-arithmetic-prog-ok.i:918:8-918:36)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.i:911:3-911:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.i:912:3-912:30)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.i:914:3-914:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

The preprocessed input also accepts the final phase, value, and relation invariants.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok-i-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 23-arithmetic-prog-ok.i
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.i:18:1-18:151)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 919 (23-arithmetic-prog-ok.i:919-919)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 38
    dead: 2 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-571] condition 'total == 10UL' is always true (23-arithmetic-prog-ok.i:918:8-918:36)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.i:911:3-911:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.i:912:3-912:30)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.i:914:3-914:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_total_phase == 5 (23-arithmetic-prog-ok.i:922:3)
  [Success][Witness] invariant confirmed: total == 10 (23-arithmetic-prog-ok.i:922:3)
  [Success][Witness] invariant confirmed: total == 2 * ghost_total_phase (23-arithmetic-prog-ok.i:922:3)

The preprocessed input also unassumes the phase/value invariants.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 23-arithmetic-prog-ok-i-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 23-arithmetic-prog-ok-i-invariants.yml --set ana.activated[+] unassume 23-arithmetic-prog-ok.i
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (23-arithmetic-prog-ok.i:18:1-18:151)
  [Warning][Deadcode] Function 'main' has dead code:
    on line 919 (23-arithmetic-prog-ok.i:919-919)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 38
    dead: 2 (1 in uncalled functions)
    total lines: 40
  [Warning][Deadcode][CWE-571] condition 'total == 10UL' is always true (23-arithmetic-prog-ok.i:918:8-918:36)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& empty) (23-arithmetic-prog-ok.i:911:3-911:31)
  [Info][Imprecise] Invalidating expressions: (pthread_cond_t * __restrict  )(& full) (23-arithmetic-prog-ok.i:912:3-912:30)
  [Info][Witness] phaseGhost: global ghost_total_phase is only accessed by unique thread [main, thread2@23-arithmetic-prog-ok.i:914:3-914:37] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: (total == (unsigned long )(2 * ghost_total_phase) && total == 10UL) && ghost_total_phase == 5 (23-arithmetic-prog-ok.i:917:7-917:11)
  [Info][Witness] unassume invariant: (total == (unsigned long )(2 * ghost_total_phase) && total == 10UL) && ghost_total_phase == 5 (23-arithmetic-prog-ok.i:918:8-918:36)
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: ghost_total_phase == 5 (23-arithmetic-prog-ok.i:922:3)
  [Success][Witness] invariant confirmed: total == 10 (23-arithmetic-prog-ok.i:922:3)
  [Success][Witness] invariant confirmed: total == 2 * ghost_total_phase (23-arithmetic-prog-ok.i:922:3)
