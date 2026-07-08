Does not succeed without the phase ghost.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 25-test-easy8.c
  [Info] Enabled widening thresholds
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

The phase change is atomic with thread 2 publishing `c = 1`.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 25-test-easy8.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 25-test-easy8.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (25-test-easy8.c:21:1-21:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 67..68 (25-test-easy8.c:67-68)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 3 (1 in uncalled functions)
    total lines: 29
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@25-test-easy8.c:63:3-63:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 0

The second witness also states the final phase and published Boolean values.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 25-test-easy8-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled 25-test-easy8.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (25-test-easy8.c:21:1-21:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 67..68 (25-test-easy8.c:67-68)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 3 (1 in uncalled functions)
    total lines: 29
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@25-test-easy8.c:63:3-63:37] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: b == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: c == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: ghost_thread2_phase == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: ghost_thread2_phase == c (25-test-easy8.c:66:3)

The second witness also states the final phase and published Boolean values and unassume them.

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level00.json --set witness.yaml.invariant-types[+] location_invariant --set ana.path_sens[+] phaseGhostSplit --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic-ghost --set exp.architecture 64bit --set witness.yaml.validate 25-test-easy8-invariants.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.autotune.activated[-] loopUnrollHeuristic --set ana.autotune.activated[-] congruences --disable witness.yaml.enabled --enable ana.unassume.ghost --enable ana.unassume.precheck --set witness.yaml.unassume 25-test-easy8-invariants.yml --set ana.activated[+] unassume 25-test-easy8.c
  [Info] Enabled widening thresholds
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: true
  [Info][Race] Memory locations race summary:
    safe: 3
    vulnerable: 0
    unsafe: 0
    total memory locations: 3
  [Warning][Deadcode] Function 'main' does not return
  [Warning][Deadcode] Function 'reach_error' is uncalled: 1 LLoC (25-test-easy8.c:21:1-21:22)
  [Warning][Deadcode] Function 'main' has dead code:
    on lines 67..68 (25-test-easy8.c:67-68)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 26
    dead: 3 (1 in uncalled functions)
    total lines: 29
  [Info][Witness] phaseGhost: global ghost_thread2_phase is only accessed by unique thread [main, thread2@25-test-easy8.c:63:3-63:37] and is monotonically increased to known bounds
  [Info][Witness] unassume invariant: ((ghost_thread2_phase == (int )c && (int )c == 1) && (int )b == 1) && ghost_thread2_phase == 1 (25-test-easy8.c:65:3-65:22)
  [Info][Witness] witness validation summary:
    confirmed: 4
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 4
  [Success][Witness] invariant confirmed: b == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: c == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: ghost_thread2_phase == 1 (25-test-easy8.c:66:3)
  [Success][Witness] invariant confirmed: ghost_thread2_phase == c (25-test-easy8.c:66:3)
