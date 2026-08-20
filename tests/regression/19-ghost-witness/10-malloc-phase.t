Check that the phase propagation also happens for memory that is dynamically allocated.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 10-malloc-phase.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 10-malloc-phase.c
  [Warning][Assert] Assertion "*published == 10" is unknown. (10-malloc-phase.c:46:5-46:38)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Warning][Witness] invariant unconfirmed: *published == 10 (10-malloc-phase.c:46:5)
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] phaseGhost: global ghost_b is only accessed by unique thread [main] and is monotonically increased to known bounds
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
