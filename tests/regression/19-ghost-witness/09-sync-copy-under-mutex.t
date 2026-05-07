Run witness validation where the worker phase may advance before main locks
`m`, then main snapshots it into a normal ghost variable while holding `m`.
With phase accesses using MCPAccess, this is no longer expected to confirm
because ghost_a is accessed by multiple unique threads.

  $ goblint --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 09-sync-copy-under-mutex.yml --set ana.activated[+] phaseGhost --set ana.activated[+] phaseGhostSplit --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 09-sync-copy-under-mutex.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Warning][Witness] phaseGhost: global ghost_b is only accessed by unique thread [main], but is not only ever increased by one
  [Info][Witness] phaseGhost: global ghost_a is only accessed by unique thread [main, fun@09-sync-copy-under-mutex.c:20:5-20:51] and is only ever increased by one
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_b == ghost_a (09-sync-copy-under-mutex.c:25:5)
