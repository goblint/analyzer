Run witness validation where a ghost update is placed on the first write inside
an atomic block. The ghost update uses its own instrumentation atomic, so it
does not release the program atomic before the second write and the intermediate
value is not visible to the worker.

  $ goblint --disable warn.race --enable ana.int.interval --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 13-atomic-ghost-unlock.yml --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --set ana.base.privatization protection-atomic-ghost --enable ana.int.interval --set colors never 13-atomic-ghost-unlock.c
  [Success][Assert] Assertion "seen != 10" will succeed (13-atomic-ghost-unlock.c:15:5-15:32)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: seen != 10 (13-atomic-ghost-unlock.c:29:5)
