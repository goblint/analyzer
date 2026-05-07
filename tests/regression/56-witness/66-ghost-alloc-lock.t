  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts  --set ana.malloc.unique_address_count 1 --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 66-ghost-alloc-lock.c
  [Success][Assert] Assertion "g1 == 0" will succeed (66-ghost-alloc-lock.c:31:3-31:27)
  [Success][Assert] Assertion "g2 == 0" will succeed (66-ghost-alloc-lock.c:34:3-34:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 23
    dead: 0
    total lines: 23
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 4
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4

  $ (yamlWitnessStrip < witness.yml) > new-stripped.yml
  $ ./strip-ghost-alloc.sh new-stripped.yml > new-stripped2.yml
  $ yamlWitnessStripDiff new-stripped2.yml 66-ghost-alloc-lock-expected.yml
