  $ goblint --enable witness.yaml.enabled --disable witness.invariant.accessed 35-base-mutex-macos.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

There should be no invariants about __sig.
Base analysis should hide mutex contents.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []
