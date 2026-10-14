  $ goblint --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled 22-affeq-invariant-constant-sign.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

The correct invariant is `x = z + 1`, not `x + 1 = z`:

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 22-affeq-invariant-constant-sign.c
          line: 7
          column: 3
          function: main
        value: (long long )x + 1LL == (long long )z
        format: c_expression
