  $ goblint --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled 08-apron-invariant-constant-sign.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 08-apron-invariant-constant-sign.c
          line: 7
          column: 3
          function: main
        value: (long long )x == (long long )z + 1LL
        format: c_expression
