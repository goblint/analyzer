  $ goblint --enable witness.yaml.enabled --enable witness.invariant.other 77-static-invariant.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Info][Witness] witness generation summary:
    location invariants: 5
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 77-static-invariant.c
          line: 10
          column: 1
          function: foo
        value: static_global == 1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 77-static-invariant.c
          line: 10
          column: 1
          function: foo
        value: static_local == 2
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 77-static-invariant.c
          line: 10
          column: 1
          function: foo
        value: static_local_nested == 3
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 77-static-invariant.c
          line: 13
          column: 3
          function: main
        value: static_global == 1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 77-static-invariant.c
          line: 14
          column: 3
          function: main
        value: static_global == 1
        format: c_expression
