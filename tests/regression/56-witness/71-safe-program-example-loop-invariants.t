  $ goblint --set ana.activated[+] apron --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --set witness.yaml.invariant-types '["loop_invariant"]' --enable ana.sv-comp.functions 71-safe-program-example-loop-invariants.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 21
    dead: 0
    total lines: 21
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 14
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )i + 255LL >= (long long )v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )i + 4294967295LL >= (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )n + (long long )i >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )n + 254LL >= (long long )v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )n >= (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )v + (long long )i >= 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )v + (long long )n >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )v + 4294967295LL >= (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: (long long )v + 4294967295LL >= (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: 4294967550LL >= (long long )v + (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: 4294967550LL >= (long long )v + (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: 8589934590LL >= (long long )n + (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: n != 0U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: main
        value: v <= 255U
        format: c_expression
