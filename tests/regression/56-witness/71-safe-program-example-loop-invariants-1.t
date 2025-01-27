  $ goblint --set ana.activated[+] apron --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --set witness.yaml.invariant-types '["loop_invariant"]' --enable ana.sv-comp.functions 71-safe-program-example-loop-invariants-1.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 13
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )i + 254LL >= (long long )v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )i + 255LL >= (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )n + (long long )i >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )n + 254LL >= (long long )v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )n >= (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )v + (long long )i >= 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )v + (long long )n >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )v + 255LL >= (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: (long long )v + 255LL >= (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: 510 >= (long long )n + (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: 510 >= (long long )v + (long long )i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: 510 >= (long long )v + (long long )n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: n != (unsigned char)0
        format: c_expression
