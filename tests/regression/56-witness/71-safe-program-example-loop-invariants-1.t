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
        value: 510 >= n + i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: 510 >= v + i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: 510 >= v + n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: i + 254 >= v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: i + 255 >= n
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
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: n + 254 >= v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: n + i >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: n >= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: v + 255 >= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: v + 255 >= n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: v + i >= 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 71-safe-program-example-loop-invariants-1.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: main
        value: v + n >= 1
        format: c_expression
