  $ goblint --set ana.activated[+] apron --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --set witness.yaml.invariant-types '["loop_invariant"]' --enable ana.sv-comp.functions 75-safe-program-example-loop-invariants-2.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 21
    dead: 0
    total lines: 21
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 25
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )i + 255 >= v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )n + 254 >= v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )n + i >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )s + 255 >= v
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )s + i >= 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )s + n >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )v + i >= 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: (long long )v + n >= 1
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 4294967550LL >= (long long )v + i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 4294967550LL >= (long long )v + n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 4294967550LL >= (long long )v + s
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 8589934590LL >= (long long )n + i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 8589934590LL >= (long long )s + i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: 8589934590LL >= (long long )s + n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: i + 4294967294LL >= s
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: i + 4294967295LL >= n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: n != 0U
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: n + 4294967294LL >= s
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: n >= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: s + 4294967295LL >= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: s + 4294967295LL >= n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: v + 4294967295LL >= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: v + 4294967295LL >= n
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: v + 4294967295LL >= s
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 75-safe-program-example-loop-invariants-2.c
          line: 13
          column: 3
          function: main
        value: v <= 255U
        format: c_expression
