  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield 01-simple-arith.c
  [Success][Assert] Assertion "a + b == 42" will succeed (01-simple-arith.c:8:3-8:31)
  [Success][Assert] Assertion "a - b == -4" will succeed (01-simple-arith.c:9:3-9:31)
  [Success][Assert] Assertion "a * b == 437" will succeed (01-simple-arith.c:10:3-10:32)
  [Success][Assert] Assertion "a / b == 0" will succeed (01-simple-arith.c:11:3-11:30)
  [Success][Assert] Assertion "a % b == 19" will succeed (01-simple-arith.c:12:3-12:31)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 9
    dead: 0
    total lines: 9
  [Info][Witness] witness generation summary:
    location invariants: 13
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 6
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 8
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 8
          column: 3
          function: main
        value: b == 23
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 9
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 9
          column: 3
          function: main
        value: b == 23
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 10
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 10
          column: 3
          function: main
        value: b == 23
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 11
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 11
          column: 3
          function: main
        value: b == 23
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 12
          column: 3
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 12
          column: 3
          function: main
        value: b == 23
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 13
          column: 1
          function: main
        value: a == 19
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-simple-arith.c
          line: 13
          column: 1
          function: main
        value: b == 23
        format: c_expression
