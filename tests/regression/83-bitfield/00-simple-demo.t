  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable ana.int.bitfield --disable ana.int.def_exc 00-simple-demo.c
  [Success][Assert] Assertion "(state & 5) == 0" will succeed (00-simple-demo.c:28:3-28:36)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Info][Witness] witness generation summary:
    location invariants: 24
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 12
          column: 3
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 14
          column: 5
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 15
          column: 7
          function: main
        value: r == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 15
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 16
          column: 7
          function: main
        value: r == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 16
          column: 7
          function: main
        value: state == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 16
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 18
          column: 5
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 19
          column: 7
          function: main
        value: r == 1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 19
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 20
          column: 7
          function: main
        value: r == 1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 20
          column: 7
          function: main
        value: state == 8
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 20
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 23
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 24
          column: 7
          function: main
        value: state == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 24
          column: 7
          function: main
        value: testvar == 235
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 28
          column: 3
          function: main
        value: (state & -11) == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 28
          column: 3
          function: main
        value: 0 <= state
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 28
          column: 3
          function: main
        value: state <= 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 28
          column: 3
          function: main
        value: testvar == 1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 29
          column: 1
          function: main
        value: (state & -11) == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 29
          column: 1
          function: main
        value: 0 <= state
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 29
          column: 1
          function: main
        value: state <= 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 00-simple-demo.c
          line: 29
          column: 1
          function: main
        value: testvar == 1
        format: c_expression
