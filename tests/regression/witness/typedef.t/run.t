  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.typedefs typedef.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
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
          file_name: typedef.c
          line: 9
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: '*((int *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: '*((int *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: a.f == 43
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: '*((int *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: a.f == 43
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: q == (void *)(& a)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: x == 42
        format: c_expression

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable witness.invariant.typedefs typedef.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    location invariants: 14
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 9
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: '*((myint *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 12
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: '*((myint *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: a.f == 43
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 13
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: ((s *)q)->f == 43
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: '*((myint *)p) == 42'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: a.f == 43
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: p == (void *)(& x)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: q == (void *)(& a)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 14
          column: 3
          function: main
        value: x == 42
        format: c_expression
