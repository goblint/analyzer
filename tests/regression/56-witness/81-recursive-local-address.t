  $ goblint --enable witness.yaml.enabled 81-recursive-local-address.c > /dev/null

The witness may name main's y through q. It must not equate p with the
current frame's x: only the caller's x has had its address passed to f.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 8
          column: 5
          function: f
        value: p == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 13
          column: 3
          function: main
        value: y == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 14
          column: 3
          function: main
        value: '*q == 0'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 14
          column: 3
          function: main
        value: q == & y
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 14
          column: 3
          function: main
        value: y == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 15
          column: 3
          function: main
        value: '*q == 0'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 15
          column: 3
          function: main
        value: q == & y
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 81-recursive-local-address.c
          line: 15
          column: 3
          function: main
        value: y == 0
        format: c_expression
