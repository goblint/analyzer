  $ goblint --set ana.activated[+] var_eq --enable witness.yaml.enabled 71-var_eq-invariants.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
  [Info][Witness] witness generation summary:
    location invariants: 6
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

With 4 equal variables, there should be just 3 var_eq invariants, not 6.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: a == b
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: a == c
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: a == d
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: b == c
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: b == d
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 71-var_eq-invariants.c
          line: 6
          column: 3
          function: main
        value: c == d
        format: c_expression
