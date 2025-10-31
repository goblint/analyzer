  $ goblint --set ana.activated[+] var_eq --enable witness.yaml.enabled --enable witness.invariant.other 73-shadowing-invariant.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Info][Witness] witness generation summary:
    location invariants: 6
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

Should not contain invariants containing shadowed variables.
They can be wrong and contradicting.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 8
          column: 3
          function: main
        value: 2 == foo
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 8
          column: 3
          function: main
        value: foo == 2
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 10
          column: 5
          function: main
        value: 2 == foo
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 10
          column: 5
          function: main
        value: foo == 2
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 11
          column: 5
          function: main
        value: 2 == foo
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 73-shadowing-invariant.c
          line: 11
          column: 5
          function: main
        value: foo == 2
        format: c_expression
