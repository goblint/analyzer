  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable witness.invariant.all-locals 08-witness-all-locals.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 08-witness-all-locals.c
          line: 7
          column: 5
          function: main
        value: x == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 08-witness-all-locals.c
          line: 9
          column: 3
          function: main
        value: x == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 08-witness-all-locals.c
          line: 9
          column: 3
          function: main
        value: y == 10
        format: c_expression

Fewer entries are emitted if locals from nested block scopes are excluded:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.all-locals 08-witness-all-locals.c
  [Warning] Disabling witness.invariant.all-locals implicitly enables cil.addNestedScopeAttr.
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 4
    dead: 0
    total lines: 4
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 08-witness-all-locals.c
          line: 7
          column: 5
          function: main
        value: x == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 08-witness-all-locals.c
          line: 9
          column: 3
          function: main
        value: x == 5
        format: c_expression
