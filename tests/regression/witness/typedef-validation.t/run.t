Validate generated invariants involving two variables with the same typedef.
Separate invariant entries pass because the checker resets between expressions.
The nested typedef also requires its underlying declaration to be registered first.

  $ goblint --disable ana.dead-code.lines --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' typedef.c
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ goblint --disable ana.dead-code.lines --enable warn.deterministic --set witness.yaml.validate witness.yml typedef.c
  [Info][Witness] witness validation summary:
    confirmed: 3
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 3
  [Success][Witness] invariant confirmed: x == 1UL (typedef.c:6:3)
  [Success][Witness] invariant confirmed: x == 1UL (typedef.c:7:3)
  [Success][Witness] invariant confirmed: y == 2UL (typedef.c:7:3)

Keeping the conjunction in one invariant must also validate. This exercises
repeated uses of size_t within a single standalone expression check.

  $ goblint --disable ana.dead-code.lines --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.split-conjunction typedef.c
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
          file_name: typedef.c
          line: 6
          column: 3
          function: main
        value: x == 1UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: typedef.c
          line: 7
          column: 3
          function: main
        value: x == 1UL && y == 2UL
        format: c_expression

  $ goblint --disable ana.dead-code.lines --enable warn.deterministic --set witness.yaml.validate witness.yml typedef.c
  [Info][Witness] witness validation summary:
    confirmed: 2
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 2
  [Success][Witness] invariant confirmed: x == 1UL (typedef.c:6:3)
  [Success][Witness] invariant confirmed: x == 1UL && y == 2UL (typedef.c:7:3)
