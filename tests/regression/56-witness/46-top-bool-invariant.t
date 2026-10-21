def_exc only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set --disable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
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
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: (_Bool)0 <= x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x <= (_Bool)1
        format: c_expression

interval only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.def_exc --enable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set --disable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
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
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: (_Bool)0 <= x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x <= (_Bool)1
        format: c_expression

enums only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --enable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set --disable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x == (_Bool)0 || x == (_Bool)1
        format: c_expression

congruence only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --enable ana.int.congruence --disable ana.int.interval_set --disable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []

interval_set only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --enable ana.int.interval_set --disable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
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
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: (_Bool)0 <= x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x <= (_Bool)1
        format: c_expression

bitfield only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set --enable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
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
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: (_Bool)0 <= x
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x <= (_Bool)1
        format: c_expression

all:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable ana.int.def_exc --enable ana.int.interval --enable ana.int.enums --enable ana.int.congruence --enable ana.int.interval_set --enable ana.int.bitfield 46-top-bool-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 1
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 46-top-bool-invariant.c
          line: 5
          column: 3
          function: main
        value: x == (_Bool)0 || x == (_Bool)1
        format: c_expression

all without inexact-type-bounds:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --enable ana.int.def_exc --enable ana.int.interval --enable ana.int.enums --enable ana.int.congruence --enable ana.int.interval_set --enable ana.int.bitfield 46-top-bool-invariant.c --disable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []
