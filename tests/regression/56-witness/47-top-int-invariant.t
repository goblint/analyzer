def_exc only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: x <= 2147483647
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: (-0x7FFFFFFF-1) <= x
      type: assertion
      format: C

interval only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable ana.int.def_exc --enable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: x <= 2147483647
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: (-0x7FFFFFFF-1) <= x
      type: assertion
      format: C

enums only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --enable ana.int.enums --disable ana.int.congruence --disable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: x <= 2147483647
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: (-0x7FFFFFFF-1) <= x
      type: assertion
      format: C

congruence only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --enable ana.int.congruence --disable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 0

  $ yamlWitnessStrip < witness.yml
  []

interval_set only:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable ana.int.def_exc --disable ana.int.interval --disable ana.int.enums --disable ana.int.congruence --enable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: x <= 2147483647
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: (-0x7FFFFFFF-1) <= x
      type: assertion
      format: C

all:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable ana.int.def_exc --enable ana.int.interval --enable ana.int.enums --enable ana.int.congruence --enable ana.int.interval_set 47-top-int-invariant.c --enable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: x <= 2147483647
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 47-top-int-invariant.c
      file_hash: $FILE_HASH
      line: 5
      column: 3
      function: main
    location_invariant:
      string: (-0x7FFFFFFF-1) <= x
      type: assertion
      format: C

all without inexact-type-bounds:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable ana.int.def_exc --enable ana.int.interval --enable ana.int.enums --enable ana.int.congruence --enable ana.int.interval_set 47-top-int-invariant.c --disable witness.invariant.inexact-type-bounds
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 0
    total lines: 2
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 0

  $ yamlWitnessStrip < witness.yml
  []
