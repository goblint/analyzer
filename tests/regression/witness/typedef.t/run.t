  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.typedefs typedef.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    total generation entries: 13

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: q == (void *)(& a)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: a.f == 43
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: '*((int *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: a.f == 43
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: '*((int *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: '*((int *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 9
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable witness.invariant.typedefs typedef.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    total generation entries: 14

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: q == (void *)(& a)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: a.f == 43
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: '*((myint *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: main
    location_invariant:
      string: ((s *)q)->f == 43
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: a.f == 43
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: main
    location_invariant:
      string: '*((myint *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: p == (void *)(& x)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: '*((myint *)p) == 42'
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: typedef.c
      file_hash: $FILE_HASH
      line: 9
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
