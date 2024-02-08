  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["loop_invariant", "location_invariant"]' pr-758.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    total generation entries: 10

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 6
      column: 2
      function: main
    loop_invariant:
      string: (0 <= x && (x <= 9 || x <= 10))
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 2
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 2
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 2
      function: main
    location_invariant:
      string: i == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 2
      function: main
    location_invariant:
      string: a.kaal == 2
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 2
      function: main
    location_invariant:
      string: a.hind == 3
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 2
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 2
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 2
      function: main
    location_invariant:
      string: i == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 12
      column: 2
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
