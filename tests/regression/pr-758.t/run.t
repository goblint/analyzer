  $ goblint --enable ana.int.interval --enable witness.yaml.enabled pr-758.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total: 6
  [Info][Witness] witness generation summary:
    total: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: pr-758.c
      file_hash: $STRIPPED_FILE_HASH
      line: 21
      column: 2
      function: main
    loop_invariant:
      string: a.hind == 3
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: pr-758.c
      file_hash: $STRIPPED_FILE_HASH
      line: 20
      column: 14
      function: main
    loop_invariant:
      string: i == 0
      type: assertion
      format: C
