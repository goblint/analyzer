  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --enable ana.int.def_exc --enable ana.int.enums --enable ana.int.interval --enable ana.int.congruence --disable ana.int.interval_set --disable witness.invariant.split-conjunction int.c
  [Success][Assert] Assertion "1" will succeed (int.c:9:5-9:23)
  [Success][Assert] Assertion "1" will succeed (int.c:12:5-12:23)
  [Success][Assert] Assertion "1" will succeed (int.c:15:5-15:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 3

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: int.c
      file_hash: $FILE_HASH
      line: 15
      column: 5
      function: main
    location_invariant:
      string: (i == 5 || i == 42) || i == 101
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: int.c
      file_hash: $FILE_HASH
      line: 12
      column: 5
      function: main
    location_invariant:
      string: 51 <= i && i <= 99
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: int.c
      file_hash: $FILE_HASH
      line: 9
      column: 5
      function: main
    location_invariant:
      string: i <= 99
      type: assertion
      format: C
