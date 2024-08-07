`protection` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization protection 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C

`mutex-meet` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization mutex-meet 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C

`write+lock` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization write+lock 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
    location_invariant:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C
