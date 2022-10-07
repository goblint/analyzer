`protection` privatization:

  $ goblint --enable witness.yaml.enabled --disable witness.invariant.other --set ana.base.privatization protection 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total: 19
  [Info][Witness] witness generation summary:
    total: 4
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C

`mutex-meet` privatization:

  $ goblint --enable witness.yaml.enabled --disable witness.invariant.other --set ana.base.privatization mutex-meet 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total: 19
  [Info][Witness] witness generation summary:
    total: 4
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C

`write+lock` privatization:

  $ goblint --enable witness.yaml.enabled --disable witness.invariant.other --set ana.base.privatization write+lock 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total: 19
  [Info][Witness] witness generation summary:
    total: 4
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: precondition_loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
    precondition:
      string: (unsigned long )arg == 0UL
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 25
      column: 2
      function: main
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 01-priv_nr.c
      file_hash: $STRIPPED_FILE_HASH
      line: 11
      column: 2
      function: t_fun
    loop_invariant:
      string: glob1 == 5
      type: assertion
      format: C
