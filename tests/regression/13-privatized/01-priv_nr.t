`protection` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization protection 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ grep format_version witness.yml
      format_version: "2.0"

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 25
          column: 3
          function: main
        value: glob1 == 5
        format: c_expression

`vojdani` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization vojdani 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 25
          column: 3
          function: main
        value: glob1 == 5
        format: c_expression

`mutex-meet` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization mutex-meet 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 25
          column: 3
          function: main
        value: glob1 == 5
        format: c_expression

`write+lock` privatization:

  $ goblint --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --set ana.base.privatization write+lock 01-priv_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (01-priv_nr.c:22:3-22:30)
  [Success][Assert] Assertion "t == 5" will succeed (01-priv_nr.c:12:3-12:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (01-priv_nr.c:14:3-14:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (01-priv_nr.c:26:3-26:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 11
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 01-priv_nr.c
          line: 25
          column: 3
          function: main
        value: glob1 == 5
        format: c_expression
