  $ goblint --enable dbg.print_protection --enable warn.deterministic 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Race] Mutex mutex1 read-write protects 1 variable(s): {glob1}
  [Info][Race] Mutex mutex2 read-write protects 0 variable(s): {}
  [Info][Race] Variable glob1 read-write protected by 1 mutex(es): {mutex1}
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
  [Info][Race] Mutex read-write protection summary:
    Number of mutexes: 2
    Max number variables of protected by a mutex: 1
    Total number of protected variables (including duplicates): 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19


Vojdani succeeds on all.

  $ goblint --set ana.base.privatization vojdani --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --disable witness.invariant.other 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 7
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
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: glob1 == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: glob1 == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 32
          column: 3
          function: main
        value: glob1 == 0
        format: c_expression


Protection does not succeed on check in main.

  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --disable witness.invariant.other 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 10
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
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 32
          column: 3
          function: main
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 32
          column: 3
          function: main
        value: glob1 <= 127
        format: c_expression


Protection-read succeeds on all.

  $ goblint --set ana.base.privatization protection-read --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --disable witness.invariant.other 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    location invariants: 7
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
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 15
          column: 3
          function: t_fun
        value: glob1 == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 16
          column: 3
          function: t_fun
        value: glob1 == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 20
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 20-publish-regression.c
          line: 32
          column: 3
          function: main
        value: glob1 == 0
        format: c_expression
