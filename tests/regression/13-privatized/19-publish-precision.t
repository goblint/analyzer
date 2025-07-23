  $ goblint --enable dbg.print_protection 19-publish-precision.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (19-publish-precision.c:27:3-27:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (19-publish-precision.c:17:3-17:30)
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (19-publish-precision.c:30:3-30:30)
  [Warning][Assert] Assertion "glob1 == 5" is unknown. (19-publish-precision.c:31:3-31:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Race] Mutex mutex1 read-write protects 0 variable(s): {}
  [Info][Race] Variable glob1 read-write protected by 1 mutex(es): {mutex2}
  [Info][Race] Mutex mutex2 read-write protects 1 variable(s): {glob1}
  [Info][Race] Mutex read-write protection summary:
    Number of mutexes: 2
    Max number variables of protected by a mutex: 1
    Total number of protected variables (including duplicates): 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1


Protection succeeds on check in t_fun.

  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --disable witness.invariant.other 19-publish-precision.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (19-publish-precision.c:27:3-27:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (19-publish-precision.c:17:3-17:30)
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (19-publish-precision.c:30:3-30:30)
  [Warning][Assert] Assertion "glob1 == 5" is unknown. (19-publish-precision.c:31:3-31:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
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
          file_name: 19-publish-precision.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 11
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 11
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 17
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 17
          column: 3
          function: t_fun
        value: glob1 == 5
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 30
          column: 3
          function: main
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 30
          column: 3
          function: main
        value: glob1 <= 127
        format: c_expression


Vojdani does not succeed on check in t_fun.

  $ goblint --set ana.base.privatization vojdani --enable witness.yaml.enabled --set witness.yaml.entry-types '["invariant_set"]' --disable witness.invariant.other 19-publish-precision.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (19-publish-precision.c:27:3-27:30)
  [Warning][Assert] Assertion "glob1 == 5" is unknown. (19-publish-precision.c:17:3-17:30)
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (19-publish-precision.c:30:3-30:30)
  [Warning][Assert] Assertion "glob1 == 5" is unknown. (19-publish-precision.c:31:3-31:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] witness generation summary:
    location invariants: 9
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Vojdani should not have location_invariant-s on glob1 after locking mutex1 (line 11), only after mutex2.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 11
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 12
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 17
          column: 3
          function: t_fun
        value: (unsigned long )arg == 0UL
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 17
          column: 3
          function: t_fun
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 17
          column: 3
          function: t_fun
        value: glob1 <= 127
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 30
          column: 3
          function: main
        value: 0 <= glob1
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 19-publish-precision.c
          line: 30
          column: 3
          function: main
        value: glob1 <= 127
        format: c_expression
