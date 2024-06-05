  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_variable", "ghost_update"]' 25-struct_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (25-struct_nr.c:26:3-26:30)
  [Success][Assert] Assertion "t == 5" will succeed (25-struct_nr.c:16:3-16:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (25-struct_nr.c:18:3-18:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (25-struct_nr.c:30:3-30:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 9
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 25-struct_nr.c
      file_hash: $FILE_HASH
      line: 27
      column: 3
      function: main
  - entry_type: ghost_update
    variable: lock1_mutex_locked
    expression: "1"
    location:
      file_name: 25-struct_nr.c
      file_hash: $FILE_HASH
      line: 28
      column: 3
      function: main
  - entry_type: ghost_update
    variable: lock1_mutex_locked
    expression: "1"
    location:
      file_name: 25-struct_nr.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: lock1_mutex_locked
    expression: "0"
    location:
      file_name: 25-struct_nr.c
      file_hash: $FILE_HASH
      line: 32
      column: 3
      function: main
  - entry_type: ghost_update
    variable: lock1_mutex_locked
    expression: "0"
    location:
      file_name: 25-struct_nr.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: t_fun
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: lock1_mutex_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (lock1_mutex_locked || glob1 == 5)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || ((-128 <= glob1 && glob1 <= 127) && glob1 != 0)'
      type: assertion
      format: C
