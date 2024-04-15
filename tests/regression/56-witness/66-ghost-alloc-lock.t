  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType  --set ana.malloc.unique_address_count 1 --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 66-ghost-alloc-lock.c
  [Success][Assert] Assertion "g1 == 0" will succeed (66-ghost-alloc-lock.c:31:3-31:27)
  [Success][Assert] Assertion "g2 == 0" will succeed (66-ghost-alloc-lock.c:34:3-34:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 23
    dead: 0
    total lines: 23
  [Info][Witness] witness generation summary:
    total generation entries: 16
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 28
      column: 3
      function: main
  - entry_type: ghost_update
    variable: alloc_m861095507_locked
    expression: "1"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 33
      column: 3
      function: main
  - entry_type: ghost_update
    variable: alloc_m861095507_locked
    expression: "1"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: alloc_m861095507_locked
    expression: "0"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 36
      column: 10
      function: main
  - entry_type: ghost_update
    variable: alloc_m861095507_locked
    expression: "0"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 18
      column: 10
      function: t_fun
  - entry_type: ghost_update
    variable: alloc_m559918035_locked
    expression: "1"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 30
      column: 3
      function: main
  - entry_type: ghost_update
    variable: alloc_m559918035_locked
    expression: "1"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 10
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: alloc_m559918035_locked
    expression: "0"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 36
      column: 10
      function: main
  - entry_type: ghost_update
    variable: alloc_m559918035_locked
    expression: "0"
    location:
      file_name: 66-ghost-alloc-lock.c
      file_hash: $FILE_HASH
      line: 18
      column: 10
      function: t_fun
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: alloc_m861095507_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: alloc_m559918035_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (alloc_m861095507_locked || g2 == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (alloc_m559918035_locked || g1 == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= g2 && g2 <= 1)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= g1 && g1 <= 1)'
      type: assertion
      format: C
