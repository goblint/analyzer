  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 68-ghost-ambiguous-idx.c
  [Warning][Assert] Assertion "data == 0" is unknown. (68-ghost-ambiguous-idx.c:24:3-24:29)
  [Warning][Unknown] unlocking mutex (m[4]) which may not be held (68-ghost-ambiguous-idx.c:25:3-25:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location data (race with conf. 110): (68-ghost-ambiguous-idx.c:4:5-4:9)
    write with [lock:{m[4]}, thread:[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:9:3-9:9)
    write with [lock:{m[4]}, thread:[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:10:3-10:9)
    read with [mhp:{created={[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]}}, thread:[main]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:24:3-24:29)
  [Info][Witness] witness generation summary:
    total generation entries: 8
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 68-ghost-ambiguous-idx.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_4_locked
    expression: "1"
    location:
      file_name: 68-ghost-ambiguous-idx.c
      file_hash: $FILE_HASH
      line: 8
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m_4_locked
    expression: "0"
    location:
      file_name: 68-ghost-ambiguous-idx.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_4_locked
    expression: "0"
    location:
      file_name: 68-ghost-ambiguous-idx.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: m_4_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m_4_locked || data == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= data && data <= 1)'
      type: assertion
      format: C

TODO: there shouldn't be invariant with m_4_locked because it's ambiguously used
