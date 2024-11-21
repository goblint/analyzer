  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_variable", "ghost_update"]' 69-ghost-ptr-protection.c
  [Success][Assert] Assertion "*p != 0" will succeed (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location p (race with conf. 110): (69-ghost-ptr-protection.c:7:5-7:12)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:14:3-14:9)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:15:3-15:9)
    read with [mhp:{created={[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]}}, lock:{m1}, thread:[main]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Witness] witness generation summary:
    total generation entries: 12
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3

TODO: Should not contain unsound flow-insensitive invariant m2_locked || (p == & g && *p == 0):

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 69-ghost-ptr-protection.c
      file_hash: $FILE_HASH
      line: 22
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 69-ghost-ptr-protection.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 69-ghost-ptr-protection.c
      file_hash: $FILE_HASH
      line: 16
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 69-ghost-ptr-protection.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 69-ghost-ptr-protection.c
      file_hash: $FILE_HASH
      line: 28
      column: 3
      function: main
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: m2_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: m1_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m2_locked || (p == & g && *p == 0))'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m1_locked || g == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= g && g <= 1)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (*p == 10 || ((0 <= *p && *p <= 1) && p == & g))'
      type: assertion
      format: C
