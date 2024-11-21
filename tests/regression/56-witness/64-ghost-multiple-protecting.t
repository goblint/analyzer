  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_variable", "ghost_update"]' 64-ghost-multiple-protecting.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

protection doesn't have precise protected invariant for g2.

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 29
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 17
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 10
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 22
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 19
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 16
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 9
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: t_fun
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
      string: '! multithreaded || (m2_locked || (m1_locked || g1 == 0))'
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

  $ goblint --set ana.base.privatization protection-read --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_variable", "ghost_update"]' 64-ghost-multiple-protecting.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 18
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

protection-read has precise protected invariant for g2.

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 29
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 17
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 10
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 22
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 19
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 16
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 9
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: t_fun
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
      string: '! multithreaded || (m2_locked || (m1_locked || g2 == 0))'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m2_locked || (m1_locked || g1 == 0))'
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

  $ goblint --set ana.base.privatization mutex-meet --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_variable", "ghost_update"]' 64-ghost-multiple-protecting.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Witness] witness generation summary:
    total generation entries: 18
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 29
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 17
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 10
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 22
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 19
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m2_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 16
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "1"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 9
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: m1_locked
    expression: "0"
    location:
      file_name: 64-ghost-multiple-protecting.c
      file_hash: $FILE_HASH
      line: 14
      column: 3
      function: t_fun
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
      string: '! multithreaded || (m2_locked || ((0 <= g2 && g2 <= 1) && g1 == 0))'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m1_locked || (g1 == 0 && g2 == 0))'
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