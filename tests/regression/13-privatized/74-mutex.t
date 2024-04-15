  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 1
    total lines: 16
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
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
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 34
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "1"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 36
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "1"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 20
      column: 5
      function: producer
  - entry_type: ghost_update
    variable: m_locked
    expression: "0"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 38
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "0"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 23
      column: 5
      function: producer
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: m_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m_locked || used == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= used && used <= 1)'
      type: assertion
      format: C

Should also work with earlyglobs.
Earlyglobs shouldn't cause protected writes in multithreaded mode from being immediately published to protected invariant.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable exp.earlyglobs 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 1
    total lines: 16
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Same with mutex-meet.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization mutex-meet --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 1
    total lines: 16
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
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
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 34
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "1"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 36
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "1"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 20
      column: 5
      function: producer
  - entry_type: ghost_update
    variable: m_locked
    expression: "0"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 38
      column: 3
      function: main
  - entry_type: ghost_update
    variable: m_locked
    expression: "0"
    location:
      file_name: 74-mutex.c
      file_hash: $FILE_HASH
      line: 23
      column: 5
      function: producer
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: m_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m_locked || used == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= used && used <= 1)'
      type: assertion
      format: C

Should also work with earlyglobs.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization mutex-meet --enable exp.earlyglobs 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 1
    total lines: 16
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
