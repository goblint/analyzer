  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 16-atomic_priv.c
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:12:3-12:33)
  [Success][Assert] Assertion "myglobal == 6" will succeed (16-atomic_priv.c:14:3-14:33)
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:16:3-16:33)
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:24:3-24:33)
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:26:3-26:33)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Warning][Race] Memory location myglobal (race with conf. 110): (16-atomic_priv.c:8:5-8:17)
    write with [lock:{[__VERIFIER_atomic]}, thread:[main, t_fun@16-atomic_priv.c:23:3-23:40]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:13:3-13:13)
    write with [lock:{[__VERIFIER_atomic]}, thread:[main, t_fun@16-atomic_priv.c:23:3-23:40]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:15:3-15:13)
    read with [mhp:{created={[main, t_fun@16-atomic_priv.c:23:3-23:40]}}, thread:[main]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:24:3-24:33)
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
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "1"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "1"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "0"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 27
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "0"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 17
      column: 3
      function: t_fun
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: __VERIFIER_atomic_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || myglobal == 5'
      type: assertion
      format: C

Non-atomic privatization:

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 16-atomic_priv.c
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:12:3-12:33)
  [Success][Assert] Assertion "myglobal == 6" will succeed (16-atomic_priv.c:14:3-14:33)
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:16:3-16:33)
  [Warning][Assert] Assertion "myglobal == 5" is unknown. (16-atomic_priv.c:24:3-24:33)
  [Success][Assert] Assertion "myglobal == 5" will succeed (16-atomic_priv.c:26:3-26:33)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Warning][Race] Memory location myglobal (race with conf. 110): (16-atomic_priv.c:8:5-8:17)
    write with [lock:{[__VERIFIER_atomic]}, thread:[main, t_fun@16-atomic_priv.c:23:3-23:40]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:13:3-13:13)
    write with [lock:{[__VERIFIER_atomic]}, thread:[main, t_fun@16-atomic_priv.c:23:3-23:40]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:15:3-15:13)
    read with [mhp:{created={[main, t_fun@16-atomic_priv.c:23:3-23:40]}}, thread:[main]] (conf. 110)  (exp: & myglobal) (16-atomic_priv.c:24:3-24:33)
  [Info][Witness] witness generation summary:
    total generation entries: 9
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
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 23
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "1"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "1"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 11
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "0"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 27
      column: 3
      function: main
  - entry_type: ghost_update
    variable: __VERIFIER_atomic_locked
    expression: "0"
    location:
      file_name: 16-atomic_priv.c
      file_hash: $FILE_HASH
      line: 17
      column: 3
      function: t_fun
  - entry_type: ghost_variable
    variable: multithreaded
    scope: global
    type: int
    initial: "0"
  - entry_type: ghost_variable
    variable: __VERIFIER_atomic_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (__VERIFIER_atomic_locked || myglobal == 5)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || ((0 <= myglobal && myglobal <= 127) && myglobal !=
        0)'
      type: assertion
      format: C
