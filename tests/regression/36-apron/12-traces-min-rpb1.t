  $ goblint --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.sv-comp.functions --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType --set witness.yaml.entry-types '["flow_insensitive_invariant"]' 12-traces-min-rpb1.c --enable ana.apron.invariant.diff-box
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:16:3-16:26)
  [Warning][Assert] Assertion "g == h" is unknown. (12-traces-min-rpb1.c:27:3-27:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:29:3-29:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
  [Warning][Race] Memory location h (race with conf. 110): (12-traces-min-rpb1.c:8:5-8:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:15:3-15:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:27:3-27:26)
  [Warning][Race] Memory location g (race with conf. 110): (12-traces-min-rpb1.c:7:5-7:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:14:3-14:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:27:3-27:26)
  [Info][Witness] witness generation summary:
    total generation entries: 10
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_update
    variable: multithreaded
    expression: "1"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 25
      column: 3
      function: main
  - entry_type: ghost_update
    variable: A_locked
    expression: "1"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 28
      column: 3
      function: main
  - entry_type: ghost_update
    variable: A_locked
    expression: "1"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 18
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: A_locked
    expression: "1"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 13
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: A_locked
    expression: "0"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 30
      column: 3
      function: main
  - entry_type: ghost_update
    variable: A_locked
    expression: "0"
    location:
      file_name: 12-traces-min-rpb1.c
      file_hash: $FILE_HASH
      line: 19
      column: 3
      function: t_fun
  - entry_type: ghost_update
    variable: A_locked
    expression: "0"
    location:
      file_name: 12-traces-min-rpb1.c
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
    variable: A_locked
    scope: global
    type: int
    initial: "0"
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (A_locked || ((0LL - (long long )g) + (long long )h
        >= 0LL && (long long )g - (long long )h >= 0LL))'
      type: assertion
      format: C
