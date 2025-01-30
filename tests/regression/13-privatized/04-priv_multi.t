  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 04-priv_multi.c
  [Success][Assert] Assertion "p == 5" will succeed (04-priv_multi.c:50:7-50:30)
  [Success][Assert] Assertion "A == B" will succeed (04-priv_multi.c:71:5-71:28)
  [Warning][Deadcode] Function 'dispose' has dead code:
    on line 53 (04-priv_multi.c:53-53)
    on line 56 (04-priv_multi.c:56-56)
  [Warning][Deadcode] Function 'process' has dead code:
    on line 37 (04-priv_multi.c:37-37)
    on line 40 (04-priv_multi.c:40-40)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 40
    dead: 4
    total lines: 44
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (04-priv_multi.c:25:10-25:11)
  [Warning][Deadcode][CWE-571] condition 'A > 0' is always true (04-priv_multi.c:27:9-27:14)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (04-priv_multi.c:45:10-45:11)
  [Warning][Deadcode][CWE-571] condition 'B > 0' is always true (04-priv_multi.c:47:9-47:14)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 3
    total generation entries: 4
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

  $ yamlWitnessStrip < witness.yml | tee witness.flow_insensitive.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: mutex_A_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: mutex_B_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 15
          column: 5
          function: generate
        updates:
        - variable: mutex_A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 18
          column: 5
          function: generate
        updates:
        - variable: mutex_A_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 26
          column: 5
          function: process
        updates:
        - variable: mutex_A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 29
          column: 7
          function: process
        updates:
        - variable: mutex_B_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 32
          column: 7
          function: process
        updates:
        - variable: mutex_B_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 34
          column: 7
          function: process
        updates:
        - variable: mutex_A_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 46
          column: 5
          function: dispose
        updates:
        - variable: mutex_B_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 49
          column: 7
          function: dispose
        updates:
        - variable: mutex_B_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 63
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 68
          column: 5
          function: main
        updates:
        - variable: mutex_A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 69
          column: 5
          function: main
        updates:
        - variable: mutex_B_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 73
          column: 5
          function: main
        updates:
        - variable: mutex_B_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 04-priv_multi.c
          file_hash: $FILE_HASH
          line: 74
          column: 5
          function: main
        updates:
        - variable: mutex_A_locked
          value: "0"
          format: c_expression
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (mutex_B_locked || (mutex_A_locked || B == 5))'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (mutex_A_locked || A == 5)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || ((0 <= B && B <= 127) && B != 0)'
      type: assertion
      format: C

Flow-insensitive invariants as location invariants.

  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' --set witness.invariant.flow_insensitive-as location_invariant 04-priv_multi.c
  [Success][Assert] Assertion "p == 5" will succeed (04-priv_multi.c:50:7-50:30)
  [Success][Assert] Assertion "A == B" will succeed (04-priv_multi.c:71:5-71:28)
  [Warning][Deadcode] Function 'dispose' has dead code:
    on line 53 (04-priv_multi.c:53-53)
    on line 56 (04-priv_multi.c:56-56)
  [Warning][Deadcode] Function 'process' has dead code:
    on line 37 (04-priv_multi.c:37-37)
    on line 40 (04-priv_multi.c:40-40)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 40
    dead: 4
    total lines: 44
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (04-priv_multi.c:25:10-25:11)
  [Warning][Deadcode][CWE-571] condition 'A > 0' is always true (04-priv_multi.c:27:9-27:14)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (04-priv_multi.c:45:10-45:11)
  [Warning][Deadcode][CWE-571] condition 'B > 0' is always true (04-priv_multi.c:47:9-47:14)
  [Info][Witness] witness generation summary:
    location invariants: 9
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 10
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

  $ yamlWitnessStrip < witness.yml > witness.location.yml

Location invariant at `for` loop in `main` should be on column 3, not 7.

  $ diff witness.flow_insensitive.yml witness.location.yml
  153,154c153,160
  < - entry_type: flow_insensitive_invariant
  <   flow_insensitive_invariant:
  ---
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 67
  >     column: 3
  >     function: main
  >   location_invariant:
  158,159c164,171
  < - entry_type: flow_insensitive_invariant
  <   flow_insensitive_invariant:
  ---
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 67
  >     column: 3
  >     function: main
  >   location_invariant:
  163,164c175,248
  < - entry_type: flow_insensitive_invariant
  <   flow_insensitive_invariant:
  ---
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 67
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || ((0 <= B && B <= 127) && B != 0)'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 65
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || (mutex_B_locked || (mutex_A_locked || B == 5))'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 65
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || (mutex_A_locked || A == 5)'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 65
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || ((0 <= B && B <= 127) && B != 0)'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 64
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || (mutex_B_locked || (mutex_A_locked || B == 5))'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 64
  >     column: 3
  >     function: main
  >   location_invariant:
  >     string: '! multithreaded || (mutex_A_locked || A == 5)'
  >     type: assertion
  >     format: C
  > - entry_type: location_invariant
  >   location:
  >     file_name: 04-priv_multi.c
  >     file_hash: $FILE_HASH
  >     line: 64
  >     column: 3
  >     function: main
  >   location_invariant:
  [1]
