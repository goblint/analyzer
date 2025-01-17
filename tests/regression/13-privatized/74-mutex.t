  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 2
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml | tee witness.flow_insensitive.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: m_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 20
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 23
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 34
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 38
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
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

Flow-insensitive invariants as location invariants.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' --set witness.invariant.flow_insensitive-as location_invariant 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml > witness.location.yml

  $ diff witness.flow_insensitive.yml witness.location.yml
  67,68c67,74
  < - entry_type: flow_insensitive_invariant
  <   flow_insensitive_invariant:
  ---
  > - entry_type: location_invariant
  >   location:
  >     file_name: 74-mutex.c
  >     file_hash: $FILE_HASH
  >     line: 36
  >     column: 3
  >     function: main
  >   location_invariant:
  72,73c78,85
  < - entry_type: flow_insensitive_invariant
  <   flow_insensitive_invariant:
  ---
  > - entry_type: location_invariant
  >   location:
  >     file_name: 74-mutex.c
  >     file_hash: $FILE_HASH
  >     line: 36
  >     column: 3
  >     function: main
  >   location_invariant:
  [1]

Should also work with earlyglobs.
Earlyglobs shouldn't cause protected writes in multithreaded mode from being immediately published to protected invariant.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable exp.earlyglobs 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Same with ghost_instrumentation and invariant_set entries.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' --set witness.invariant.flow_insensitive-as invariant_set-location_invariant 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Witness] witness generation summary:
    location invariants: 2
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: m_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 20
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 23
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 34
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 38
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        value: '! multithreaded || (0 <= used && used <= 1)'
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        value: '! multithreaded || (m_locked || used == 0)'
        format: c_expression

Same protected invariant with vojdani but no unprotected invariant.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization vojdani --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 1
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: m_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 20
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 23
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 34
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 38
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m_locked || used == 0)'
      type: assertion
      format: C

Same as protection with mutex-meet.

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization mutex-meet --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 74-mutex.c
  [Success][Assert] Assertion "used == 0" will succeed (74-mutex.c:37:3-37:29)
  [Warning][Deadcode] Function 'producer' has dead code:
    on line 26 (74-mutex.c:26-26)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 2
    total generation entries: 3
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: m_locked
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 20
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 23
          column: 5
          function: producer
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 34
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 36
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 74-mutex.c
          file_hash: $FILE_HASH
          line: 38
          column: 3
          function: main
        updates:
        - variable: m_locked
          value: "0"
          format: c_expression
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
    live: 14
    dead: 1
    total lines: 15
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (74-mutex.c:19:10-19:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
