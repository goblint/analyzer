  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts  --set ana.malloc.unique_address_count 1 --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 66-ghost-alloc-lock.c
  [Success][Assert] Assertion "g1 == 0" will succeed (66-ghost-alloc-lock.c:31:3-31:27)
  [Success][Assert] Assertion "g2 == 0" will succeed (66-ghost-alloc-lock.c:34:3-34:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 23
    dead: 0
    total lines: 23
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 4
    total generation entries: 5
  [Info][Race] Memory locations race summary:
    safe: 4
    vulnerable: 0
    unsafe: 0
    total memory locations: 4

  $ (yamlWitnessStrip < witness.yml) > new-stripped.yml
  $ ./strip-ghost-alloc.sh new-stripped.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: ALLOC_VAR1_LOCKED
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      - name: ALLOC_VAR2_LOCKED
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
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 10
          column: 3
          function: t_fun
        updates:
        - variable: ALLOC_VAR1_LOCKED
          value: "1"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 13
          column: 3
          function: t_fun
        updates:
        - variable: ALLOC_VAR1_LOCKED
          value: "0"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 14
          column: 3
          function: t_fun
        updates:
        - variable: ALLOC_VAR2_LOCKED
          value: "1"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 17
          column: 3
          function: t_fun
        updates:
        - variable: ALLOC_VAR2_LOCKED
          value: "0"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 28
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 30
          column: 3
          function: main
        updates:
        - variable: ALLOC_VAR1_LOCKED
          value: "1"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 32
          column: 3
          function: main
        updates:
        - variable: ALLOC_VAR1_LOCKED
          value: "0"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 33
          column: 3
          function: main
        updates:
        - variable: ALLOC_VAR2_LOCKED
          value: "1"
          format: c_expression
      - location:
          file_name: 66-ghost-alloc-lock.c
          file_hash: $FILE_HASH
          line: 35
          column: 3
          function: main
        updates:
        - variable: ALLOC_VAR2_LOCKED
          value: "0"
          format: c_expression
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (ALLOC_VAR2_LOCKED || g2 == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (ALLOC_VAR1_LOCKED || g1 == 0)'
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
