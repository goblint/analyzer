  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types '["flow_insensitive_invariant", "ghost_instrumentation"]' 67-ghost-no-unlock.c
  [Success][Assert] Assertion "g1 == 0" will succeed (67-ghost-no-unlock.c:24:3-24:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Witness] witness generation summary:
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
      - name: m1_locked
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
          file_name: 67-ghost-no-unlock.c
          file_hash: $FILE_HASH
          line: 9
          column: 3
          function: t_fun
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          file_hash: $FILE_HASH
          line: 12
          column: 3
          function: t_fun
        updates:
        - variable: m1_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          file_hash: $FILE_HASH
          line: 21
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          file_hash: $FILE_HASH
          line: 23
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (m1_locked || g1 == 0)'
      type: assertion
      format: C
  - entry_type: flow_insensitive_invariant
    flow_insensitive_invariant:
      string: '! multithreaded || (0 <= g1 && g1 <= 1)'
      type: assertion
      format: C
