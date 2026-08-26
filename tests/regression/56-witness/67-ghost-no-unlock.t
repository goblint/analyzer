  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 67-ghost-no-unlock.c
  [Success][Assert] Assertion "g1 == 0" will succeed (67-ghost-no-unlock.c:24:3-24:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 2
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
          line: 9
          column: 3
          function: t_fun
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          line: 12
          column: 3
          function: t_fun
        updates:
        - variable: m1_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          line: 21
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 67-ghost-no-unlock.c
          line: 23
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (0 <= g1 && g1 <= 1)'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (m1_locked || g1 == 0)'
        format: c_expression
