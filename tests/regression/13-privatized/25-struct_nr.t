  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 25-struct_nr.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (25-struct_nr.c:26:3-26:30)
  [Success][Assert] Assertion "t == 5" will succeed (25-struct_nr.c:16:3-16:26)
  [Success][Assert] Assertion "glob1 == -10" will succeed (25-struct_nr.c:18:3-18:32)
  [Success][Assert] Assertion "glob1 == 6" will succeed (25-struct_nr.c:30:3-30:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
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
      - name: lock1_mutex_locked
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
          file_name: 25-struct_nr.c
          line: 14
          column: 3
          function: t_fun
        updates:
        - variable: lock1_mutex_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 25-struct_nr.c
          line: 20
          column: 3
          function: t_fun
        updates:
        - variable: lock1_mutex_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 25-struct_nr.c
          line: 27
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 25-struct_nr.c
          line: 28
          column: 3
          function: main
        updates:
        - variable: lock1_mutex_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 25-struct_nr.c
          line: 32
          column: 3
          function: main
        updates:
        - variable: lock1_mutex_locked
          value: "0"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || ((-128 <= glob1 && glob1 <= 127) && glob1 != 0)'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (lock1_mutex_locked || glob1 == 5)'
        format: c_expression
