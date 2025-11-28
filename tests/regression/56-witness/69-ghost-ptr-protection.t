  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 69-ghost-ptr-protection.c
  [Success][Assert] Assertion "*p != 0" will succeed (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location p (race with conf. 110): (69-ghost-ptr-protection.c:7:5-7:12)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:14:3-14:9)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:15:3-15:9)
    read with [mhp:{created={[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]}}, lock:{m1}, thread:[main]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 4
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3

Should not contain unsound flow-insensitive invariant m2_locked || (p == & g && *p == 0):

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
      - name: m2_locked
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
          file_name: 69-ghost-ptr-protection.c
          line: 13
          column: 3
          function: t_fun
        updates:
        - variable: m2_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 16
          column: 3
          function: t_fun
        updates:
        - variable: m2_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 22
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 23
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 28
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "0"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (*p == 10 || ((0 <= *p && *p <= 1) && p == & g))'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (0 <= g && g <= 1)'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (m1_locked || g == 0)'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (m2_locked || ((0 <= *p && *p <= 1) && p == & g))'
        format: c_expression

Same with vojdani.

  $ goblint --set ana.base.privatization vojdani --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 69-ghost-ptr-protection.c
  [Success][Assert] Assertion "*p != 0" will succeed (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location p (race with conf. 110): (69-ghost-ptr-protection.c:7:5-7:12)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:14:3-14:9)
    write with [lock:{m2}, thread:[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:15:3-15:9)
    read with [mhp:{created={[main, t_fun@69-ghost-ptr-protection.c:22:3-22:40]}}, lock:{m1}, thread:[main]] (conf. 110)  (exp: & p) (69-ghost-ptr-protection.c:26:3-26:27)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 1
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 1
    total memory locations: 3

Should not contain unsound flow-insensitive invariant m2_locked || (p == & g && *p == 0):

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
      - name: m2_locked
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
          file_name: 69-ghost-ptr-protection.c
          line: 13
          column: 3
          function: t_fun
        updates:
        - variable: m2_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 16
          column: 3
          function: t_fun
        updates:
        - variable: m2_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 22
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 23
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 69-ghost-ptr-protection.c
          line: 28
          column: 3
          function: main
        updates:
        - variable: m1_locked
          value: "0"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (m1_locked || g == 0)'
        format: c_expression
