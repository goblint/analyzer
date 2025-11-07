  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection-atomic --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 16-atomic_priv.c
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
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 1
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 16-atomic_priv.c
          line: 23
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || myglobal == 5'
        format: c_expression

Non-atomic privatization:

  $ goblint --enable ana.sv-comp.functions --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 16-atomic_priv.c
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
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 2
    total generation entries: 2
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 16-atomic_priv.c
          line: 23
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || ((0 <= myglobal && myglobal <= 127) && myglobal !=
          0)'
        format: c_expression
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || myglobal == 5'
        format: c_expression
