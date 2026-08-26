  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 68-ghost-ambiguous-idx.c
  [Warning][Assert] Assertion "data == 0" is unknown. (68-ghost-ambiguous-idx.c:24:3-24:29)
  [Warning][Unknown] unlocking mutex (m[4]) which may not be held (68-ghost-ambiguous-idx.c:25:3-25:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 0
    total lines: 15
  [Warning][Race] Memory location data (race with conf. 110): (68-ghost-ambiguous-idx.c:4:5-4:9)
    write with [lock:{m[4]}, thread:[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:9:3-9:9)
    write with [lock:{m[4]}, thread:[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:10:3-10:9)
    read with [mhp:{created={[main, t_fun@68-ghost-ambiguous-idx.c:20:3-20:40]}}, thread:[main]] (conf. 110)  (exp: & data) (68-ghost-ambiguous-idx.c:24:3-24:29)
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
          file_name: 68-ghost-ambiguous-idx.c
          line: 20
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
        value: '! multithreaded || (0 <= data && data <= 1)'
        format: c_expression
