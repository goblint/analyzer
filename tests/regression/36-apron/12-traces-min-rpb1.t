  $ goblint --enable witness.yaml.enabled --enable warn.deterministic --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.other --disable ana.base.invariant.enabled --set ana.relation.privatization mutex-meet --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.apron.domain polyhedra 12-traces-min-rpb1.c
  [Warning][Assert] Assertion "g == h" is unknown. (12-traces-min-rpb1.c:27:3-27:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:16:3-16:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:29:3-29:26)
  [Warning][Race] Memory location g (race with conf. 110): (12-traces-min-rpb1.c:7:5-7:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:14:3-14:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:27:3-27:26)
  [Warning][Race] Memory location h (race with conf. 110): (12-traces-min-rpb1.c:8:5-8:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:15:3-15:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:27:3-27:26)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
  [Info][Witness] witness generation summary:
    location invariants: 3
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 12-traces-min-rpb1.c
          line: 14
          column: 3
          function: t_fun
        value: (long long )h == (long long )g
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 12-traces-min-rpb1.c
          line: 19
          column: 3
          function: t_fun
        value: (long long )h == (long long )g
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 12-traces-min-rpb1.c
          line: 29
          column: 3
          function: main
        value: (long long )h == (long long )g
        format: c_expression


  $ goblint --enable warn.deterministic --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.sv-comp.functions --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 12-traces-min-rpb1.c --enable ana.apron.invariant.diff-box
  [Warning][Assert] Assertion "g == h" is unknown. (12-traces-min-rpb1.c:27:3-27:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:16:3-16:26)
  [Success][Assert] Assertion "g == h" will succeed (12-traces-min-rpb1.c:29:3-29:26)
  [Warning][Race] Memory location g (race with conf. 110): (12-traces-min-rpb1.c:7:5-7:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:14:3-14:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & g) (12-traces-min-rpb1.c:27:3-27:26)
  [Warning][Race] Memory location h (race with conf. 110): (12-traces-min-rpb1.c:8:5-8:10)
    write with [lock:{A}, thread:[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:15:3-15:8)
    read with [mhp:{created={[main, t_fun@12-traces-min-rpb1.c:25:3-25:40]}}, thread:[main]] (conf. 110)  (exp: & h) (12-traces-min-rpb1.c:27:3-27:26)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 2
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 1
    total generation entries: 2

  $ yamlWitnessStrip < witness.yml
  - entry_type: ghost_instrumentation
    content:
      ghost_variables:
      - name: A_locked
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
          file_name: 12-traces-min-rpb1.c
          line: 13
          column: 3
          function: t_fun
        updates:
        - variable: A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 17
          column: 3
          function: t_fun
        updates:
        - variable: A_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 18
          column: 3
          function: t_fun
        updates:
        - variable: A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 19
          column: 3
          function: t_fun
        updates:
        - variable: A_locked
          value: "0"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 25
          column: 3
          function: main
        updates:
        - variable: multithreaded
          value: "1"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 28
          column: 3
          function: main
        updates:
        - variable: A_locked
          value: "1"
          format: c_expression
      - location:
          file_name: 12-traces-min-rpb1.c
          line: 30
          column: 3
          function: main
        updates:
        - variable: A_locked
          value: "0"
          format: c_expression
  - entry_type: invariant_set
    content:
    - invariant:
        type: flow_insensitive_invariant
        value: '! multithreaded || (A_locked || (long long )g == (long long )h)'
        format: c_expression
