  $ goblint --set ana.base.privatization protection --enable witness.yaml.enabled --set ana.activated[+] mutexGhosts --set witness.yaml.entry-types[+] ghost_instrumentation --set witness.yaml.invariant-types[*] flow_insensitive_invariant --set witness.yaml.format-version 2.1-goblint 92-idx_priv.c
  [Success][Assert] Assertion "data == 0" will succeed (92-idx_priv.c:22:3-22:29)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
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
      - name: multithreaded
        scope: global
        type: int
        initial:
          value: "0"
          format: c_expression
      ghost_updates:
      - location:
          file_name: 92-idx_priv.c
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

TODO: protected invariant with m_4_locked without making 56-witness/68-ghost-ambiguous-idx unsound
