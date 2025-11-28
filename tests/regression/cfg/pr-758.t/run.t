  $ cfgDot pr-758.c

  $ graph-easy --as=boxart main.dot
  
    ┌────────────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                        │
    │                                                ┌────────────────────────────────────┐  │
    │                                                │ main()                             │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │
    │                                                  │ (body)                              │
    │                                                  ▼                                     │
    │                                                ┌────────────────────────────────────┐  │
    │                                                │ pr-758.c:5:3-5:13                  │  │
    │                                                │ (pr-758.c:5:7-5:13 (synthetic))    │  │
    │                                                │ YAML loc: pr-758.c:5:3-5:13        │  │
    │                                                │ server: false                      │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │ x = x + 1
    │                                                  │ x = 42                              │
    │                                                  ▼                                     │
    │                                                ┌────────────────────────────────────┐  │
    │                                                │ pr-758.c:6:3-8:3                   │  │
    │                                                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
    │                                                │ YAML loc: pr-758.c:6:3-8:3         │  │
    │                                                │ server: false                      │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │
    │                                                  │ x = 0                               │
    │                                                  ▼                                     │
  ┌─────────────────────────────────┐                ┌────────────────────────────────────┐  │
  │                                 │                │ pr-758.c:6:3-8:3 (synthetic)       │  │
  │ pr-758.c:6:3-8:3 (synthetic)    │                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
  │ (pr-758.c:6:7-6:26 (synthetic)) │                │ [pr-758.c:6:3-8:3 (synthetic)      │  │
  │ server: false                   │                │ (unknown)]                         │  │
  │                                 │  Pos(x < 10)   │ YAML loop: pr-758.c:6:3-8:3        │  │
  │                                 │ ◀───────────── │ server: false                      │ ◀┘
  └─────────────────────────────────┘                └────────────────────────────────────┘
                                                       │
                                                       │ Neg(x < 10)
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12                │
                                                     │ (pr-758.c:12:3-12:12)              │
                                                     │ YAML loc: pr-758.c:12:3-12:12      │
                                                     │ server: true                       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ k = 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12 (synthetic)    │
                                                     │ (pr-758.c:12:3-12:12 (synthetic))  │
                                                     │ server: false                      │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ i = k
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25                │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ YAML loc: pr-758.c:20:3-20:25      │
                                                     │ server: false                      │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.kaal = 2
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25 (synthetic)    │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ server: false                      │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.hind = 3
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:21:3-21:11                │
                                                     │ (pr-758.c:21:10-21:11)             │
                                                     │ YAML loc: pr-758.c:21:3-21:11      │
                                                     │ server: true                       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ return 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ return of main()                   │
                                                     └────────────────────────────────────┘



  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant", "location_invariant"]' pr-758.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    location invariants: 10
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 6
          column: 3
          function: main
        value: x == 42
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 12
          column: 3
          function: main
        value: x == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 20
          column: 3
          function: main
        value: i == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 20
          column: 3
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 20
          column: 3
          function: main
        value: x == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 21
          column: 3
          function: main
        value: a.hind == 3
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 21
          column: 3
          function: main
        value: a.kaal == 2
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 21
          column: 3
          function: main
        value: i == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 21
          column: 3
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: pr-758.c
          line: 21
          column: 3
          function: main
        value: x == 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: pr-758.c
          line: 6
          column: 3
          function: main
        value: 0 <= x
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: pr-758.c
          line: 6
          column: 3
          function: main
        value: x <= 10
        format: c_expression
