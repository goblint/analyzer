  $ cfgDot loops.c

  $ graph-easy --as=boxart main.dot
  
                                                        ┌──────────────────────────────────────────────────────────────────────────────┐
                                                        │                                                                              │
                                                        │                                                                              │
    ┌───────────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────┐    │
    │                                                   │                                                                         │    │
    │                                                   │                                                                         │    │
    │    ┌──────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┐    │    │
    │    │                                              │                                                                    │    │    │
    │    │                                              │                                                                    │    │    │
    │    │    ┌─────────────────────────────────────────┼────────────────────────────────────────────────────┐               │    │    │
    │    │    │                                         │                                                    │               │    │    │
    │    │    │                                         │             ┌───────────────────────────────────┐  │               │    │    │
    │    │    │    ┌────────────────────────────────────┘             │ main()                            │  │               │    │    │
    │    │    │    │                                                  └───────────────────────────────────┘  │               │    │    │
    │    │    │    │                                                    │                                    │               │    │    │ i = i + 1
    │    │    │    │                                                    │ (body)                             │               │    │    │
    │    │    │    │                                                    ▼                                    │               │    │    │
    │    │    │    │                                                  ┌───────────────────────────────────┐  │               │    │    │
    │    │    │    │                                                  │ loops.c:7:3-7:8                   │  │               │    │    │
    │    │    │    │                                                  │ (loops.c:7:3-7:8)                 │  │               │    │    │
    │    │    │    │                                                  │ YAML loc: loops.c:7:3-7:8         │  │               │    │    │
    │    │    │    │                                                  │ server: true                      │  │               │    │    │
    │    │    │    │                                                  └───────────────────────────────────┘  │               │    │    │
    │    │    │    │                                                    │                                    │               │    │    │
    │    │    │    │                                                    │ i = 0                              │               │    │    │
    │    │    │    │                                                    ▼                                    │               │    │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐  │               │    │    │
    │    │    │  │                                   │                │ loops.c:8:3-10:3 (synthetic)      │  │               │    │    │
    │    │    │  │ loops.c:9:5-9:8                   │                │ (loops.c:8:10-8:16 (synthetic))   │  │               │    │    │
    │    │    │  │ (loops.c:9:5-9:8)                 │                │ [loops.c:8:3-10:3 (synthetic)     │  │               │    │    │
    │    │    │  │ YAML loc: loops.c:9:5-9:8         │                │ (unknown)]                        │  │               │    │    │
    │    │    │  │ server: true                      │  Pos(i < 10)   │ YAML loop: loops.c:8:3-10:3       │  │               │    │    │
    │    │    │  │                                   │ ◀───────────── │ server: false                     │ ◀┼───────────────┼────┼────┘
    │    │    │  └───────────────────────────────────┘                └───────────────────────────────────┘  │               │    │
    │    │    │                                                         │                                    │               │    │
    │    │    │                                                         │ Neg(i < 10)                        │               │    │
    │    │    │                                                         ▼                                    │               │    │
    │    │    │                                                       ┌───────────────────────────────────┐  │               │    │
    │    │    │                                                       │ loops.c:13:3-15:3                 │  │               │    │
    │    │    │                                                       │ (loops.c:13:7-13:26 (synthetic))  │  │               │    │
    │    │    │                                                       │ YAML loc: loops.c:13:3-15:3       │  │ i = i + 1     │    │
    │    │    │                                                       │ server: false                     │  │               │    │
    │    │    │                                                       └───────────────────────────────────┘  │               │    │
    │    │    │                                                         │                                    │               │    │
    │    │    │                                                         │ i = 0                              │               │    │
    │    │    │                                                         ▼                                    │               │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐  │               │    │
    │    │    │  │                                   │                │ loops.c:13:3-15:3 (synthetic)     │  │               │    │
    │    │    │  │ loops.c:14:5-14:23                │                │ (loops.c:13:7-13:26 (synthetic))  │  │               │    │
    │    │    │  │ (loops.c:14:5-14:23)              │                │ [loops.c:13:3-15:3 (synthetic)    │  │               │    │
    │    │    │  │ YAML loc: loops.c:14:5-14:23      │                │ (unknown)]                        │  │               │    │
    │    │    │  │ server: true                      │  Pos(i < 10)   │ YAML loop: loops.c:13:3-15:3      │  │               │    │
    │    │    │  │                                   │ ◀───────────── │ server: false                     │ ◀┘               │    │
    │    │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                  │    │
    │    │    │    │                                                    │                                                    │    │
    │    │    │    │ __goblint_check(1)                                 │ Neg(i < 10)                                        │    │
    │    │    │    ▼                                                    ▼                                                    │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                  │    │
    │    │    │  │ loops.c:13:3-15:3 (synthetic)     │                │ loops.c:18:3-20:3                 │                  │    │
    │    │    │  │ (loops.c:13:7-13:26 (synthetic))  │                │ (loops.c:18:7-18:26 (synthetic))  │                  │    │
    │    │    │  │ server: false                     │                │ YAML loc: loops.c:18:3-20:3       │                  │    │
    │    │    └─ │                                   │                │ server: false                     │                  │    │
    │    │       └───────────────────────────────────┘                └───────────────────────────────────┘                  │    │
    │    │                                                              │                                                    │    │
    │    │                                                              │ i = 0                                              │    │
    │    │                                                              ▼                                                    │    │
    │    │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                  │    │
    │    │       │                                   │                │ loops.c:18:3-20:3 (synthetic)     │                  │    │
    │    │       │ loops.c:18:3-20:3 (synthetic)     │                │ (loops.c:18:7-18:26 (synthetic))  │                  │    │
    │    │       │ (loops.c:18:7-18:26 (synthetic))  │                │ [loops.c:18:3-20:3 (synthetic)    │                  │    │
    │    │       │ server: false                     │                │ (unknown)]                        │                  │    │
    │    │       │                                   │  Pos(i < 10)   │ YAML loop: loops.c:18:3-20:3      │  i = i + 1       │    │
    │    └────── │                                   │ ◀───────────── │ server: false                     │ ◀────────────────┘    │
    │            └───────────────────────────────────┘                └───────────────────────────────────┘                       │
    │                                                                   │                                                         │
    │                                                                   │ Neg(i < 10)                                             │
    │                                                                   ▼                                                         │
    │                                                                 ┌───────────────────────────────────┐                       │
    │                                                                 │ loops.c:23:3-25:3                 │                       │
    │                                                                 │ (loops.c:23:7-23:22 (synthetic))  │                       │
    │                                                                 │ YAML loc: loops.c:23:3-25:3       │                       │
    │                                                                 │ server: false                     │                       │
    │                                                                 └───────────────────────────────────┘                       │
    │                                                                   │                                                         │
    │                                                                   │ i = 0                                                   │
    │                                                                   ▼                                                         │
    │            ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
    │            │                                   │                │ loops.c:23:3-25:3 (synthetic)     │                       │
    │            │ loops.c:24:5-24:8                 │                │ (loops.c:23:7-23:22 (synthetic))  │                       │
    │            │ (loops.c:24:5-24:8)               │                │ [loops.c:23:3-25:3 (synthetic)    │                       │
    │            │ YAML loc: loops.c:24:5-24:8       │                │ (unknown)]                        │                       │
    │            │ server: true                      │  Pos(i < 10)   │ YAML loop: loops.c:23:3-25:3      │  i = i + 1            │
    └─────────── │                                   │ ◀───────────── │ server: false                     │ ◀─────────────────────┘
                 └───────────────────────────────────┘                └───────────────────────────────────┘
                                                                        │
                                                                        │ Neg(i < 10)
                                                                        ▼
                                                                      ┌───────────────────────────────────┐
                                                                      │ loops.c:28:3-28:8                 │
                                                                      │ (loops.c:28:3-28:8)               │
                                                                      │ YAML loc: loops.c:28:3-28:8       │
                                                                      │ server: true                      │
                                                                      └───────────────────────────────────┘
                                                                        │
                                                                        │ i = 0
                                                                        ▼
                 ┌───────────────────────────────────┐                ┌───────────────────────────────────┐
                 │                                   │                │ loops.c:29:3-31:3 (synthetic)     │
                 │ loops.c:30:5-30:23                │                │ (loops.c:29:7-29:21 (synthetic))  │
                 │ (loops.c:30:5-30:23)              │                │ [loops.c:29:3-31:3 (synthetic)    │
                 │ YAML loc: loops.c:30:5-30:23      │                │ (unknown)]                        │
                 │ server: true                      │  Pos(i < 10)   │ YAML loop: loops.c:29:3-31:3      │  i = i + 1
                 │                                   │ ◀───────────── │ server: false                     │ ◀─────────────────────┐
                 └───────────────────────────────────┘                └───────────────────────────────────┘                       │
                   │                                                    │                                                         │
                   │ __goblint_check(1)                                 │ Neg(i < 10)                                             │
                   ▼                                                    ▼                                                         │
                 ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
                 │ loops.c:29:3-31:3 (synthetic)     │                │ loops.c:34:3-36:3                 │                       │
                 │ (loops.c:29:7-29:21 (synthetic))  │                │ (loops.c:34:12-34:17 (synthetic)) │                       │
                 │ server: false                     │                │ YAML loc: loops.c:34:3-36:3       │                       │
         ┌────── │                                   │                │ server: false                     │                       │
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │
         │                                                              │                                                         │
         │                                                              │ j = 0                                                   │
         │                                                              ▼                                                         │
         │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
         │       │                                   │                │ loops.c:34:3-36:3 (synthetic)     │                       │
         │       │ loops.c:35:5-35:23                │                │ (loops.c:34:7-34:30 (synthetic))  │                       │
         │       │ (loops.c:35:5-35:23)              │                │ [loops.c:34:3-36:3 (synthetic)    │                       │
         │       │ YAML loc: loops.c:35:5-35:23      │                │ (unknown)]                        │                       │
         │       │ server: true                      │  Pos(j < 10)   │ YAML loop: loops.c:34:3-36:3      │  j = j + 1            │
         │       │                                   │ ◀───────────── │ server: false                     │ ◀─────────────────────┼────┐
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │         │ __goblint_check(1)                                 │ Neg(j < 10)                                             │    │
         │         ▼                                                    ▼                                                         │    │
         │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │
         │       │ loops.c:34:3-36:3 (synthetic)     │                │ loops.c:39:3-41:3                 │                       │    │
         │       │ (loops.c:34:12-34:17 (synthetic)) │                │ (loops.c:39:12-39:23 (synthetic)) │                       │    │
         │       │ server: false                     │                │ YAML loc: loops.c:39:3-41:3       │                       │    │
         │       │                                   │                │ server: false                     │                       │    │
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │         │                                                    │ i = 0                                                   │    │
         │         │                                                    ▼                                                         │    │
         │         │                                                  ┌───────────────────────────────────┐                       │    │
         │         │                                                  │ loops.c:39:3-41:3 (synthetic)     │                       │    │
         │         │                                                  │ (loops.c:39:12-39:23 (synthetic)) │                       │    │
         │         │                                                  │ server: false                     │                       │    │
         │         │                                                  └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │    ┌────┘                                                    │ k = i                                                   │    │
         │    │                                                         ▼                                                         │    │
         │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │
         │    │  │                                   │                │ loops.c:39:3-41:3 (synthetic)     │                       │    │
         │    │  │ loops.c:40:5-40:23                │                │ (loops.c:39:7-39:36 (synthetic))  │                       │    │
         │    │  │ (loops.c:40:5-40:23)              │                │ [loops.c:39:3-41:3 (synthetic)    │                       │    │
         │    │  │ YAML loc: loops.c:40:5-40:23      │                │ (unknown)]                        │                       │    │
         │    │  │ server: true                      │  Pos(i < 10)   │ YAML loop: loops.c:39:3-41:3      │  i = i + 1            │    │
         │    │  │                                   │ ◀───────────── │ server: false                     │ ◀─────────────────────┼────┼─────────────┐
         │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │ __goblint_check(1)                                 │ Neg(i < 10)                                             │    │             │
         │    │    ▼                                                    ▼                                                         │    │             │
         │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │             │
         │    │  │ loops.c:39:3-41:3 (synthetic)     │                │ loops.c:44:3-44:8                 │                       │    │             │
         │    │  │ (loops.c:39:12-39:23 (synthetic)) │                │ (loops.c:44:3-44:8)               │                       │    │             │
         │    │  │ server: false                     │                │ YAML loc: loops.c:44:3-44:8       │                       │    │             │
         │    │  │                                   │                │ server: true                      │                       │    │             │
         │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │                                                    │ i = 0                                                   │    │             │
         │    │    │                                                    ▼                                                         │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐                       │    │             │
         │    │    │                                                  │ loops.c:46:5-46:8                 │                       │    │             │
         │    │    │                                                  │ (loops.c:46:5-46:8)               │                       │    │             │
         │    │    │                                                  │ YAML loc: loops.c:46:5-46:8       │                       │    │             │
         │    │    │                                                  │ server: true                      │ ◀┐                    │    │             │
         │    │    │                                                  └───────────────────────────────────┘  │                    │    │             │
         │    │    │                                                    │                                    │                    │    │             │
         │    │    │                                                    │ i = i + 1                          │ Pos(i < 10)        │    │             │
         │    │    │                                                    ▼                                    │                    │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐  │                    │    │             │
         │    │    │                                                  │ loops.c:45:3-47:19 (synthetic)    │  │                    │    │             │
         │    │    │                                                  │ (loops.c:47:12-47:19 (synthetic)) │  │                    │    │             │
         │    │    │                                                  │ [loops.c:45:3-47:19 (synthetic)   │  │                    │    │             │
         │    │    │                                                  │ (unknown)]                        │  │                    │    │             │
         │    │    │                                                  │ YAML loop: loops.c:45:3-47:19     │  │                    │    │             │
         │    │    │                                                  │ server: false                     │ ─┘                    │    │             │
         │    │    │                                                  └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │                                                    │ Neg(i < 10)                                             │    │             │
         │    │    │                                                    ▼                                                         │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐                       │    │             │
         │    │    │                                                  │ loops.c:49:3-49:11                │                       │    │             │
         │    │    │                                                  │ (loops.c:49:10-49:11)             │                       │    │             │
         │    │    │                                                  │ YAML loc: loops.c:49:3-49:11      │                       │    │             │
         │    │    │                                                  │ server: true                      │                       │    │             │
         │    │    │                                                  └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    └────┼────────────────────────────────────┐               │ return 0                                                │    │             │
         │         │                                    │               ▼                                                         │    │             │
         │         │                                    │             ┌───────────────────────────────────┐                       │    │             │
         │         │                                    │             │ return of main()                  │                       │    │             │
         │         │                                    │             └───────────────────────────────────┘                       │    │             │
         │         │                                    │                                                                         │    │             │
         └─────────┼────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────┘    │             │
                   │                                    │                                                                              │             │
                   │                                    │                                                                              │             │
                   │                                    └──────────────────────────────────────────────────────────────────────────────┘             │
                   │                                                                                                                                 │
                   │                                                                                                                                 │
                   └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant", "loop_invariant"]' loops.c
  [Success][Assert] Assertion "1" will succeed (loops.c:14:5-14:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:30:5-30:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:35:5-35:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:40:5-40:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] witness generation summary:
    location invariants: 32
    loop invariants: 21
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 9
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 9
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 13
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 14
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 14
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 18
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 23
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 24
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 24
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 28
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 30
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 30
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 34
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 35
          column: 5
          function: main
        value: 0 <= j
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 35
          column: 5
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 35
          column: 5
          function: main
        value: j <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 40
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 40
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 40
          column: 5
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 40
          column: 5
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 44
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 44
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 44
          column: 3
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 46
          column: 5
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 46
          column: 5
          function: main
        value: i <= 9
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 46
          column: 5
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 46
          column: 5
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 49
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 49
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: loops.c
          line: 49
          column: 3
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 8
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 8
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 13
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 13
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 18
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 18
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 23
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 23
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 29
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 29
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 34
          column: 3
          function: main
        value: 0 <= j
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 34
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 34
          column: 3
          function: main
        value: j <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: 0 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 39
          column: 3
          function: main
        value: k == 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 45
          column: 3
          function: main
        value: 1 <= i
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 45
          column: 3
          function: main
        value: i <= 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 45
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: loops.c
          line: 45
          column: 3
          function: main
        value: k == 0
        format: c_expression
