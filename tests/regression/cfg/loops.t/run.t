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
    │    │    │    │                                                  │ GraphML: true; server: true       │  │               │    │    │
    │    │    │    │                                                  └───────────────────────────────────┘  │               │    │    │
    │    │    │    │                                                    │                                    │               │    │    │
    │    │    │    │                                                    │ i = 0                              │               │    │    │
    │    │    │    │                                                    ▼                                    │               │    │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐  │               │    │    │
    │    │    │  │ loops.c:9:5-9:8                   │                │ loops.c:8:3-10:3 (synthetic)      │  │               │    │    │
    │    │    │  │ (loops.c:9:5-9:8)                 │                │ (loops.c:8:10-8:16 (synthetic))   │  │               │    │    │
    │    │    │  │ YAML loc: loops.c:9:5-9:8         │                │ YAML loop: loops.c:8:3-10:3       │  │               │    │    │
    │    │    │  │ GraphML: true; server: true       │  Pos(i < 10)   │ GraphML: true; server: false      │  │               │    │    │
    │    │    │  │                                   │ ◀───────────── │ loop: loops.c:8:3-10:3            │ ◀┼───────────────┼────┼────┘
    │    │    │  └───────────────────────────────────┘                └───────────────────────────────────┘  │               │    │
    │    │    │                                                         │                                    │               │    │
    │    │    │                                                         │ Neg(i < 10)                        │               │    │
    │    │    │                                                         ▼                                    │               │    │
    │    │    │                                                       ┌───────────────────────────────────┐  │               │    │
    │    │    │                                                       │ loops.c:13:3-15:3                 │  │               │    │
    │    │    │                                                       │ (loops.c:13:7-13:26 (synthetic))  │  │               │    │
    │    │    │                                                       │ YAML loc: loops.c:13:3-15:3       │  │ i = i + 1     │    │
    │    │    │                                                       │ GraphML: true; server: false      │  │               │    │
    │    │    │                                                       └───────────────────────────────────┘  │               │    │
    │    │    │                                                         │                                    │               │    │
    │    │    │                                                         │ i = 0                              │               │    │
    │    │    │                                                         ▼                                    │               │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐  │               │    │
    │    │    │  │ loops.c:14:5-14:23                │                │ loops.c:13:3-15:3 (synthetic)     │  │               │    │
    │    │    │  │ (loops.c:14:5-14:23)              │                │ (loops.c:13:7-13:26 (synthetic))  │  │               │    │
    │    │    │  │ YAML loc: loops.c:14:5-14:23      │                │ YAML loop: loops.c:13:3-15:3      │  │               │    │
    │    │    │  │ GraphML: true; server: true       │  Pos(i < 10)   │ GraphML: true; server: false      │  │               │    │
    │    │    │  │                                   │ ◀───────────── │ loop: loops.c:13:3-15:3           │ ◀┘               │    │
    │    │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                  │    │
    │    │    │    │                                                    │                                                    │    │
    │    │    │    │ __goblint_check(1)                                 │ Neg(i < 10)                                        │    │
    │    │    │    ▼                                                    ▼                                                    │    │
    │    │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                  │    │
    │    │    │  │ loops.c:13:3-15:3 (synthetic)     │                │ loops.c:18:3-20:3                 │                  │    │
    │    │    │  │ (loops.c:13:7-13:26 (synthetic))  │                │ (loops.c:18:7-18:26 (synthetic))  │                  │    │
    │    │    │  │ GraphML: true; server: false      │                │ YAML loc: loops.c:18:3-20:3       │                  │    │
    │    │    └─ │                                   │                │ GraphML: true; server: false      │                  │    │
    │    │       └───────────────────────────────────┘                └───────────────────────────────────┘                  │    │
    │    │                                                              │                                                    │    │
    │    │                                                              │ i = 0                                              │    │
    │    │                                                              ▼                                                    │    │
    │    │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                  │    │
    │    │       │                                   │                │ loops.c:18:3-20:3 (synthetic)     │                  │    │
    │    │       │ loops.c:18:3-20:3 (synthetic)     │                │ (loops.c:18:7-18:26 (synthetic))  │                  │    │
    │    │       │ (loops.c:18:7-18:26 (synthetic))  │                │ YAML loop: loops.c:18:3-20:3      │                  │    │
    │    │       │ GraphML: true; server: false      │  Pos(i < 10)   │ GraphML: true; server: false      │  i = i + 1       │    │
    │    └────── │                                   │ ◀───────────── │ loop: loops.c:18:3-20:3           │ ◀────────────────┘    │
    │            └───────────────────────────────────┘                └───────────────────────────────────┘                       │
    │                                                                   │                                                         │
    │                                                                   │ Neg(i < 10)                                             │
    │                                                                   ▼                                                         │
    │                                                                 ┌───────────────────────────────────┐                       │
    │                                                                 │ loops.c:23:3-25:3                 │                       │
    │                                                                 │ (loops.c:23:7-23:22 (synthetic))  │                       │
    │                                                                 │ YAML loc: loops.c:23:3-25:3       │                       │
    │                                                                 │ GraphML: true; server: false      │                       │
    │                                                                 └───────────────────────────────────┘                       │
    │                                                                   │                                                         │
    │                                                                   │ i = 0                                                   │
    │                                                                   ▼                                                         │
    │            ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
    │            │ loops.c:24:5-24:8                 │                │ loops.c:23:3-25:3 (synthetic)     │                       │
    │            │ (loops.c:24:5-24:8)               │                │ (loops.c:23:7-23:22 (synthetic))  │                       │
    │            │ YAML loc: loops.c:24:5-24:8       │                │ YAML loop: loops.c:23:3-25:3      │                       │
    │            │ GraphML: true; server: true       │  Pos(i < 10)   │ GraphML: true; server: false      │  i = i + 1            │
    └─────────── │                                   │ ◀───────────── │ loop: loops.c:23:3-25:3           │ ◀─────────────────────┘
                 └───────────────────────────────────┘                └───────────────────────────────────┘
                                                                        │
                                                                        │ Neg(i < 10)
                                                                        ▼
                                                                      ┌───────────────────────────────────┐
                                                                      │ loops.c:28:3-28:8                 │
                                                                      │ (loops.c:28:3-28:8)               │
                                                                      │ YAML loc: loops.c:28:3-28:8       │
                                                                      │ GraphML: true; server: true       │
                                                                      └───────────────────────────────────┘
                                                                        │
                                                                        │ i = 0
                                                                        ▼
                 ┌───────────────────────────────────┐                ┌───────────────────────────────────┐
                 │ loops.c:30:5-30:23                │                │ loops.c:29:3-31:3 (synthetic)     │
                 │ (loops.c:30:5-30:23)              │                │ (loops.c:29:7-29:21 (synthetic))  │
                 │ YAML loc: loops.c:30:5-30:23      │                │ YAML loop: loops.c:29:3-31:3      │
                 │ GraphML: true; server: true       │  Pos(i < 10)   │ GraphML: true; server: false      │  i = i + 1
                 │                                   │ ◀───────────── │ loop: loops.c:29:3-31:3           │ ◀─────────────────────┐
                 └───────────────────────────────────┘                └───────────────────────────────────┘                       │
                   │                                                    │                                                         │
                   │ __goblint_check(1)                                 │ Neg(i < 10)                                             │
                   ▼                                                    ▼                                                         │
                 ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
                 │ loops.c:29:3-31:3 (synthetic)     │                │ loops.c:34:8-34:17                │                       │
                 │ (loops.c:29:7-29:21 (synthetic))  │                │ (loops.c:34:12-34:17 (synthetic)) │                       │
                 │ GraphML: true; server: false      │                │ YAML loc: loops.c:34:8-34:17      │                       │
         ┌────── │                                   │                │ GraphML: true; server: false      │                       │
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │
         │                                                              │                                                         │
         │                                                              │ j = 0                                                   │
         │                                                              ▼                                                         │
         │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │
         │       │ loops.c:35:5-35:23                │                │ loops.c:34:3-36:3 (synthetic)     │                       │
         │       │ (loops.c:35:5-35:23)              │                │ (loops.c:34:7-34:30 (synthetic))  │                       │
         │       │ YAML loc: loops.c:35:5-35:23      │                │ YAML loop: loops.c:34:3-36:3      │                       │
         │       │ GraphML: true; server: true       │  Pos(j < 10)   │ GraphML: true; server: false      │  j = j + 1            │
         │       │                                   │ ◀───────────── │ loop: loops.c:34:3-36:3           │ ◀─────────────────────┼────┐
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │         │ __goblint_check(1)                                 │ Neg(j < 10)                                             │    │
         │         ▼                                                    ▼                                                         │    │
         │       ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │
         │       │ loops.c:34:8-34:17 (synthetic)    │                │ loops.c:39:8-39:23                │                       │    │
         │       │ (loops.c:34:12-34:17 (synthetic)) │                │ (loops.c:39:12-39:23 (synthetic)) │                       │    │
         │       │ GraphML: true; server: false      │                │ YAML loc: loops.c:39:8-39:23      │                       │    │
         │       │                                   │                │ GraphML: true; server: false      │                       │    │
         │       └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │         │                                                    │ i = 0                                                   │    │
         │         │                                                    ▼                                                         │    │
         │         │                                                  ┌───────────────────────────────────┐                       │    │
         │         │                                                  │ loops.c:39:8-39:23 (synthetic)    │                       │    │
         │         │                                                  │ (loops.c:39:12-39:23 (synthetic)) │                       │    │
         │         │                                                  │ GraphML: true; server: false      │                       │    │
         │         │                                                  └───────────────────────────────────┘                       │    │
         │         │                                                    │                                                         │    │
         │    ┌────┘                                                    │ k = i                                                   │    │
         │    │                                                         ▼                                                         │    │
         │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │
         │    │  │ loops.c:40:5-40:23                │                │ loops.c:39:3-41:3 (synthetic)     │                       │    │
         │    │  │ (loops.c:40:5-40:23)              │                │ (loops.c:39:7-39:36 (synthetic))  │                       │    │
         │    │  │ YAML loc: loops.c:40:5-40:23      │                │ YAML loop: loops.c:39:3-41:3      │                       │    │
         │    │  │ GraphML: true; server: true       │  Pos(i < 10)   │ GraphML: true; server: false      │  i = i + 1            │    │
         │    │  │                                   │ ◀───────────── │ loop: loops.c:39:3-41:3           │ ◀─────────────────────┼────┼─────────────┐
         │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │ __goblint_check(1)                                 │ Neg(i < 10)                                             │    │             │
         │    │    ▼                                                    ▼                                                         │    │             │
         │    │  ┌───────────────────────────────────┐                ┌───────────────────────────────────┐                       │    │             │
         │    │  │ loops.c:39:8-39:23 (synthetic)    │                │ loops.c:44:3-44:8                 │                       │    │             │
         │    │  │ (loops.c:39:12-39:23 (synthetic)) │                │ (loops.c:44:3-44:8)               │                       │    │             │
         │    │  │ GraphML: true; server: false      │                │ YAML loc: loops.c:44:3-44:8       │                       │    │             │
         │    │  │                                   │                │ GraphML: true; server: true       │                       │    │             │
         │    │  └───────────────────────────────────┘                └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │                                                    │ i = 0                                                   │    │             │
         │    │    │                                                    ▼                                                         │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐                       │    │             │
         │    │    │                                                  │ loops.c:46:5-46:8                 │                       │    │             │
         │    │    │                                                  │ (loops.c:46:5-46:8)               │                       │    │             │
         │    │    │                                                  │ YAML loc: loops.c:46:5-46:8       │                       │    │             │
         │    │    │                                                  │ YAML loop: loops.c:45:3-47:19     │                       │    │             │
         │    │    │                                                  │ GraphML: true; server: true       │                       │    │             │
         │    │    │                                                  │ loop: loops.c:45:3-47:19          │ ◀┐                    │    │             │
         │    │    │                                                  └───────────────────────────────────┘  │                    │    │             │
         │    │    │                                                    │                                    │                    │    │             │
         │    │    │                                                    │ i = i + 1                          │ Pos(i < 10)        │    │             │
         │    │    │                                                    ▼                                    │                    │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐  │                    │    │             │
         │    │    │                                                  │ loops.c:45:3-47:19 (synthetic)    │  │                    │    │             │
         │    │    │                                                  │ (loops.c:47:12-47:19 (synthetic)) │  │                    │    │             │
         │    │    │                                                  │ GraphML: true; server: false      │ ─┘                    │    │             │
         │    │    │                                                  └───────────────────────────────────┘                       │    │             │
         │    │    │                                                    │                                                         │    │             │
         │    │    │                                                    │ Neg(i < 10)                                             │    │             │
         │    │    │                                                    ▼                                                         │    │             │
         │    │    │                                                  ┌───────────────────────────────────┐                       │    │             │
         │    │    │                                                  │ loops.c:49:3-49:11                │                       │    │             │
         │    │    │                                                  │ (loops.c:49:10-49:11)             │                       │    │             │
         │    │    │                                                  │ YAML loc: loops.c:49:3-49:11      │                       │    │             │
         │    │    │                                                  │ GraphML: true; server: true       │                       │    │             │
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

  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant", "loop_invariant"]' loops.c
  [Success][Assert] Assertion "1" will succeed (loops.c:14:5-14:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:30:5-30:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:35:5-35:23)
  [Success][Assert] Assertion "1" will succeed (loops.c:40:5-40:23)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 0
    total lines: 20
  [Info][Witness] witness generation summary:
    total generation entries: 53

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 45
      column: 2
      function: main
    loop_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 45
      column: 2
      function: main
    loop_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 45
      column: 2
      function: main
    loop_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 45
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    loop_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    loop_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 34
      column: 2
      function: main
    loop_invariant:
      string: j <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 34
      column: 2
      function: main
    loop_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 34
      column: 2
      function: main
    loop_invariant:
      string: 0 <= j
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 29
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 29
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 23
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 23
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 18
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 18
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 13
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 13
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 8
      column: 2
      function: main
    loop_invariant:
      string: i <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 8
      column: 2
      function: main
    loop_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 49
      column: 2
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 49
      column: 2
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 49
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 46
      column: 4
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 46
      column: 4
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 46
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 46
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 44
      column: 2
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 44
      column: 2
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 44
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 40
      column: 4
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 40
      column: 4
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 40
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 40
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 39
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 35
      column: 4
      function: main
    location_invariant:
      string: j <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 35
      column: 4
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 35
      column: 4
      function: main
    location_invariant:
      string: 0 <= j
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 34
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 30
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 30
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 28
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 24
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 24
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 23
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 18
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 14
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 14
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 13
      column: 2
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 9
      column: 4
      function: main
    location_invariant:
      string: i <= 9
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: loops.c
      file_hash: $FILE_HASH
      line: 9
      column: 4
      function: main
    location_invariant:
      string: 0 <= i
      type: assertion
      format: C
