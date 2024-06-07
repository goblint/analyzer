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
    │                                                │ GraphML: true; server: false       │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │ x = x + 1
    │                                                  │ x = 42                              │
    │                                                  ▼                                     │
    │                                                ┌────────────────────────────────────┐  │
    │                                                │ pr-758.c:6:3-8:3                   │  │
    │                                                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
    │                                                │ YAML loc: pr-758.c:6:3-8:3         │  │
    │                                                │ GraphML: true; server: false       │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │
    │                                                  │ x = 0                               │
    │                                                  ▼                                     │
  ┌─────────────────────────────────┐                ┌────────────────────────────────────┐  │
  │                                 │                │ pr-758.c:6:3-8:3 (synthetic)       │  │
  │ pr-758.c:6:3-8:3 (synthetic)    │                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
  │ (pr-758.c:6:7-6:26 (synthetic)) │                │ YAML loop: pr-758.c:6:3-8:3        │  │
  │ GraphML: true; server: false    │  Pos(x < 10)   │ GraphML: true; server: false       │  │
  │                                 │ ◀───────────── │ loop: pr-758.c:6:3-8:3             │ ◀┘
  └─────────────────────────────────┘                └────────────────────────────────────┘
                                                       │
                                                       │ Neg(x < 10)
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12                │
                                                     │ (pr-758.c:12:3-12:12)              │
                                                     │ YAML loc: pr-758.c:12:3-12:12      │
                                                     │ GraphML: true; server: true        │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ k = 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12 (synthetic)    │
                                                     │ (pr-758.c:12:3-12:12 (synthetic))  │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ i = k
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25                │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ YAML loc: pr-758.c:20:3-20:25      │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.kaal = 2
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25 (synthetic)    │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.hind = 3
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:21:3-21:11                │
                                                     │ (pr-758.c:21:10-21:11)             │
                                                     │ YAML loc: pr-758.c:21:3-21:11      │
                                                     │ GraphML: true; server: true        │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ return 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ return of main()                   │
                                                     └────────────────────────────────────┘



  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["loop_invariant", "location_invariant"]' pr-758.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Info][Witness] witness generation summary:
    total generation entries: 12

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 6
      column: 3
      function: main
    loop_invariant:
      string: x <= 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 6
      column: 3
      function: main
    loop_invariant:
      string: 0 <= x
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 3
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 3
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 3
      function: main
    location_invariant:
      string: i == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 3
      function: main
    location_invariant:
      string: a.kaal == 2
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 21
      column: 3
      function: main
    location_invariant:
      string: a.hind == 3
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: main
    location_invariant:
      string: k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 20
      column: 3
      function: main
    location_invariant:
      string: i == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: x == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: pr-758.c
      file_hash: $FILE_HASH
      line: 6
      column: 3
      function: main
    location_invariant:
      string: x == 42
      type: assertion
      format: C
