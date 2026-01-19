  $ cfgDot issue-1356.c

  $ graph-easy --as=boxart minus.dot
  
    ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                                                  │
    │                                                                     ┌─────────────────────────────────────────┐  │
    │                                                                     │ minus()                                 │  │
    │                                                                     └─────────────────────────────────────────┘  │
    │                                                                       │                                          │
    │ Pos((long )a >= (long )b - 2147483648)                                │ (body)                                   │
    ▼                                                                       ▼                                          │
  ┌─────────────────────────────────────────┐                             ┌─────────────────────────────────────────┐  │
  │ issue-1356.c:9:3-9:53 (synthetic)       │                             │ issue-1356.c:9:3-9:53                   │  │
  │ (issue-1356.c:9:3-9:53 (synthetic))     │                             │ (issue-1356.c:9:3-9:53)                 │  │
  │ server: false                           │  Pos(b <= 0)                │ YAML loc: issue-1356.c:9:3-9:53         │  │
  │                                         │ ◀────────────────────────── │ server: true                            │  │
  └─────────────────────────────────────────┘                             └─────────────────────────────────────────┘  │
    │                                                                       │                                          │
    │                                                                       │ Neg(b <= 0)                              │
    │                                                                       ▼                                          │
    │                                                                     ┌─────────────────────────────────────────┐  │
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │  │
    │                                                                     │ (issue-1356.c:9:3-9:53 (synthetic))     │  │
    │                                                                     │ server: false                           │ ─┘
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ Neg((long )a >= (long )b - 2147483648)
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │
    │                                                                     │ (issue-1356.c:9:3-9:53 (synthetic))     │
    │                                                                     │ server: false                           │
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ tmp = 0
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │
    │                                         tmp = 1                     │ (issue-1356.c:9:3-9:53 (synthetic))     │
    └───────────────────────────────────────────────────────────────────▶ │ server: false                           │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ assume_abort_if_not(tmp)
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ issue-1356.c:10:3-10:53                 │
                                                                          │ (issue-1356.c:10:3-10:53)               │
                                                                          │ YAML loc: issue-1356.c:10:3-10:53       │
                                                                          │ server: true                            │ ─┐
                                                                          └─────────────────────────────────────────┘  │
                                                                            │                                          │
                                                                            │ Neg(b >= 0)                              │
                                                                            ▼                                          │
  ┌─────────────────────────────────────────┐                             ┌─────────────────────────────────────────┐  │
  │ issue-1356.c:10:3-10:53 (synthetic)     │                             │ issue-1356.c:10:3-10:53 (synthetic)     │  │
  │ (issue-1356.c:10:3-10:53 (synthetic))   │  Neg(a <= b + 2147483647)   │ (issue-1356.c:10:3-10:53 (synthetic))   │  │ Pos(b >= 0)
  │ server: false                           │ ◀────────────────────────── │ server: false                           │  │
  └─────────────────────────────────────────┘                             └─────────────────────────────────────────┘  │
    │                                                                       │                                          │
    │                                                                       │ Pos(a <= b + 2147483647)                 │
    │                                                                       ▼                                          │
    │                                                                     ┌─────────────────────────────────────────┐  │
    │                                                                     │ issue-1356.c:10:3-10:53 (synthetic)     │  │
    │                                                                     │ (issue-1356.c:10:3-10:53 (synthetic))   │  │
    │                                                                     │ server: false                           │ ◀┘
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ tmp___0 = 1
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:10:3-10:53 (synthetic)     │
    │                                         tmp___0 = 0                 │ (issue-1356.c:10:3-10:53 (synthetic))   │
    └───────────────────────────────────────────────────────────────────▶ │ server: false                           │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ assume_abort_if_not(tmp___0)
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ issue-1356.c:11:3-11:15                 │
                                                                          │ (issue-1356.c:11:10-11:15)              │
                                                                          │ YAML loc: issue-1356.c:11:3-11:15       │
                                                                          │ server: true                            │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ return a - b
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ return of minus()                       │
                                                                          └─────────────────────────────────────────┘




  $ goblint --enable ana.sv-comp.functions --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' issue-1356.c
  [Warning][Integer > Overflow][CWE-190][CWE-191] Signed integer overflow and underflow in binary - (issue-1356.c:11:10-11:15)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 13
    dead: 0
    total lines: 13
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content: []
