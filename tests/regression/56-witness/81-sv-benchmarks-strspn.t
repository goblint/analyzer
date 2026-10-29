  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 81-sv-benchmarks-strspn.c
  [Warning][Deadcode] Function 'strspn' has dead code:
    on line 17 (81-sv-benchmarks-strspn.c:17-17)
    on line 19 (81-sv-benchmarks-strspn.c:19-19)
    on line 22 (81-sv-benchmarks-strspn.c:22-22)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 15
    dead: 3
    total lines: 18
  [Warning][Deadcode][CWE-570] condition '*s' (possibly inserted by CIL) is always false (81-sv-benchmarks-strspn.c:16:8-16:12)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "l == 0" in strspn.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 81-sv-benchmarks-strspn.c
          line: 16
          column: 3
          function: strspn
        value: l == 0UL
        format: c_expression

TODO: Do preprocessing in cfgDot.

  $ cpp 81-sv-benchmarks-strspn.c > 81-sv-benchmarks-strspn.i

  $ cfgDot 81-sv-benchmarks-strspn.i

  $ graph-easy --as=boxart strspn.dot
                                                                                                                          ┌─────────────────────────────────────────────────────┐
                                                                                                                          │ strspn()                                            │
                                                                                                                          └─────────────────────────────────────────────────────┘
                                                                                                                            │
                                                                                                                            │ (body)
                                                                                                                            ▼
                                                                                                                          ┌─────────────────────────────────────────────────────┐
                                                                                                                          │ 81-sv-benchmarks-strspn.c:13:3-13:15                │
                                                                                                                          │ (81-sv-benchmarks-strspn.c:13:10-13:15 (synthetic)) │
                                                                                                                          │ YAML loc: 81-sv-benchmarks-strspn.c:13:3-13:15      │
                                                                                                                          │ server: false                                       │
                                                                                                                          └─────────────────────────────────────────────────────┘
                                                                                                                            │
                                                                                                                            │ l = (size_t )0
                                                                                                                            ▼
                                                                                                                          ┌─────────────────────────────────────────────────────┐            ┌────────────────────────────────────────────────┐
                                                                                                                          │ 81-sv-benchmarks-strspn.c:16:3-23:3 (synthetic)     │            │                                                │
                                                                                                                          │ (81-sv-benchmarks-strspn.c:16:8-16:12 (synthetic))  │            │ 81-sv-benchmarks-strspn.c:25:3-25:11           │
                                                                                                                          │ [81-sv-benchmarks-strspn.c:16:3-23:3 (synthetic)    │            │ (81-sv-benchmarks-strspn.c:25:10-25:11)        │
                                                                                                                          │ (unknown)]                                          │            │ YAML loc: 81-sv-benchmarks-strspn.c:25:3-25:11 │
                                                                                                                          │ YAML loop: 81-sv-benchmarks-strspn.c:16:3-23:3      │  Neg(*s)   │ server: true                                   │
                                                                                                              ┌─────────▶ │ server: false                                       │ ─────────▶ │                                                │ ◀┐
                                                                                                              │           └─────────────────────────────────────────────────────┘            └────────────────────────────────────────────────┘  │
                                                                                                              │             │                                                                  │                                                 │
                                                                                                              │             │ Pos(*s)                                                          │ return l                                        │
                                                                                                              │             ▼                                                                  ▼                                                 │
                                                                                                              │           ┌─────────────────────────────────────────────────────┐            ┌────────────────────────────────────────────────┐  │
                                                                                                              │           │ 81-sv-benchmarks-strspn.c:17:5-17:42                │            │                                                │  │
                                                                                                              │           │ (81-sv-benchmarks-strspn.c:17:9-17:20 (synthetic))  │            │ return of strspn()                             │  │
                                                                                                              │ s = s + 1 │ YAML loc: 81-sv-benchmarks-strspn.c:17:5-17:42      │            │                                                │  │
                                                                                                              │           │ server: false                                       │            │                                                │  │
                                                                                                              │           └─────────────────────────────────────────────────────┘            └────────────────────────────────────────────────┘  │
                                                                                                              │             │                                                                                                                    │
                                                                                                              │             │ a = accept                                                                                                         │
                                                                                                              │             ▼                                                                                                                    │
                                                                                                              │           ┌─────────────────────────────────────────────────────┐                                                                │
                                                                                                              │           │ 81-sv-benchmarks-strspn.c:17:5-17:42 (synthetic)    │                                                                │
                                                                                                              │           │ (81-sv-benchmarks-strspn.c:17:20-17:36 (synthetic)) │                                                                │
                                                                                                              │           │ [81-sv-benchmarks-strspn.c:17:5-17:42 (synthetic)   │                                                                │
                                                                                                              │           │ (unknown)]                                          │                                                                │
                                                          a = a + 1                                           │           │ YAML loop: 81-sv-benchmarks-strspn.c:17:5-17:42     │                                                                │
    ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────┼─────────▶ │ server: false                                       │ ─┐                                                             │
    │                                                                                                         │           └─────────────────────────────────────────────────────┘  │                                                             │
    │                                                                                                         │             │                                                      │                                                             │
    │                                                                                                         │             │ Pos(*a)                                              │                                                             │
    │                                                                                                         │             ▼                                                      │                                                             │
  ┌─────────────────────────────────────────────────────┐                                                     │           ┌─────────────────────────────────────────────────────┐  │                                                             │
  │ 81-sv-benchmarks-strspn.c:17:5-17:42 (synthetic)    │                                                     │           │ 81-sv-benchmarks-strspn.c:17:5-17:42 (synthetic)    │  │                                                             │
  │ (81-sv-benchmarks-strspn.c:17:36-17:41 (synthetic)) │  Pos((int const   )*s != (int const   )*a)          │           │ (81-sv-benchmarks-strspn.c:17:20-17:36 (synthetic)) │  │ Neg(*a)                                                     │
  │ server: false                                       │ ◀───────────────────────────────────────────────────┼────────── │ server: false                                       │  │                                                             │
  └─────────────────────────────────────────────────────┘                                                     │           └─────────────────────────────────────────────────────┘  │                                                             │ Pos(! *a)
                                                                                                              │             │                                                      │                                                             │
                                                                                                              │             │ Neg((int const   )*s != (int const   )*a)            │                                                             │
                                                                                                              │             ▼                                                      │                                                             │
                                                                                                              │           ┌─────────────────────────────────────────────────────┐  │                                                             │
                                                                                                              │           │ 81-sv-benchmarks-strspn.c:19:5-22:9                 │  │                                                             │
                                                                                                              │           │ (81-sv-benchmarks-strspn.c:19:9-19:12)              │  │                                                             │
                                                                                                              │           │ YAML loc: 81-sv-benchmarks-strspn.c:19:5-22:9       │  │                                                             │
                                                                                                         ┌────┼────────── │ server: true                                        │ ◀┘                                                             │
                                                                                                         │    │           └─────────────────────────────────────────────────────┘                                                                │
                                                                                                         │    │             │                                                                                                                    │
                                                                                                         │    │             │ Neg(! *a)                                                                                                          │
                                                                                                         │    │             ▼                                                                                                                    │
                                                                                                         │    │           ┌─────────────────────────────────────────────────────┐                                                                │
                                                                                                         │    │           │ 81-sv-benchmarks-strspn.c:22:6-22:9                 │                                                                │
                                                                                                         │    │           │ (81-sv-benchmarks-strspn.c:22:6-22:9)               │                                                                │
                                                                                                         │    │           │ YAML loc: 81-sv-benchmarks-strspn.c:22:6-22:9       │                                                                │
                                                                                                         │    │           │ server: true                                        │                                                                │
                                                                                                         │    │           └─────────────────────────────────────────────────────┘                                                                │
                                                                                                         │    │             │                                                                                                                    │
                                                                                                         │    │             │ l = l + 1UL                                                                                                        │
                                                                                                         │    │             ▼                                                                                                                    │
                                                                                                         │    │           ┌─────────────────────────────────────────────────────┐                                                                │
                                                                                                         │    │           │ 81-sv-benchmarks-strspn.c:16:3-23:3 (synthetic)     │                                                                │
                                                                                                         │    │           │ (81-sv-benchmarks-strspn.c:16:12-16:17 (synthetic)) │                                                                │
                                                                                                         │    └────────── │ server: false                                       │                                                                │
                                                                                                         │                └─────────────────────────────────────────────────────┘                                                                │
                                                                                                         │                                                                                                                                       │
                                                                                                         └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
