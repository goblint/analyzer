  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 80-sv-benchmarks-strlen.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 11
    dead: 0
    total lines: 11
  [Warning][Deadcode][CWE-570] condition '! s' is always false (80-sv-benchmarks-strlen.c:13:7-13:9)
  [Warning][Deadcode][CWE-570] condition '*s' (possibly inserted by CIL) is always false (80-sv-benchmarks-strlen.c:14:11-14:15)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "i == 0" in strlen.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 80-sv-benchmarks-strlen.c
          line: 14
          column: 3
          function: strlen
        value: i == 0UL
        format: c_expression

TODO: Do preprocessing in cfgDot.

  $ cpp 80-sv-benchmarks-strlen.c > 80-sv-benchmarks-strlen.i

  $ cfgDot 80-sv-benchmarks-strlen.i

  $ graph-easy --as=boxart strlen.dot
                                                                  ┌─────────────────────────────────────────────────────┐
                                                                  │ strlen()                                            │
                                                                  └─────────────────────────────────────────────────────┘
                                                                    │
                                                                    │ (body)
                                                                    ▼
  ┌─────────────────────────────────────────────────┐             ┌─────────────────────────────────────────────────────┐
  │ 80-sv-benchmarks-strlen.c:13:11-13:19           │             │ 80-sv-benchmarks-strlen.c:13:3-13:19                │
  │ (80-sv-benchmarks-strlen.c:13:18-13:19)         │             │ (80-sv-benchmarks-strlen.c:13:7-13:9)               │
  │ YAML loc: 80-sv-benchmarks-strlen.c:13:11-13:19 │  Pos(! s)   │ YAML loc: 80-sv-benchmarks-strlen.c:13:3-13:19      │
  │ server: true                                    │ ◀────────── │ server: true                                        │
  └─────────────────────────────────────────────────┘             └─────────────────────────────────────────────────────┘
    │                                                               │
    │ return (size_t )0                                             │ Neg(! s)
    ▼                                                               ▼
  ┌─────────────────────────────────────────────────┐             ┌─────────────────────────────────────────────────────┐
  │                                                 │             │ 80-sv-benchmarks-strlen.c:14:3-14:25                │
  │ return of strlen()                              │             │ (80-sv-benchmarks-strlen.c:14:7-14:11 (synthetic))  │
  │                                                 │             │ YAML loc: 80-sv-benchmarks-strlen.c:14:3-14:25      │
  │                                                 │             │ server: false                                       │
  └─────────────────────────────────────────────────┘             └─────────────────────────────────────────────────────┘
    ▲                                                               │
    │ return i                                                      │ i = (size_t )0
    │                                                               ▼
  ┌─────────────────────────────────────────────────┐             ┌─────────────────────────────────────────────────────┐
  │                                                 │             │ 80-sv-benchmarks-strlen.c:14:3-14:25 (synthetic)    │
  │ 80-sv-benchmarks-strlen.c:15:3-15:11            │             │ (80-sv-benchmarks-strlen.c:14:11-14:15 (synthetic)) │
  │ (80-sv-benchmarks-strlen.c:15:10-15:11)         │             │ [80-sv-benchmarks-strlen.c:14:3-14:25 (synthetic)   │
  │ YAML loc: 80-sv-benchmarks-strlen.c:15:3-15:11  │             │ (unknown)]                                          │
  │ server: true                                    │  Neg(*s)    │ YAML loop: 80-sv-benchmarks-strlen.c:14:3-14:25     │
  │                                                 │ ◀────────── │ server: false                                       │ ◀┐
  └─────────────────────────────────────────────────┘             └─────────────────────────────────────────────────────┘  │
                                                                    │                                                      │
                                                                    │ Pos(*s)                                              │
                                                                    ▼                                                      │
                                                                  ┌─────────────────────────────────────────────────────┐  │
                                                                  │ 80-sv-benchmarks-strlen.c:14:22-14:25               │  │
                                                                  │ (80-sv-benchmarks-strlen.c:14:22-14:25)             │  │
                                                                  │ YAML loc: 80-sv-benchmarks-strlen.c:14:22-14:25     │  │ s = s + 1
                                                                  │ server: true                                        │  │
                                                                  └─────────────────────────────────────────────────────┘  │
                                                                    │                                                      │
                                                                    │ i = i + 1UL                                          │
                                                                    ▼                                                      │
                                                                  ┌─────────────────────────────────────────────────────┐  │
                                                                  │ 80-sv-benchmarks-strlen.c:14:3-14:25 (synthetic)    │  │
                                                                  │ (80-sv-benchmarks-strlen.c:14:15-14:20 (synthetic)) │  │
                                                                  │ server: false                                       │ ─┘
                                                                  └─────────────────────────────────────────────────────┘
