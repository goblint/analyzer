  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 83-sv-benchmarks-cstrnlen.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 14
    dead: 0
    total lines: 14
  [Warning][Deadcode][CWE-571] condition 'maxlen != 0UL' (possibly inserted by CIL) is always true (83-sv-benchmarks-cstrnlen.c:43:15-43:43)
  [Warning][Deadcode][CWE-570] condition '(int const   )*cp != 0' (possibly inserted by CIL) is always false (83-sv-benchmarks-cstrnlen.c:43:15-43:43)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "1 <= maxlen" in cstrnlen.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-sv-benchmarks-cstrnlen.c
          line: 43
          column: 2
          function: cstrnlen
        value: 1UL <= maxlen
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 83-sv-benchmarks-cstrnlen.c
          line: 43
          column: 2
          function: cstrnlen
        value: maxlen <= 2147483647UL
        format: c_expression

TODO: Do preprocessing in cfgDot.

  $ cpp 83-sv-benchmarks-cstrnlen.c > 83-sv-benchmarks-cstrnlen.i

  $ cfgDot 83-sv-benchmarks-cstrnlen.i

  $ graph-easy --as=boxart cstrnlen.dot
                                                                                                             ┌───────────────────────────────────────────────────────┐
                                                                                                             │ cstrnlen()                                            │
                                                                                                             └───────────────────────────────────────────────────────┘
                                                                                                               │
                                                                                                               │ (body)
                                                                                                               ▼
                                                                                                             ┌───────────────────────────────────────────────────────┐
                                                                                                             │ 83-sv-benchmarks-cstrnlen.c:43:2-44:3                 │
                                                                                                             │ (83-sv-benchmarks-cstrnlen.c:43:6-43:15 (synthetic))  │
                                                                                                             │ YAML loc: 83-sv-benchmarks-cstrnlen.c:43:2-44:3       │
                                                                                                             │ server: false                                         │
                                                                                                             └───────────────────────────────────────────────────────┘
                                                                                                               │
                                                                                                               │ cp = str
                                                                                                               ▼
                                  ┌──────────────────────────────────────────────────┐                       ┌───────────────────────────────────────────────────────┐
                                  │                                                  │                       │ 83-sv-benchmarks-cstrnlen.c:43:2-44:3 (synthetic)     │
                                  │ 83-sv-benchmarks-cstrnlen.c:46:2-46:27           │                       │ (83-sv-benchmarks-cstrnlen.c:43:15-43:43 (synthetic)) │
                                  │ (83-sv-benchmarks-cstrnlen.c:46:9-46:27)         │                       │ [83-sv-benchmarks-cstrnlen.c:43:2-44:3 (synthetic)    │
                                  │ YAML loc: 83-sv-benchmarks-cstrnlen.c:46:2-46:27 │                       │ (unknown)]                                            │
                                  │ server: true                                     │  Neg(maxlen != 0UL)   │ YAML loop: 83-sv-benchmarks-cstrnlen.c:43:2-44:3      │
    ┌───────────────────────────▶ │                                                  │ ◀──────────────────── │ server: false                                         │ ◀┐
    │                             └──────────────────────────────────────────────────┘                       └───────────────────────────────────────────────────────┘  │
    │                               │                                                                          │                                                        │
    │                               │ return (size_t )(cp - str)                                               │ Pos(maxlen != 0UL)                                     │
    │                               ▼                                                                          ▼                                                        │
    │                             ┌──────────────────────────────────────────────────┐                       ┌───────────────────────────────────────────────────────┐  │
    │ Neg((int const   )*cp != 0) │                                                  │                       │ 83-sv-benchmarks-cstrnlen.c:43:2-44:3 (synthetic)     │  │
    │                             │ return of cstrnlen()                             │                       │ (83-sv-benchmarks-cstrnlen.c:43:15-43:43 (synthetic)) │  │
    │                             │                                                  │  ┌─────────────────── │ server: false                                         │  │
    │                             └──────────────────────────────────────────────────┘  │                    └───────────────────────────────────────────────────────┘  │
    │                                                                                   │                      │                                                        │
    └───────────────────────────────────────────────────────────────────────────────────┘                      │ Pos((int const   )*cp != 0)                            │ maxlen = maxlen - 1UL
                                                                                                               ▼                                                        │
                                                                                                             ┌───────────────────────────────────────────────────────┐  │
                                                                                                             │ 83-sv-benchmarks-cstrnlen.c:43:2-44:3 (synthetic)     │  │
                                                                                                             │ (83-sv-benchmarks-cstrnlen.c:43:43-43:59 (synthetic)) │  │
                                                                                                             │ server: false                                         │  │
                                                                                                             └───────────────────────────────────────────────────────┘  │
                                                                                                               │                                                        │
                                                                                                               │ cp = cp + 1                                            │
                                                                                                               ▼                                                        │
                                                                                                             ┌───────────────────────────────────────────────────────┐  │
                                                                                                             │ 83-sv-benchmarks-cstrnlen.c:43:2-44:3                 │  │
                                                                                                             │ (83-sv-benchmarks-cstrnlen.c:43:43-43:59 (synthetic)) │  │
                                                                                                             │ YAML loc: 83-sv-benchmarks-cstrnlen.c:43:2-44:3       │  │
                                                                                                             │ server: false                                         │ ─┘
                                                                                                             └───────────────────────────────────────────────────────┘
