  $ goblint --enable ana.sv-comp.functions --enable witness.yaml.enabled --set witness.yaml.invariant-types[*] loop_invariant 82-sv-benchmarks-wcsrchr.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (82-sv-benchmarks-wcsrchr.c:24:3-24:18)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (82-sv-benchmarks-wcsrchr.c:13:8-13:14)
  [Warning][Deadcode] Function 'wcsrchr' has dead code:
    on lines 14..15 (82-sv-benchmarks-wcsrchr.c:14-15)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 12
    dead: 2
    total lines: 14
  [Warning][Deadcode][CWE-570] condition '*wcs' (possibly inserted by CIL) is always false (82-sv-benchmarks-wcsrchr.c:13:8-13:14)
  [Info][Witness] witness generation summary:
    location invariants: 0
    loop invariants: 1
    flow-insensitive invariants: 0
    total generation entries: 1

TODO: Should not have loop invariant "last == 0" in wcsrchr.

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: loop_invariant
        location:
          file_name: 82-sv-benchmarks-wcsrchr.c
          line: 13
          column: 3
          function: wcsrchr
        value: last == 0
        format: c_expression

TODO: Do preprocessing in cfgDot.

  $ cpp 82-sv-benchmarks-wcsrchr.c > 82-sv-benchmarks-wcsrchr.i

  $ cfgDot 82-sv-benchmarks-wcsrchr.i

  $ graph-easy --as=boxart wcsrchr.dot
                    ┌──────────────────────────────────────────────────────┐
                    │ wcsrchr()                                            │
                    └──────────────────────────────────────────────────────┘
                      │
                      │ (body)
                      ▼
                    ┌──────────────────────────────────────────────────────┐
                    │ 82-sv-benchmarks-wcsrchr.c:12:3-12:14                │
                    │ (82-sv-benchmarks-wcsrchr.c:12:6-12:14 (synthetic))  │
                    │ YAML loc: 82-sv-benchmarks-wcsrchr.c:12:3-12:14      │
                    │ server: false                                        │
                    └──────────────────────────────────────────────────────┘
                      │
                      │ last = (int *)0
                      ▼
                    ┌──────────────────────────────────────────────────────┐                                  ┌─────────────────────────────────────────────────┐                ┌─────────────────────┐
                    │ 82-sv-benchmarks-wcsrchr.c:13:3-16:3 (synthetic)     │                                  │                                                 │                │                     │
                    │ (82-sv-benchmarks-wcsrchr.c:13:8-13:14 (synthetic))  │                                  │ 82-sv-benchmarks-wcsrchr.c:16:3-16:14           │                │                     │
                    │ [82-sv-benchmarks-wcsrchr.c:13:3-16:3 (synthetic)    │                                  │ (82-sv-benchmarks-wcsrchr.c:16:10-16:14)        │                │ return of wcsrchr() │
                    │ (unknown)]                                           │                                  │ YAML loc: 82-sv-benchmarks-wcsrchr.c:16:3-16:14 │                │                     │
                    │ YAML loop: 82-sv-benchmarks-wcsrchr.c:13:3-16:3      │  Neg(*wcs)                       │ server: true                                    │  return last   │                     │
    ┌─────────────▶ │ server: false                                        │ ───────────────────────────────▶ │                                                 │ ─────────────▶ │                     │
    │               └──────────────────────────────────────────────────────┘                                  └─────────────────────────────────────────────────┘                └─────────────────────┘
    │                 │
    │                 │ Pos(*wcs)
    │                 ▼
    │               ┌──────────────────────────────────────────────────────┐
    │               │ 82-sv-benchmarks-wcsrchr.c:14:5-15:21                │
    │               │ (82-sv-benchmarks-wcsrchr.c:14:9-14:19)              │
    │               │ YAML loc: 82-sv-benchmarks-wcsrchr.c:14:5-15:21      │
    │               │ server: true                                         │ ─┐
    │               └──────────────────────────────────────────────────────┘  │
    │                 │                                                       │
    │ wcs = wcs + 1   │ Pos(*wcs == (int const   )wc)                         │
    │                 ▼                                                       │
    │               ┌──────────────────────────────────────────────────────┐  │
    │               │ 82-sv-benchmarks-wcsrchr.c:15:7-15:21                │  │
    │               │ (82-sv-benchmarks-wcsrchr.c:15:7-15:21)              │  │
    │               │ YAML loc: 82-sv-benchmarks-wcsrchr.c:15:7-15:21      │  │ Neg(*wcs == (int const   )wc)
    │               │ server: true                                         │  │
    │               └──────────────────────────────────────────────────────┘  │
    │                 │                                                       │
    │                 │ last = (int *)wcs                                     │
    │                 ▼                                                       │
    │               ┌──────────────────────────────────────────────────────┐  │
    │               │ 82-sv-benchmarks-wcsrchr.c:13:3-16:3 (synthetic)     │  │
    │               │ (82-sv-benchmarks-wcsrchr.c:13:14-13:21 (synthetic)) │  │
    └────────────── │ server: false                                        │ ◀┘
                    └──────────────────────────────────────────────────────┘
