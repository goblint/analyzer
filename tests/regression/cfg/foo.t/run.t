  $ cfgDot foo.c

  $ graph-easy --as=boxart main.dot
                                                        ┌───────────────────────────────┐
                                                        │ main()                        │
                                                        └───────────────────────────────┘
                                                          │
                                                          │ (body)
                                                          ▼
                                                        ┌───────────────────────────────┐
                                                        │ foo.c:2.3-2.19                │
                                                        │ (foo.c:2.7-2.12 (synthetic))  │
                                                        │ YAML loc: foo.c:2.3-2.19      │
                                                        │ server: false                 │
                                                        └───────────────────────────────┘
                                                          │
                                                          │ a = 1
                                                          ▼
                                                        ┌───────────────────────────────┐
                                                        │ foo.c:2.3-2.19 (synthetic)    │
                                                        │ (foo.c:2.14-2.19 (synthetic)) │
                                                        │ server: false                 │
                                                        └───────────────────────────────┘
                                                          │
                                                          │ b = 1
                                                          ▼
             ┌──────────────────────────┐               ┌───────────────────────────────┐
             │ foo.c:7.3-7.11           │               │ foo.c:3.3-6.3 (synthetic)     │
             │ (foo.c:7.10-7.11)        │               │ (foo.c:3.10-3.20 (synthetic)) │
             │ YAML loc: foo.c:7.3-7.11 │  Neg(a > 0)   │ YAML loop: foo.c:3.3-6.3      │
    ┌──────▶ │ server: true             │ ◀──────────── │ server: false                 │ ◀┐
    │        └──────────────────────────┘               └───────────────────────────────┘  │
    │          │                                          │                                │
    │          │ return 0                                 │ Pos(a > 0)                     │
    │          ▼                                          ▼                                │
    │        ┌──────────────────────────┐               ┌───────────────────────────────┐  │
    │ Neg(b) │                          │               │ foo.c:3.3-6.3 (synthetic)     │  │
    │        │ return of main()         │               │ (foo.c:3.10-3.20 (synthetic)) │  │
    │        │                          │  ┌─────────── │ server: false                 │  │
    │        └──────────────────────────┘  │            └───────────────────────────────┘  │
    │                                      │              │                                │
    └──────────────────────────────────────┘              │ Pos(b)                         │
                                                          ▼                                │ b = b - 1
                                                        ┌───────────────────────────────┐  │
                                                        │ foo.c:4.5-4.8                 │  │
                                                        │ (foo.c:4.5-4.8)               │  │
                                                        │ YAML loc: foo.c:4.5-4.8       │  │
                                                        │ server: true                  │  │
                                                        └───────────────────────────────┘  │
                                                          │                                │
                                                          │ a = a + 1                      │
                                                          ▼                                │
                                                        ┌───────────────────────────────┐  │
                                                        │ foo.c:5.5-5.8                 │  │
                                                        │ (foo.c:5.5-5.8)               │  │
                                                        │ YAML loc: foo.c:5.5-5.8       │  │
                                                        │ server: true                  │ ─┘
                                                        └───────────────────────────────┘

  $ goblint --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant", "loop_invariant"]' --set sem.int.signed_overflow assume_none foo.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow (foo.c:4.5-4.8)
  [Warning][Integer > Overflow][CWE-191] Signed integer underflow (foo.c:5.5-5.8)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6
  [Warning][Deadcode][CWE-571] condition 'a > 0' (possibly inserted by CIL) is always true (foo.c:3.10-3.20)
  [Info][Witness] witness generation summary:
    location invariants: 8
    loop invariants: 2
    flow-insensitive invariants: 0
    total generation entries: 10

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: foo.c
      line: 3
      column: 3
      function: main
    loop_invariant:
      string: b <= 1
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: foo.c
      line: 3
      column: 3
      function: main
    loop_invariant:
      string: 1 <= a
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 7
      column: 3
      function: main
    location_invariant:
      string: b == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 7
      column: 3
      function: main
    location_invariant:
      string: 1 <= a
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 5
      column: 5
      function: main
    location_invariant:
      string: b <= 1
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 5
      column: 5
      function: main
    location_invariant:
      string: b != 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 5
      column: 5
      function: main
    location_invariant:
      string: 2 <= a
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 4
      column: 5
      function: main
    location_invariant:
      string: b <= 1
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 4
      column: 5
      function: main
    location_invariant:
      string: b != 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: foo.c
      line: 4
      column: 5
      function: main
    location_invariant:
      string: 1 <= a
      type: assertion
      format: C
