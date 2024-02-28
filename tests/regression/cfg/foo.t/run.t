  $ cfgDot foo.c

  $ graph-easy --as=boxart main.dot
                                                              ┌────────────────────────────────┐
                                                              │ main()                         │
                                                              └────────────────────────────────┘
                                                                │
                                                                │ (body)
                                                                ▼
                                                              ┌────────────────────────────────┐
                                                              │ foo.c:2:3-2:19                 │
                                                              │ (foo.c:2:7-2:12 (synthetic))   │
                                                              │ YAML loc: foo.c:2:3-2:19       │
                                                              │ YAMLval loc: true, loop: false │
                                                              │ GraphML: true; server: false   │
                                                              └────────────────────────────────┘
                                                                │
                                                                │ a = 1
                                                                ▼
                                                              ┌────────────────────────────────┐
                                                              │ foo.c:2:3-2:19 (synthetic)     │
                                                              │ (foo.c:2:14-2:19 (synthetic))  │
                                                              │ YAMLval loc: true, loop: false │
                                                              │ GraphML: true; server: false   │
                                                              └────────────────────────────────┘
                                                                │
                                                                │ b = 1
                                                                ▼
             ┌────────────────────────────────┐               ┌────────────────────────────────┐
             │ foo.c:7:3-7:11                 │               │ foo.c:3:3-6:3 (synthetic)      │
             │ (foo.c:7:10-7:11)              │               │ (foo.c:3:10-3:20 (synthetic))  │
             │ YAML loc: foo.c:7:3-7:11       │               │ YAML loop: foo.c:3:3-6:3       │
             │ YAMLval loc: true, loop: false │               │ YAMLval loc: true, loop: true  │
             │ GraphML: true; server: true    │  Neg(a > 0)   │ GraphML: true; server: false   │
    ┌──────▶ │                                │ ◀──────────── │ loop: foo.c:3:3-6:3            │ ◀┐
    │        └────────────────────────────────┘               └────────────────────────────────┘  │
    │          │                                                │                                 │
    │          │ return 0                                       │ Pos(a > 0)                      │
    │          ▼                                                ▼                                 │
    │        ┌────────────────────────────────┐               ┌────────────────────────────────┐  │
    │ Neg(b) │                                │               │ foo.c:3:3-6:3 (synthetic)      │  │
    │        │ return of main()               │               │ (foo.c:3:10-3:20 (synthetic))  │  │
    │        │                                │               │ YAMLval loc: true, loop: false │  │
    │        │                                │  ┌─────────── │ GraphML: true; server: false   │  │
    │        └────────────────────────────────┘  │            └────────────────────────────────┘  │
    │                                            │              │                                 │
    └────────────────────────────────────────────┘              │ Pos(b)                          │
                                                                ▼                                 │ b = b - 1
                                                              ┌────────────────────────────────┐  │
                                                              │ foo.c:4:5-4:8                  │  │
                                                              │ (foo.c:4:5-4:8)                │  │
                                                              │ YAML loc: foo.c:4:5-4:8        │  │
                                                              │ YAMLval loc: true, loop: false │  │
                                                              │ GraphML: true; server: true    │  │
                                                              └────────────────────────────────┘  │
                                                                │                                 │
                                                                │ a = a + 1                       │
                                                                ▼                                 │
                                                              ┌────────────────────────────────┐  │
                                                              │ foo.c:5:5-5:8                  │  │
                                                              │ (foo.c:5:5-5:8)                │  │
                                                              │ YAML loc: foo.c:5:5-5:8        │  │
                                                              │ YAMLval loc: true, loop: false │  │
                                                              │ GraphML: true; server: true    │ ─┘
                                                              └────────────────────────────────┘
