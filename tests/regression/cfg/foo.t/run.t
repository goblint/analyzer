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
                                                              │ YAML loc: true, loop: false    │
                                                              │ YAMLval loc: true, loop: false │
                                                              │ GraphML: true; server: false   │
                                                              └────────────────────────────────┘
                                                                │
                                                                │ a = 1
                                                                ▼
                                                              ┌────────────────────────────────┐
                                                              │ foo.c:2:3-2:19 (synthetic)     │
                                                              │ (foo.c:2:14-2:19 (synthetic))  │
                                                              │ YAML loc: false, loop: false   │
                                                              │ YAMLval loc: true, loop: false │
                                                              │ GraphML: true; server: false   │
                                                              └────────────────────────────────┘
                                                                │
                                                                │ b = 1
                                                                ▼
             ┌────────────────────────────────┐               ┌────────────────────────────────┐
             │ foo.c:7:3-7:11                 │               │ foo.c:3:3-6:3 (synthetic)      │
             │ (foo.c:7:10-7:11)              │               │ (foo.c:3:10-3:20 (synthetic))  │
             │ YAML loc: true, loop: false    │               │ YAML loc: false, loop: true    │
             │ YAMLval loc: true, loop: false │  Neg(a > 0)   │ YAMLval loc: true, loop: true  │
    ┌──────▶ │ GraphML: true; server: true    │ ◀──────────── │ GraphML: true; server: false   │ ◀┐
    │        └────────────────────────────────┘               └────────────────────────────────┘  │
    │          │                                                │                                 │
    │          │ return 0                                       │ Pos(a > 0)                      │
    │          ▼                                                ▼                                 │
    │        ┌────────────────────────────────┐               ┌────────────────────────────────┐  │
    │        │                                │               │ foo.c:3:3-6:3 (synthetic)      │  │
    │ Neg(b) │                                │               │ (foo.c:3:10-3:20 (synthetic))  │  │
    │        │ return of main()               │               │ YAML loc: false, loop: false   │  │
    │        │                                │               │ YAMLval loc: true, loop: false │  │
    │        │                                │  ┌─────────── │ GraphML: true; server: false   │  │
    │        └────────────────────────────────┘  │            └────────────────────────────────┘  │
    │                                            │              │                                 │
    └────────────────────────────────────────────┘              │ Pos(b)                          │ b = b - 1
                                                                ▼                                 │
                                                              ┌────────────────────────────────┐  │
                                                              │ foo.c:4:5-4:8                  │  │
                                                              │ (foo.c:4:5-4:8)                │  │
                                                              │ YAML loc: true, loop: false    │  │
                                                              │ YAMLval loc: true, loop: false │  │
                                                              │ GraphML: true; server: true    │  │
                                                              └────────────────────────────────┘  │
                                                                │                                 │
                                                                │ a = a + 1                       │
                                                                ▼                                 │
                                                              ┌────────────────────────────────┐  │
                                                              │ foo.c:5:5-5:8                  │  │
                                                              │ (foo.c:5:5-5:8)                │  │
                                                              │ YAML loc: true, loop: false    │  │
                                                              │ YAMLval loc: true, loop: false │  │
                                                              │ GraphML: true; server: true    │ ─┘
                                                              └────────────────────────────────┘
