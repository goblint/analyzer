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
    │                                                │ YAML loc: false, loop: false       │  │
    │                                                │ YAMLval loc: true, loop: false     │  │
    │                                                │ GraphML: true; server: false       │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │ x = x + 1
    │                                                  │ x = 42                              │
    │                                                  ▼                                     │
    │                                                ┌────────────────────────────────────┐  │
    │                                                │ pr-758.c:6:3-8:3 (synthetic)       │  │
    │                                                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
    │                                                │ YAML loc: false, loop: false       │  │
    │                                                │ YAMLval loc: true, loop: false     │  │
    │                                                │ GraphML: true; server: false       │  │
    │                                                └────────────────────────────────────┘  │
    │                                                  │                                     │
    │                                                  │ x = 0                               │
    │                                                  ▼                                     │
  ┌─────────────────────────────────┐                ┌────────────────────────────────────┐  │
  │ pr-758.c:6:3-8:3 (synthetic)    │                │ pr-758.c:6:3-8:3 (synthetic)       │  │
  │ (pr-758.c:6:7-6:26 (synthetic)) │                │ (pr-758.c:6:7-6:26 (synthetic))    │  │
  │ YAML loc: false, loop: false    │                │ YAML loc: false, loop: true        │  │
  │ YAMLval loc: true, loop: false  │  Pos(x < 10)   │ YAMLval loc: true, loop: true      │  │
  │ GraphML: true; server: false    │ ◀───────────── │ GraphML: true; server: false       │ ◀┘
  └─────────────────────────────────┘                └────────────────────────────────────┘
                                                       │
                                                       │ Neg(x < 10)
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12                │
                                                     │ (pr-758.c:12:3-12:12)              │
                                                     │ YAML loc: true, loop: false        │
                                                     │ YAMLval loc: true, loop: false     │
                                                     │ GraphML: true; server: true        │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ k = 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:12:3-12:12 (synthetic)    │
                                                     │ (pr-758.c:12:3-12:12 (synthetic))  │
                                                     │ YAML loc: false, loop: false       │
                                                     │ YAMLval loc: true, loop: false     │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ i = k
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25                │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ YAML loc: false, loop: false       │
                                                     │ YAMLval loc: true, loop: false     │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.kaal = 2
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:20:3-20:25 (synthetic)    │
                                                     │ (pr-758.c:20:15-20:24 (synthetic)) │
                                                     │ YAML loc: false, loop: false       │
                                                     │ YAMLval loc: true, loop: false     │
                                                     │ GraphML: true; server: false       │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ a.hind = 3
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ pr-758.c:21:3-21:11                │
                                                     │ (pr-758.c:21:10-21:11)             │
                                                     │ YAML loc: true, loop: false        │
                                                     │ YAMLval loc: true, loop: false     │
                                                     │ GraphML: true; server: true        │
                                                     └────────────────────────────────────┘
                                                       │
                                                       │ return 0
                                                       ▼
                                                     ┌────────────────────────────────────┐
                                                     │ return of main()                   │
                                                     └────────────────────────────────────┘

