  $ cfgDot loops.c

  $ graph-easy --as=boxart main.dot
  
                                                  ┌─────────────────────────────────────────────────────────────────────────┐
                                                  │                                                                         │
                                                  │                                                                         │
    ┌─────────────────────────────────────────────┼────────────────────────────────────────────────────────────────────┐    │
    │                                             │                                                                    │    │
    │                                             │                                                                    │    │
    │    ┌────────────────────────────────────────┼────────────────────────────────────────────────────┐               │    │
    │    │                                        │                                                    │               │    │
    │    │                                        │             ┌───────────────────────────────────┐  │               │    │
    │    │    ┌───────────────────────────────────┘             │ main()                            │  │               │    │
    │    │    │                                                 └───────────────────────────────────┘  │               │    │
    │    │    │                                                   │                                    │               │    │
    │    │    │                                                   │ (body)                             │               │    │
    │    │    │                                                   ▼                                    │               │    │ i = i + 1
    │    │    │                                                 ┌───────────────────────────────────┐  │               │    │
    │    │    │                                                 │ loops.c:7:3-7:8                   │  │               │    │
    │    │    │                                                 │ (loops.c:7:3-7:8)                 │  │               │    │
    │    │    │                                                 │ YAML loc: true, loop: false       │  │               │    │
    │    │    │                                                 │ YAMLval loc: true, loop: false    │  │               │    │
    │    │    │                                                 │ GraphML: true; server: true       │  │               │    │
    │    │    │                                                 └───────────────────────────────────┘  │               │    │
    │    │    │                                                   │                                    │               │    │
    │    │    │                                                   │ i = 0                              │               │    │
    │    │    │                                                   ▼                                    │               │    │
    │    │  ┌──────────────────────────────────┐                ┌───────────────────────────────────┐  │               │    │
    │    │  │ loops.c:9:5-9:8                  │                │ loops.c:8:3-10:3 (synthetic)      │  │               │    │
    │    │  │ (loops.c:9:5-9:8)                │                │ (loops.c:8:10-8:16 (synthetic))   │  │               │    │
    │    │  │ YAML loc: true, loop: false      │                │ YAML loc: false, loop: true       │  │               │    │
    │    │  │ YAMLval loc: true, loop: false   │  Pos(i < 10)   │ YAMLval loc: true, loop: true     │  │               │    │
    │    │  │ GraphML: true; server: true      │ ◀───────────── │ GraphML: true; server: false      │ ◀┼───────────────┼────┘
    │    │  └──────────────────────────────────┘                └───────────────────────────────────┘  │               │
    │    │                                                        │                                    │               │
    │    │                                                        │ Neg(i < 10)                        │               │
    │    │                                                        ▼                                    │               │
    │    │                                                      ┌───────────────────────────────────┐  │               │
    │    │                                                      │ loops.c:13:3-15:3                 │  │               │
    │    │                                                      │ (loops.c:13:7-13:26 (synthetic))  │  │               │
    │    │                                                      │ YAML loc: true, loop: false       │  │ i = i + 1     │
    │    │                                                      │ YAMLval loc: true, loop: false    │  │               │
    │    │                                                      │ GraphML: true; server: false      │  │               │
    │    │                                                      └───────────────────────────────────┘  │               │
    │    │                                                        │                                    │               │
    │    │                                                        │ i = 0                              │               │
    │    │                                                        ▼                                    │               │
    │    │  ┌──────────────────────────────────┐                ┌───────────────────────────────────┐  │               │
    │    │  │ loops.c:14:5-14:23               │                │ loops.c:13:3-15:3 (synthetic)     │  │               │
    │    │  │ (loops.c:14:5-14:23)             │                │ (loops.c:13:7-13:26 (synthetic))  │  │               │
    │    │  │ YAML loc: true, loop: false      │                │ YAML loc: false, loop: true       │  │               │
    │    │  │ YAMLval loc: true, loop: false   │  Pos(i < 10)   │ YAMLval loc: true, loop: true     │  │               │
    │    │  │ GraphML: true; server: true      │ ◀───────────── │ GraphML: true; server: false      │ ◀┘               │
    │    │  └──────────────────────────────────┘                └───────────────────────────────────┘                  │
    │    │    │                                                   │                                                    │
    │    │    │ __goblint_check(1)                                │ Neg(i < 10)                                        │
    │    │    ▼                                                   ▼                                                    │
    │    │  ┌──────────────────────────────────┐                ┌───────────────────────────────────┐                  │
    │    │  │ loops.c:13:3-15:3 (synthetic)    │                │ loops.c:18:3-20:3                 │                  │
    │    │  │ (loops.c:13:7-13:26 (synthetic)) │                │ (loops.c:18:7-18:26 (synthetic))  │                  │
    │    │  │ YAML loc: false, loop: false     │                │ YAML loc: true, loop: false       │                  │
    │    │  │ YAMLval loc: true, loop: false   │                │ YAMLval loc: true, loop: false    │                  │
    │    └─ │ GraphML: true; server: false     │                │ GraphML: true; server: false      │                  │
    │       └──────────────────────────────────┘                └───────────────────────────────────┘                  │
    │                                                             │                                                    │
    │                                                             │ i = 0                                              │
    │                                                             ▼                                                    │
    │       ┌──────────────────────────────────┐                ┌───────────────────────────────────┐                  │
    │       │ loops.c:18:3-20:3 (synthetic)    │                │ loops.c:18:3-20:3 (synthetic)     │                  │
    │       │ (loops.c:18:7-18:26 (synthetic)) │                │ (loops.c:18:7-18:26 (synthetic))  │                  │
    │       │ YAML loc: false, loop: false     │                │ YAML loc: false, loop: true       │                  │
    │       │ YAMLval loc: true, loop: false   │  Pos(i < 10)   │ YAMLval loc: true, loop: true     │  i = i + 1       │
    └────── │ GraphML: true; server: false     │ ◀───────────── │ GraphML: true; server: false      │ ◀────────────────┘
            └──────────────────────────────────┘                └───────────────────────────────────┘
                                                                  │
                                                                  │ Neg(i < 10)
                                                                  ▼
                                                                ┌───────────────────────────────────┐
                                                                │ loops.c:23:3-25:3                 │
                                                                │ (loops.c:23:7-23:22 (synthetic))  │
                                                                │ YAML loc: true, loop: false       │
                                                                │ YAMLval loc: true, loop: false    │
                                                                │ GraphML: true; server: false      │
                                                                └───────────────────────────────────┘
                                                                  │
                                                                  │ i = 0
                                                                  ▼
            ┌──────────────────────────────────┐                ┌───────────────────────────────────┐
            │ loops.c:24:5-24:8                │                │ loops.c:23:3-25:3 (synthetic)     │
            │ (loops.c:24:5-24:8)              │                │ (loops.c:23:7-23:22 (synthetic))  │
            │ YAML loc: true, loop: false      │                │ YAML loc: false, loop: true       │
            │ YAMLval loc: true, loop: false   │  Pos(i < 10)   │ YAMLval loc: true, loop: true     │  i = i + 1
            │ GraphML: true; server: true      │ ◀───────────── │ GraphML: true; server: false      │ ◀─────────────────────┐
            └──────────────────────────────────┘                └───────────────────────────────────┘                       │
              │                                                   │                                                         │
              │                                                   │ Neg(i < 10)                                             │
              │                                                   ▼                                                         │
              │                                                 ┌───────────────────────────────────┐                       │
              │                                                 │ loops.c:28:3-28:8                 │                       │
              │                                                 │ (loops.c:28:3-28:8)               │                       │
              │                                                 │ YAML loc: true, loop: false       │                       │
              │                                                 │ YAMLval loc: true, loop: false    │                       │
              │                                                 │ GraphML: true; server: true       │                       │
              │                                                 └───────────────────────────────────┘                       │
              │                                                   │                                                         │
         ┌────┘                                                   │ i = 0                                                   │
         │                                                        ▼                                                         │
         │  ┌──────────────────────────────────┐                ┌───────────────────────────────────┐                       │
         │  │ loops.c:30:5-30:23               │                │ loops.c:29:3-31:3 (synthetic)     │                       │
         │  │ (loops.c:30:5-30:23)             │                │ (loops.c:29:7-29:21 (synthetic))  │                       │
         │  │ YAML loc: true, loop: false      │                │ YAML loc: false, loop: true       │                       │
         │  │ YAMLval loc: true, loop: false   │  Pos(i < 10)   │ YAMLval loc: true, loop: true     │  i = i + 1            │
         │  │ GraphML: true; server: true      │ ◀───────────── │ GraphML: true; server: false      │ ◀─────────────────────┼─────────────┐
         │  └──────────────────────────────────┘                └───────────────────────────────────┘                       │             │
         │    │                                                   │                                                         │             │
         │    │ __goblint_check(1)                                │ Neg(i < 10)                                             │             │
         │    ▼                                                   ▼                                                         │             │
         │  ┌──────────────────────────────────┐                ┌───────────────────────────────────┐                       │             │
         │  │ loops.c:29:3-31:3 (synthetic)    │                │ loops.c:34:3-34:8                 │                       │             │
         │  │ (loops.c:29:7-29:21 (synthetic)) │                │ (loops.c:34:3-34:8)               │                       │             │
         │  │ YAML loc: false, loop: false     │                │ YAML loc: true, loop: false       │                       │             │
         │  │ YAMLval loc: true, loop: false   │                │ YAMLval loc: true, loop: false    │                       │             │
         │  │ GraphML: true; server: false     │ ─┐             │ GraphML: true; server: true       │                       │             │
         │  └──────────────────────────────────┘  │             └───────────────────────────────────┘                       │             │
         │                                        │               │                                                         │             │
         │                                        │               │ i = 0                                                   │             │
         │                                        │               ▼                                                         │             │
         │                                        │             ┌───────────────────────────────────┐                       │             │
         │                                        │             │ loops.c:36:5-36:8                 │                       │             │
         │                                        │             │ (loops.c:36:5-36:8)               │                       │             │
         │                                        │             │ YAML loc: true, loop: true        │                       │             │
         │                                        │             │ YAMLval loc: true, loop: true     │                       │             │
         │                                        │             │ GraphML: true; server: true       │ ◀┐                    │             │
         │                                        │             └───────────────────────────────────┘  │                    │             │
         │                                        │               │                                    │                    │             │
         │                                        │               │ i = i + 1                          │ Pos(i < 10)        │             │
         │                                        │               ▼                                    │                    │             │
         │                                        │             ┌───────────────────────────────────┐  │                    │             │
         │                                        │             │ loops.c:35:3-37:19 (synthetic)    │  │                    │             │
         │                                        │             │ (loops.c:37:12-37:19 (synthetic)) │  │                    │             │
         │                                        │             │ YAML loc: false, loop: false      │  │                    │             │
         │                                        │             │ YAMLval loc: true, loop: false    │  │                    │             │
         │                                        │             │ GraphML: true; server: false      │ ─┘                    │             │
         │                                        │             └───────────────────────────────────┘                       │             │
         │                                        │               │                                                         │             │
         │                                        │               │ Neg(i < 10)                                             │             │
         │                                        │               ▼                                                         │             │
         │                                        │             ┌───────────────────────────────────┐                       │             │
         │                                        │             │ loops.c:39:3-39:11                │                       │             │
         │                                        │             │ (loops.c:39:10-39:11)             │                       │             │
         │                                        │             │ YAML loc: true, loop: false       │                       │             │
         │                                        │             │ YAMLval loc: true, loop: false    │                       │             │
         │                                        │             │ GraphML: true; server: true       │                       │             │
         │                                        │             └───────────────────────────────────┘                       │             │
         │                                        │               │                                                         │             │
         │                                        │               │ return 0                                                │             │
         │                                        │               ▼                                                         │             │
         │                                        │             ┌───────────────────────────────────┐                       │             │
         │                                        │             │ return of main()                  │                       │             │
         │                                        │             └───────────────────────────────────┘                       │             │
         │                                        │                                                                         │             │
         └────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────────┘             │
                                                  │                                                                                       │
                                                  │                                                                                       │
                                                  └───────────────────────────────────────────────────────────────────────────────────────┘


