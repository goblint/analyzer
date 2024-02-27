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
  │ YAML loc: false, loop: false            │                             │ YAML loc: true, loop: false             │  │
  │ YAMLval loc: true, loop: false          │  Pos(b <= 0)                │ YAMLval loc: true, loop: false          │  │
  │ GraphML: true; server: false            │ ◀────────────────────────── │ GraphML: true; server: true             │  │
  └─────────────────────────────────────────┘                             └─────────────────────────────────────────┘  │
    │                                                                       │                                          │
    │                                                                       │ Neg(b <= 0)                              │
    │                                                                       ▼                                          │
    │                                                                     ┌─────────────────────────────────────────┐  │
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │  │
    │                                                                     │ (issue-1356.c:9:3-9:53 (synthetic))     │  │
    │                                                                     │ YAML loc: false, loop: false            │  │
    │                                                                     │ YAMLval loc: true, loop: false          │  │
    │                                                                     │ GraphML: true; server: false            │ ─┘
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ Neg((long )a >= (long )b - 2147483648)
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │
    │                                                                     │ (issue-1356.c:9:3-9:53 (synthetic))     │
    │                                                                     │ YAML loc: false, loop: false            │
    │                                                                     │ YAMLval loc: true, loop: false          │
    │                                                                     │ GraphML: true; server: false            │
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ tmp = 0
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:9:3-9:53 (synthetic)       │
    │                                                                     │ (issue-1356.c:9:3-9:53 (synthetic))     │
    │                                                                     │ YAML loc: false, loop: false            │
    │                                         tmp = 1                     │ YAMLval loc: true, loop: false          │
    └───────────────────────────────────────────────────────────────────▶ │ GraphML: true; server: false            │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ assume_abort_if_not(tmp)
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ issue-1356.c:10:3-10:53                 │
                                                                          │ (issue-1356.c:10:3-10:53)               │
                                                                          │ YAML loc: true, loop: false             │
                                                                          │ YAMLval loc: true, loop: false          │
                                                                          │ GraphML: true; server: true             │ ─┐
                                                                          └─────────────────────────────────────────┘  │
                                                                            │                                          │
                                                                            │ Neg(b >= 0)                              │
                                                                            ▼                                          │
  ┌─────────────────────────────────────────┐                             ┌─────────────────────────────────────────┐  │
  │ issue-1356.c:10:3-10:53 (synthetic)     │                             │ issue-1356.c:10:3-10:53 (synthetic)     │  │
  │ (issue-1356.c:10:3-10:53 (synthetic))   │                             │ (issue-1356.c:10:3-10:53 (synthetic))   │  │
  │ YAML loc: false, loop: false            │                             │ YAML loc: false, loop: false            │  │ Pos(b >= 0)
  │ YAMLval loc: true, loop: false          │  Neg(a <= b + 2147483647)   │ YAMLval loc: true, loop: false          │  │
  │ GraphML: true; server: false            │ ◀────────────────────────── │ GraphML: true; server: false            │  │
  └─────────────────────────────────────────┘                             └─────────────────────────────────────────┘  │
    │                                                                       │                                          │
    │                                                                       │ Pos(a <= b + 2147483647)                 │
    │                                                                       ▼                                          │
    │                                                                     ┌─────────────────────────────────────────┐  │
    │                                                                     │ issue-1356.c:10:3-10:53 (synthetic)     │  │
    │                                                                     │ (issue-1356.c:10:3-10:53 (synthetic))   │  │
    │                                                                     │ YAML loc: false, loop: false            │  │
    │                                                                     │ YAMLval loc: true, loop: false          │  │
    │                                                                     │ GraphML: true; server: false            │ ◀┘
    │                                                                     └─────────────────────────────────────────┘
    │                                                                       │
    │                                                                       │ tmp___0 = 1
    │                                                                       ▼
    │                                                                     ┌─────────────────────────────────────────┐
    │                                                                     │ issue-1356.c:10:3-10:53 (synthetic)     │
    │                                                                     │ (issue-1356.c:10:3-10:53 (synthetic))   │
    │                                                                     │ YAML loc: false, loop: false            │
    │                                         tmp___0 = 0                 │ YAMLval loc: true, loop: false          │
    └───────────────────────────────────────────────────────────────────▶ │ GraphML: true; server: false            │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ assume_abort_if_not(tmp___0)
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ issue-1356.c:11:3-11:15                 │
                                                                          │ (issue-1356.c:11:10-11:15)              │
                                                                          │ YAML loc: true, loop: false             │
                                                                          │ YAMLval loc: true, loop: false          │
                                                                          │ GraphML: true; server: true             │
                                                                          └─────────────────────────────────────────┘
                                                                            │
                                                                            │ return a - b
                                                                            ▼
                                                                          ┌─────────────────────────────────────────┐
                                                                          │ return of minus()                       │
                                                                          └─────────────────────────────────────────┘

