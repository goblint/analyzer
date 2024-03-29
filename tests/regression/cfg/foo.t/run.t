  $ cfgDot foo.c

  $ graph-easy --as=boxart main.dot
                                                ┌───────────────────────────────┐
                                                │ main()                        │
                                                └───────────────────────────────┘
                                                  │
                                                  │ (body)
                                                  ▼
                                                ┌───────────────────────────────┐
                                                │ foo.c:2:7-2:12                │
                                                │ (foo.c:2:7-2:12)              │
                                                └───────────────────────────────┘
                                                  │
                                                  │ a = 1
                                                  ▼
                                                ┌───────────────────────────────┐
                                                │ foo.c:2:14-2:19               │
                                                │ (foo.c:2:14-2:19)             │
                                                └───────────────────────────────┘
                                                  │
                                                  │ b = 1
                                                  ▼
             ┌──────────────────┐               ┌───────────────────────────────┐
             │ foo.c:7:3-7:11   │  Neg(a > 0)   │ foo.c:3:3-6:3 (synthetic)     │
    ┌──────▶ │ (unknown)        │ ◀──────────── │ (foo.c:3:10-3:20 (synthetic)) │ ◀┐
    │        └──────────────────┘               └───────────────────────────────┘  │
    │          │                                  │                                │
    │          │ return 0                         │ Pos(a > 0)                     │
    │          ▼                                  ▼                                │
    │ Neg(b) ┌──────────────────┐               ┌───────────────────────────────┐  │
    │        │ return of main() │               │ foo.c:3:3-6:3 (synthetic)     │  │
    │        │                  │  ┌─────────── │ (foo.c:3:10-3:20 (synthetic)) │  │
    │        └──────────────────┘  │            └───────────────────────────────┘  │
    │                              │              │                                │
    └──────────────────────────────┘              │ Pos(b)                         │ b = b - 1
                                                  ▼                                │
                                                ┌───────────────────────────────┐  │
                                                │ foo.c:4:5-4:8                 │  │
                                                │ (foo.c:4:5-4:8)               │  │
                                                └───────────────────────────────┘  │
                                                  │                                │
                                                  │ a = a + 1                      │
                                                  ▼                                │
                                                ┌───────────────────────────────┐  │
                                                │ foo.c:5:5-5:8                 │  │
                                                │ (foo.c:5:5-5:8)               │ ─┘
                                                └───────────────────────────────┘
