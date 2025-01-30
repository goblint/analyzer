  $ cfgDot 21-empty-loops.c

  $ graph-easy --as=boxart f_empty_goto_loop.dot
  ┌───────────────────────────────────────┐
  │ f_empty_goto_loop()                   │
  └───────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌───────────────────────────────────────┐
  │ 21-empty-loops.c:58:3-58:31           │
  │ (unknown)                             │
  │ [21-empty-loops.c:57:1-58:3           │   skip
  │ (unknown)]                            │ ───────┐
  │ YAML loc: 21-empty-loops.c:58:3-58:31 │        │
  │ GraphML: true; server: true           │ ◀──────┘
  └───────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌───────────────────────────────────────┐
  │ 21-empty-loops.c:59:1-59:1            │
  │ (unknown)                             │
  │ YAML loc: 21-empty-loops.c:59:1-59:1  │
  │ GraphML: true; server: true           │
  └───────────────────────────────────────┘
    │
    │ return
    ▼
  ┌───────────────────────────────────────┐
  │ return of f_empty_goto_loop()         │
  └───────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_while_loop.dot
  ┌────────────────────────────────────────────┐
  │ f_empty_while_loop()                       │
  └────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:63:3-63:14 (synthetic)    │
  │ (21-empty-loops.c:63:10-63:11 (synthetic)) │   Pos(1)
  │ YAML loop: 21-empty-loops.c:63:3-63:14     │ ─────────┐
  │ GraphML: true; server: false               │          │
  │ loop: 21-empty-loops.c:63:3-63:14          │ ◀────────┘
  └────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:64:1-64:1                 │
  │ (unknown)                                  │
  │ YAML loc: 21-empty-loops.c:64:1-64:1       │
  │ GraphML: true; server: true                │
  └────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌────────────────────────────────────────────┐
  │ return of f_empty_while_loop()             │
  └────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_suffix.dot
             ┌───────────────────────────────────────┐
             │ 21-empty-loops.c:76:3-76:11           │
             │ (21-empty-loops.c:76:3-76:11)         │
             │ YAML loc: 21-empty-loops.c:76:3-76:11 │
             │ GraphML: true; server: true           │
             └───────────────────────────────────────┘
               │
               │ suffix()
               ▼
             ┌───────────────────────────────────────┐
             │ 21-empty-loops.c:77:1-77:1            │
             │ (unknown)                             │
             │ YAML loc: 21-empty-loops.c:77:1-77:1  │
             │ GraphML: true; server: true           │ ◀┐
             └───────────────────────────────────────┘  │
               │                                        │
               │ return                                 │
               ▼                                        │
             ┌───────────────────────────────────────┐  │
             │ return of f_empty_goto_loop_suffix()  │  │
             └───────────────────────────────────────┘  │
             ┌───────────────────────────────────────┐  │ Neg(1)
             │ f_empty_goto_loop_suffix()            │  │
             └───────────────────────────────────────┘  │
               │                                        │
               │ (body)                                 │
               ▼                                        │
             ┌───────────────────────────────────────┐  │
             │ 21-empty-loops.c:74:3-74:38           │  │
             │ (unknown)                             │  │
      skip   │ [21-empty-loops.c:73:1-74:3           │  │
    ┌─────── │ (unknown)]                            │  │
    │        │ YAML loc: 21-empty-loops.c:74:3-74:38 │  │
    └──────▶ │ GraphML: true; server: true           │ ─┘
             └───────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_while_loop_suffix.dot
  ┌────────────────────────────────────────────┐
  │ f_empty_while_loop_suffix()                │
  └────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:81:3-81:14 (synthetic)    │
  │ (21-empty-loops.c:81:10-81:11 (synthetic)) │   Pos(1)
  │ YAML loop: 21-empty-loops.c:81:3-81:14     │ ─────────┐
  │ GraphML: true; server: false               │          │
  │ loop: 21-empty-loops.c:81:3-81:14          │ ◀────────┘
  └────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:83:3-83:11                │
  │ (21-empty-loops.c:83:3-83:11)              │
  │ YAML loc: 21-empty-loops.c:83:3-83:11      │
  │ GraphML: true; server: true                │
  └────────────────────────────────────────────┘
    │
    │ suffix()
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:84:1-84:1                 │
  │ (unknown)                                  │
  │ YAML loc: 21-empty-loops.c:84:1-84:1       │
  │ GraphML: true; server: true                │
  └────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌────────────────────────────────────────────┐
  │ return of f_empty_while_loop_suffix()      │
  └────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_nonempty_goto_loop.dot
  ┌──────────────────────────────────────┐
  │ f_nonempty_goto_loop()               │
  └──────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌──────────────────────────────────────┐
  │ 21-empty-loops.c:94:3-94:9           │   body()
  │ (21-empty-loops.c:94:3-94:9)         │ ─────────┐
  │ YAML loc: 21-empty-loops.c:94:3-94:9 │          │
  │ GraphML: true; server: true          │ ◀────────┘
  └──────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────┐
  │ 21-empty-loops.c:96:1-96:1           │
  │ (unknown)                            │
  │ YAML loc: 21-empty-loops.c:96:1-96:1 │
  │ GraphML: true; server: true          │
  └──────────────────────────────────────┘
    │
    │ return
    ▼
  ┌──────────────────────────────────────┐
  │ return of f_nonempty_goto_loop()     │
  └──────────────────────────────────────┘

  $ graph-easy --as=boxart f_nonempty_while_loop.dot
  
    ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                                     │
    │                                                   ┌──────────────────────────────────────────────┐  │
    │                                                   │ f_nonempty_while_loop()                      │  │
    │                                                   └──────────────────────────────────────────────┘  │
    │                                                     │                                               │ body()
    │                                                     │ (body)                                        │
    │                                                     ▼                                               │
  ┌─────────────────────────────────────────┐           ┌──────────────────────────────────────────────┐  │
  │ 21-empty-loops.c:102:5-102:11           │           │ 21-empty-loops.c:100:3-103:3 (synthetic)     │  │
  │ (21-empty-loops.c:102:5-102:11)         │           │ (21-empty-loops.c:100:10-100:11 (synthetic)) │  │
  │ YAML loc: 21-empty-loops.c:102:5-102:11 │           │ YAML loop: 21-empty-loops.c:100:3-103:3      │  │
  │ GraphML: true; server: true             │  Pos(1)   │ GraphML: true; server: false                 │  │
  │                                         │ ◀──────── │ loop: 21-empty-loops.c:100:3-103:3           │ ◀┘
  └─────────────────────────────────────────┘           └──────────────────────────────────────────────┘
                                                          │
                                                          │ Neg(1)
                                                          ▼
                                                        ┌──────────────────────────────────────────────┐
                                                        │ 21-empty-loops.c:104:1-104:1                 │
                                                        │ (unknown)                                    │
                                                        │ YAML loc: 21-empty-loops.c:104:1-104:1       │
                                                        │ GraphML: true; server: true                  │
                                                        └──────────────────────────────────────────────┘
                                                          │
                                                          │ return
                                                          ▼
                                                        ┌──────────────────────────────────────────────┐
                                                        │ return of f_nonempty_while_loop()            │
                                                        └──────────────────────────────────────────────┘


  $ graph-easy --as=boxart f_empty_goto_loop_prefix.dot
  ┌─────────────────────────────────────────┐
  │ f_empty_goto_loop_prefix()              │
  └─────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:113:3-113:11           │
  │ (21-empty-loops.c:113:3-113:11)         │
  │ YAML loc: 21-empty-loops.c:113:3-113:11 │
  │ GraphML: true; server: true             │
  └─────────────────────────────────────────┘
    │
    │ prefix()
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:116:3-116:38           │
  │ (unknown)                               │
  │ [21-empty-loops.c:115:1-116:3           │   skip
  │ (unknown)]                              │ ───────┐
  │ YAML loc: 21-empty-loops.c:116:3-116:38 │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:117:1-117:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:117:1-117:1  │
  │ GraphML: true; server: true             │
  └─────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌─────────────────────────────────────────┐
  │ return of f_empty_goto_loop_prefix()    │
  └─────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_while_loop_prefix.dot
  ┌──────────────────────────────────────────────┐
  │ f_empty_while_loop_prefix()                  │
  └──────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:121:3-121:11                │
  │ (21-empty-loops.c:121:3-121:11)              │
  │ YAML loc: 21-empty-loops.c:121:3-121:11      │
  │ GraphML: true; server: true                  │
  └──────────────────────────────────────────────┘
    │
    │ prefix()
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:123:3-123:14 (synthetic)    │
  │ (21-empty-loops.c:123:10-123:11 (synthetic)) │   Pos(1)
  │ YAML loop: 21-empty-loops.c:123:3-123:14     │ ─────────┐
  │ GraphML: true; server: false                 │          │
  │ loop: 21-empty-loops.c:123:3-123:14          │ ◀────────┘
  └──────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:124:1-124:1                 │
  │ (unknown)                                    │
  │ YAML loc: 21-empty-loops.c:124:1-124:1       │
  │ GraphML: true; server: true                  │
  └──────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌──────────────────────────────────────────────┐
  │ return of f_empty_while_loop_prefix()        │
  └──────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_semicolon.dot
  ┌─────────────────────────────────────────┐
  │ f_empty_goto_loop_semicolon()           │
  └─────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌─────────────────────────────────────────┐
  │ unknown                                 │
  │ (unknown)                               │   skip
  │ [21-empty-loops.c:128:1-129:3           │ ───────┐
  │ (unknown)]                              │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:132:1-132:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:132:1-132:1  │
  │ GraphML: true; server: true             │
  └─────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌─────────────────────────────────────────┐
  │ return of f_empty_goto_loop_semicolon() │
  └─────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_while_loop_semicolon.dot
  ┌──────────────────────────────────────────────┐
  │ f_empty_while_loop_semicolon()               │
  └──────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:136:3-138:3 (synthetic)     │
  │ (21-empty-loops.c:136:10-136:11 (synthetic)) │   Pos(1)
  │ YAML loop: 21-empty-loops.c:136:3-138:3      │ ─────────┐
  │ GraphML: true; server: false                 │          │
  │ loop: 21-empty-loops.c:136:3-138:3           │ ◀────────┘
  └──────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:139:1-139:1                 │
  │ (unknown)                                    │
  │ YAML loc: 21-empty-loops.c:139:1-139:1       │
  │ GraphML: true; server: true                  │
  └──────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌──────────────────────────────────────────────┐
  │ return of f_empty_while_loop_semicolon()     │
  └──────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_multiple.dot
  ┌─────────────────────────────────────────┐
  │ f_empty_goto_loop_multiple()            │
  └─────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:144:3-144:42           │
  │ (unknown)                               │
  │ [21-empty-loops.c:143:1-144:3           │   skip
  │ (unknown)]                              │ ───────┐
  │ YAML loc: 21-empty-loops.c:144:3-144:42 │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:147:1-147:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:147:1-147:1  │
  │ GraphML: true; server: true             │
  └─────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌─────────────────────────────────────────┐
  │ return of f_empty_goto_loop_multiple()  │
  └─────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_multiple_semicolon_first.dot
  ┌────────────────────────────────────────────────────────┐
  │ f_empty_goto_loop_multiple_semicolon_first()           │
  └────────────────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌────────────────────────────────────────────────────────┐
  │ unknown                                                │
  │ (unknown)                                              │   skip
  │ [21-empty-loops.c:151:1-152:3                          │ ───────┐
  │ (unknown)]                                             │        │
  │ GraphML: true; server: true                            │ ◀──────┘
  └────────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:156:1-156:1                           │
  │ (unknown)                                              │
  │ YAML loc: 21-empty-loops.c:156:1-156:1                 │
  │ GraphML: true; server: true                            │
  └────────────────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌────────────────────────────────────────────────────────┐
  │ return of f_empty_goto_loop_multiple_semicolon_first() │
  └────────────────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_multiple_semicolon_second.dot
  ┌─────────────────────────────────────────────────────────┐
  │ f_empty_goto_loop_multiple_semicolon_second()           │
  └─────────────────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌─────────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:161:3-161:59                           │
  │ (unknown)                                               │
  │ [21-empty-loops.c:160:1-161:3                           │   skip
  │ (unknown)]                                              │ ───────┐
  │ YAML loc: 21-empty-loops.c:161:3-161:59                 │        │
  │ GraphML: true; server: true                             │ ◀──────┘
  └─────────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:165:1-165:1                            │
  │ (unknown)                                               │
  │ YAML loc: 21-empty-loops.c:165:1-165:1                  │
  │ GraphML: true; server: true                             │
  └─────────────────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌─────────────────────────────────────────────────────────┐
  │ return of f_empty_goto_loop_multiple_semicolon_second() │
  └─────────────────────────────────────────────────────────┘

  $ graph-easy --as=boxart f_empty_goto_loop_multiple_semicolon_both.dot
  ┌───────────────────────────────────────────────────────┐
  │ f_empty_goto_loop_multiple_semicolon_both()           │
  └───────────────────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌───────────────────────────────────────────────────────┐
  │ unknown                                               │
  │ (unknown)                                             │   skip
  │ [21-empty-loops.c:169:1-170:3                         │ ───────┐
  │ (unknown)]                                            │        │
  │ GraphML: true; server: true                           │ ◀──────┘
  └───────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌───────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:175:1-175:1                          │
  │ (unknown)                                             │
  │ YAML loc: 21-empty-loops.c:175:1-175:1                │
  │ GraphML: true; server: true                           │
  └───────────────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌───────────────────────────────────────────────────────┐
  │ return of f_empty_goto_loop_multiple_semicolon_both() │
  └───────────────────────────────────────────────────────┘
