  $ cfgDot 21-empty-loops.c

  $ graph-easy --as=boxart f_empty_goto_loop.dot
  ┌───────────────────────────────────────┐
  │ f_empty_goto_loop()                   │
  └───────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌───────────────────────────────────────┐
  │ 21-empty-loops.c:57:3-57:31           │
  │ (unknown)                             │
  │ [21-empty-loops.c:56:1-57:3           │
  │ (unknown)]                            │   skip
  │ YAML loc: 21-empty-loops.c:57:3-57:31 │ ───────┐
  │ YAMLval loc: true, loop: true         │        │
  │ GraphML: true; server: true           │ ◀──────┘
  └───────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌───────────────────────────────────────┐
  │ 21-empty-loops.c:58:1-58:1            │
  │ (unknown)                             │
  │ YAML loc: 21-empty-loops.c:58:1-58:1  │
  │ YAMLval loc: true, loop: false        │
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
  │ 21-empty-loops.c:62:3-62:14 (synthetic)    │
  │ (21-empty-loops.c:62:10-62:11 (synthetic)) │
  │ YAML loop: 21-empty-loops.c:62:3-62:14     │   Pos(1)
  │ YAMLval loc: true, loop: true              │ ─────────┐
  │ GraphML: true; server: false               │          │
  │ loop: 21-empty-loops.c:62:3-62:14          │ ◀────────┘
  └────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:63:1-63:1                 │
  │ (unknown)                                  │
  │ YAML loc: 21-empty-loops.c:63:1-63:1       │
  │ YAMLval loc: true, loop: false             │
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
             │ 21-empty-loops.c:75:3-75:11           │
             │ (21-empty-loops.c:75:3-75:11)         │
             │ YAML loc: 21-empty-loops.c:75:3-75:11 │
             │ YAMLval loc: true, loop: false        │
             │ GraphML: true; server: true           │
             └───────────────────────────────────────┘
               │
               │ suffix()
               ▼
             ┌───────────────────────────────────────┐
             │ 21-empty-loops.c:76:1-76:1            │
             │ (unknown)                             │
             │ YAML loc: 21-empty-loops.c:76:1-76:1  │
             │ YAMLval loc: true, loop: false        │
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
             │ 21-empty-loops.c:73:3-73:38           │  │
             │ (unknown)                             │  │
             │ [21-empty-loops.c:72:1-73:3           │  │
      skip   │ (unknown)]                            │  │
    ┌─────── │ YAML loc: 21-empty-loops.c:73:3-73:38 │  │
    │        │ YAMLval loc: true, loop: true         │  │
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
  │ 21-empty-loops.c:80:3-80:14 (synthetic)    │
  │ (21-empty-loops.c:80:10-80:11 (synthetic)) │
  │ YAML loop: 21-empty-loops.c:80:3-80:14     │   Pos(1)
  │ YAMLval loc: true, loop: true              │ ─────────┐
  │ GraphML: true; server: false               │          │
  │ loop: 21-empty-loops.c:80:3-80:14          │ ◀────────┘
  └────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:82:3-82:11                │
  │ (21-empty-loops.c:82:3-82:11)              │
  │ YAML loc: 21-empty-loops.c:82:3-82:11      │
  │ YAMLval loc: true, loop: false             │
  │ GraphML: true; server: true                │
  └────────────────────────────────────────────┘
    │
    │ suffix()
    ▼
  ┌────────────────────────────────────────────┐
  │ 21-empty-loops.c:83:1-83:1                 │
  │ (unknown)                                  │
  │ YAML loc: 21-empty-loops.c:83:1-83:1       │
  │ YAMLval loc: true, loop: false             │
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
  │ 21-empty-loops.c:93:3-93:9           │
  │ (21-empty-loops.c:93:3-93:9)         │   body()
  │ YAML loc: 21-empty-loops.c:93:3-93:9 │ ─────────┐
  │ YAMLval loc: true, loop: true        │          │
  │ GraphML: true; server: true          │ ◀────────┘
  └──────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────┐
  │ 21-empty-loops.c:95:1-95:1           │
  │ (unknown)                            │
  │ YAML loc: 21-empty-loops.c:95:1-95:1 │
  │ YAMLval loc: true, loop: false       │
  │ GraphML: true; server: true          │
  └──────────────────────────────────────┘
    │
    │ return
    ▼
  ┌──────────────────────────────────────┐
  │ return of f_nonempty_goto_loop()     │
  └──────────────────────────────────────┘

  $ graph-easy --as=boxart f_nonempty_while_loop.dot
  
    ┌───────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                                                                                                   │
    │                                                   ┌────────────────────────────────────────────┐  │
    │                                                   │ f_nonempty_while_loop()                    │  │
    │                                                   └────────────────────────────────────────────┘  │
    │                                                     │                                             │ body()
    │                                                     │ (body)                                      │
    │                                                     ▼                                             │
  ┌─────────────────────────────────────────┐           ┌────────────────────────────────────────────┐  │
  │ 21-empty-loops.c:101:5-101:11           │           │ 21-empty-loops.c:99:3-102:3 (synthetic)    │  │
  │ (21-empty-loops.c:101:5-101:11)         │           │ (21-empty-loops.c:99:10-99:11 (synthetic)) │  │
  │ YAML loc: 21-empty-loops.c:101:5-101:11 │           │ YAML loop: 21-empty-loops.c:99:3-102:3     │  │
  │ YAMLval loc: true, loop: false          │           │ YAMLval loc: true, loop: true              │  │
  │ GraphML: true; server: true             │  Pos(1)   │ GraphML: true; server: false               │  │
  │                                         │ ◀──────── │ loop: 21-empty-loops.c:99:3-102:3          │ ◀┘
  └─────────────────────────────────────────┘           └────────────────────────────────────────────┘
                                                          │
                                                          │ Neg(1)
                                                          ▼
                                                        ┌────────────────────────────────────────────┐
                                                        │ 21-empty-loops.c:103:1-103:1               │
                                                        │ (unknown)                                  │
                                                        │ YAML loc: 21-empty-loops.c:103:1-103:1     │
                                                        │ YAMLval loc: true, loop: false             │
                                                        │ GraphML: true; server: true                │
                                                        └────────────────────────────────────────────┘
                                                          │
                                                          │ return
                                                          ▼
                                                        ┌────────────────────────────────────────────┐
                                                        │ return of f_nonempty_while_loop()          │
                                                        └────────────────────────────────────────────┘


  $ graph-easy --as=boxart f_empty_goto_loop_prefix.dot
  ┌─────────────────────────────────────────┐
  │ f_empty_goto_loop_prefix()              │
  └─────────────────────────────────────────┘
    │
    │ (body)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:112:3-112:11           │
  │ (21-empty-loops.c:112:3-112:11)         │
  │ YAML loc: 21-empty-loops.c:112:3-112:11 │
  │ YAMLval loc: true, loop: false          │
  │ GraphML: true; server: true             │
  └─────────────────────────────────────────┘
    │
    │ prefix()
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:115:3-115:38           │
  │ (unknown)                               │
  │ [21-empty-loops.c:114:1-115:3           │
  │ (unknown)]                              │   skip
  │ YAML loc: 21-empty-loops.c:115:3-115:38 │ ───────┐
  │ YAMLval loc: true, loop: true           │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:116:1-116:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:116:1-116:1  │
  │ YAMLval loc: true, loop: false          │
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
  │ 21-empty-loops.c:120:3-120:11                │
  │ (21-empty-loops.c:120:3-120:11)              │
  │ YAML loc: 21-empty-loops.c:120:3-120:11      │
  │ YAMLval loc: true, loop: false               │
  │ GraphML: true; server: true                  │
  └──────────────────────────────────────────────┘
    │
    │ prefix()
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:122:3-122:14 (synthetic)    │
  │ (21-empty-loops.c:122:10-122:11 (synthetic)) │
  │ YAML loop: 21-empty-loops.c:122:3-122:14     │   Pos(1)
  │ YAMLval loc: true, loop: true                │ ─────────┐
  │ GraphML: true; server: false                 │          │
  │ loop: 21-empty-loops.c:122:3-122:14          │ ◀────────┘
  └──────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:123:1-123:1                 │
  │ (unknown)                                    │
  │ YAML loc: 21-empty-loops.c:123:1-123:1       │
  │ YAMLval loc: true, loop: false               │
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
  │ (unknown)                               │
  │ [21-empty-loops.c:127:1-128:3           │   skip
  │ (unknown)]                              │ ───────┐
  │ YAMLval loc: true, loop: true           │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:131:1-131:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:131:1-131:1  │
  │ YAMLval loc: true, loop: false          │
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
  │ 21-empty-loops.c:135:3-137:3 (synthetic)     │
  │ (21-empty-loops.c:135:10-135:11 (synthetic)) │
  │ YAML loop: 21-empty-loops.c:135:3-137:3      │   Pos(1)
  │ YAMLval loc: true, loop: true                │ ─────────┐
  │ GraphML: true; server: false                 │          │
  │ loop: 21-empty-loops.c:135:3-137:3           │ ◀────────┘
  └──────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌──────────────────────────────────────────────┐
  │ 21-empty-loops.c:138:1-138:1                 │
  │ (unknown)                                    │
  │ YAML loc: 21-empty-loops.c:138:1-138:1       │
  │ YAMLval loc: true, loop: false               │
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
  │ 21-empty-loops.c:143:3-143:42           │
  │ (unknown)                               │
  │ [21-empty-loops.c:142:1-143:3           │
  │ (unknown)]                              │   skip
  │ YAML loc: 21-empty-loops.c:143:3-143:42 │ ───────┐
  │ YAMLval loc: true, loop: true           │        │
  │ GraphML: true; server: true             │ ◀──────┘
  └─────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────┐
  │ 21-empty-loops.c:146:1-146:1            │
  │ (unknown)                               │
  │ YAML loc: 21-empty-loops.c:146:1-146:1  │
  │ YAMLval loc: true, loop: false          │
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
  │ (unknown)                                              │
  │ [21-empty-loops.c:150:1-151:3                          │   skip
  │ (unknown)]                                             │ ───────┐
  │ YAMLval loc: true, loop: true                          │        │
  │ GraphML: true; server: true                            │ ◀──────┘
  └────────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌────────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:155:1-155:1                           │
  │ (unknown)                                              │
  │ YAML loc: 21-empty-loops.c:155:1-155:1                 │
  │ YAMLval loc: true, loop: false                         │
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
  │ 21-empty-loops.c:160:3-160:59                           │
  │ (unknown)                                               │
  │ [21-empty-loops.c:159:1-160:3                           │
  │ (unknown)]                                              │   skip
  │ YAML loc: 21-empty-loops.c:160:3-160:59                 │ ───────┐
  │ YAMLval loc: true, loop: true                           │        │
  │ GraphML: true; server: true                             │ ◀──────┘
  └─────────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌─────────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:164:1-164:1                            │
  │ (unknown)                                               │
  │ YAML loc: 21-empty-loops.c:164:1-164:1                  │
  │ YAMLval loc: true, loop: false                          │
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
  │ (unknown)                                             │
  │ [21-empty-loops.c:168:1-169:3                         │   skip
  │ (unknown)]                                            │ ───────┐
  │ YAMLval loc: true, loop: true                         │        │
  │ GraphML: true; server: true                           │ ◀──────┘
  └───────────────────────────────────────────────────────┘
    │
    │ Neg(1)
    ▼
  ┌───────────────────────────────────────────────────────┐
  │ 21-empty-loops.c:174:1-174:1                          │
  │ (unknown)                                             │
  │ YAML loc: 21-empty-loops.c:174:1-174:1                │
  │ YAMLval loc: true, loop: false                        │
  │ GraphML: true; server: true                           │
  └───────────────────────────────────────────────────────┘
    │
    │ return
    ▼
  ┌───────────────────────────────────────────────────────┐
  │ return of f_empty_goto_loop_multiple_semicolon_both() │
  └───────────────────────────────────────────────────────┘
