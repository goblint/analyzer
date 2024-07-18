  $ cfgDot --unroll 1 11-unrolled-loop-invariant.c
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:3:3-4:8 with factor 1
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:8:5-9:10 with factor 1
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:7:3-11:3 with factor 1

  $ graph-easy --as=boxart main.dot
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ main()                                               │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ (body)
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:2:3-2:12                │
                                                   │ (11-unrolled-loop-invariant.c:2:7-2:12 (synthetic))  │
                                                   │ YAML loc: 11-unrolled-loop-invariant.c:2:3-2:12      │
                                                   │ GraphML: true; server: false                         │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ i = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:3:3-4:8 (synthetic)     │
                                                   │ (11-unrolled-loop-invariant.c:3:10-3:16 (synthetic)) │
                                                   │ YAML loop: 11-unrolled-loop-invariant.c:3:3-4:8      │
                                                   │ GraphML: true; server: false                         │
                                    ┌───────────── │ loop: 11-unrolled-loop-invariant.c:3:3-4:8           │ ·┐
                                    │              └──────────────────────────────────────────────────────┘  :
                                    │                │                                                       :
                                    │                │ Pos(i < 10)                                           :
                                    │                ▼                                                       :
                                    │              ┌──────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:4:5-4:8                 │  :
                                    │              │ (11-unrolled-loop-invariant.c:4:5-4:8)               │  :
                                    │              │ YAML loc: 11-unrolled-loop-invariant.c:4:5-4:8       │  :
                                    │              │ GraphML: true; server: true                          │ ·┼····································································┐
                                    │              └──────────────────────────────────────────────────────┘  :                                                                    :
                                    │                │                                                       :                                                                    :
                                    │                │ i = i + 1                                             :                                                                    :
                                    │ Neg(i < 10)    ▼                                                       ▼                                                                    :
                                    │              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:3:3-4:8 (synthetic)                                                                          │  :
                                    │              │ (11-unrolled-loop-invariant.c:3:10-3:16 (synthetic))                                                                      │  :
                                    │              │ YAML loop: 11-unrolled-loop-invariant.c:3:3-4:8                                                                           │  :
                                    │              │ GraphML: true; server: false                                                                                              │  :
                                    │              │ loop: 11-unrolled-loop-invariant.c:3:3-4:8                                                                                │  :
                                    │              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :
                                    │                │                                                       │               ▲                                                    :
                                    │                │ Neg(i < 10)                                           │ Pos(i < 10)   │ i = i + 1                                          :
                                    │                ▼                                                       │               │                                                    :
                                    │              ┌──────────────────────────────────────────────────────┐  │             ┌───────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:6:3-6:19                │  │             │ 11-unrolled-loop-invariant.c:4:5-4:8              │  :
                                    │              │ (11-unrolled-loop-invariant.c:6:7-6:12 (synthetic))  │  │             │ (11-unrolled-loop-invariant.c:4:5-4:8)            │  :
                                    │              │ YAML loc: 11-unrolled-loop-invariant.c:6:3-6:19      │  │             │ YAML loc: 11-unrolled-loop-invariant.c:4:5-4:8    │  :
                                    └────────────▶ │ GraphML: true; server: false                         │  └───────────▶ │ GraphML: true; server: true                       │ ◀┘
                                                   └──────────────────────────────────────────────────────┘                └───────────────────────────────────────────────────┘
                                                     │
                                                     │ j = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:6:3-6:19 (synthetic)    │
                                                   │ (11-unrolled-loop-invariant.c:6:14-6:19 (synthetic)) │
                                                   │ GraphML: true; server: false                         │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ k = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐                ┌───────────────────────────────────────────────────┐             ┌──────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:7:3-11:3 (synthetic)    │                │ 11-unrolled-loop-invariant.c:12:3-12:11           │             │                  │
                                                   │ (11-unrolled-loop-invariant.c:7:10-7:16 (synthetic)) │                │ (11-unrolled-loop-invariant.c:12:10-12:11)        │             │                  │
                                                   │ YAML loop: 11-unrolled-loop-invariant.c:7:3-11:3     │                │ YAML loc: 11-unrolled-loop-invariant.c:12:3-12:11 │             │ return of main() │
                                                   │ GraphML: true; server: false                         │  Neg(j < 10)   │ GraphML: true; server: true                       │  return 0   │                  │
                                    ┌············· │ loop: 11-unrolled-loop-invariant.c:7:3-11:3          │ ─────────────▶ │                                                   │ ──────────▶ │                  │
                                    :              └──────────────────────────────────────────────────────┘                └───────────────────────────────────────────────────┘             └──────────────────┘
                                    :                │                                                                       ▲                                                   Neg(j < 10)
                                    :                │ Pos(j < 10)                                                           └──────────────────────────────────────────────────────────────────────────────────────────┐
                                    :                ▼                                                                                                                                                                  │
                                    :              ┌──────────────────────────────────────────────────────┐                                                                                                             │
                                    :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)    │                                                                                                             │
                                    :              │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic)) │                                                                                                             │
                                    :              │ YAML loop: 11-unrolled-loop-invariant.c:8:5-9:10     │                                                                                                             │
                                    :              │ GraphML: true; server: false                         │                                                                                                             │
         ┌──────────────────────────┼───────────── │ loop: 11-unrolled-loop-invariant.c:8:5-9:10          │ ······································································┐                                     │
         │                          :              └──────────────────────────────────────────────────────┘                                                                       :                                     │
         │                          :                │                                                                                                                            :                                     │
         │                          :                │ Pos(k < 100)                                                                                                               :                                     │
         │                          :                ▼                                                                                                                            :                                     │
         │                          :              ┌──────────────────────────────────────────────────────┐                                                                       :                                     │
         │                          :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │                                                                       :                                     │
         │                          :              │ (11-unrolled-loop-invariant.c:9:7-9:10)              │                                                                       :                                     │
         │                          :              │ YAML loc: 11-unrolled-loop-invariant.c:9:7-9:10      │                                                                       :                                     │
         │                          :              │ GraphML: true; server: true                          │ ······································································┼············┐                        │
         │                          :              └──────────────────────────────────────────────────────┘                                                                       :            :                        │
         │                          :                │                                                                                                                            :            :                        │
         │                          :                │ k = k + 1                                                             ┌────────────────────────────────────────────────────┼────────────┼────────────────────────┼─────────────┐
         │                          :                ▼                                                                       │                                                    :            :                        │             │
         │                          :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                                         │  :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                                      │  :            :                        │             │
         │                          :              │ YAML loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                          │  :            :                        │             │
         │                          :              │ GraphML: true; server: false                                                                                              │  :            :                        │             │
         │                          :              │ loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                               │  :            :                        │             │
         │                          :              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                        │             │
         │                          :                │                                                       :               ▲                                                    :            :                        │             │
         │                          :                │ Pos(k < 100)                                          :               │ k = k + 1                                          :            :                        │             │
         │                          :                ▼                                                       :               │                                                    :            :                        │             │
         │                          :              ┌──────────────────────────────────────────────────────┐  :               │                                                    :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │  :               │                                                    :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:9:7-9:10)              │  :               │                                                    :            :                        │             │
         │                          :              │ YAML loc: 11-unrolled-loop-invariant.c:9:7-9:10      │  :               │                                                    :            :                        │             │
         │                          :              │ GraphML: true; server: true                          │ ─┼───────────────┘                                                    :            :                        │             │
         │                          :              └──────────────────────────────────────────────────────┘  :                                                                    :            :                        │             │
         │                          :                :                                                       :                                                                    :            :                        │             │
    ┌────┘                          :                :                                                       :                                                                    :            :                        │             │
    │                               :                ▼                                                       :                                                                    :            :                        │             │
    │                               :              ┌──────────────────────────────────────────────────────┐  :                                                                    :            :                        │             │
    │                               :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │  :                                                                    :            :                        │             │
    │                               :              │ (11-unrolled-loop-invariant.c:9:7-9:10)              │  :                                                                    :            :                        │             │
    │                               :              │ YAML loc: 11-unrolled-loop-invariant.c:9:7-9:10      │  :                                                                    :            :                        │             │
    │    ┌··························┼············▶ │ GraphML: true; server: true                          │ ◀┼───────────────┐                                                    :            :                        │             │
    │    :                          :              └──────────────────────────────────────────────────────┘  :               │                                                    :            :                        │             │
    │    :                          :                │                                                       :               │                                                    :            :                        │             │
    │    :                          :                │ k = k + 1                                             :               │ Pos(k < 100)                                       :            :                        │             │
    │    :                          :                ▼                                                       ▼               │                                                    :            :                        │             │
    │    :                          :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
    │    :                          :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                                         │  :            :                        │             │
    │    :                          :              │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                                      │  :            :                        │             │
    │    :                          :              │ YAML loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                          │  :            :                        │             │
    │    :        k = k + 1         :              │ GraphML: true; server: false                                                                                              │  :            :                        │             │
    │    :    ┌─────────────────────┼────────────▶ │ loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                               │ ◀┼············┼···················┐    │             │
    │    :    │                     :              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │
    │    :    │                     :                │                                                                                                                            :            :                   :    │             │
    │    :    │                     :                │ Neg(k < 100)                                                                                                               :            :                   :    │             │
    │    :    │                     :                ▼                                                                                                                            :            :                   :    │             │
    │    :    │                     :              ┌──────────────────────────────────────────────────────┐                                                                       :            :                   :    │             │
    │    :    │                     :              │ 11-unrolled-loop-invariant.c:10:5-10:8               │                                                                       :            :                   :    │             │
    │    :    │                     :              │ (11-unrolled-loop-invariant.c:10:5-10:8)             │                                                                       :            :                   :    │             │
    │    :    │                     :              │ YAML loc: 11-unrolled-loop-invariant.c:10:5-10:8     │                                                                       :            :                   :    │             │
    │    :    │    ┌────────────────┼────────────▶ │ GraphML: true; server: true                          │ ◀·····································································┼············┼···················┼····┼·············┼····┐
    │    :    │    │                :              └──────────────────────────────────────────────────────┘                                                                       :            :                   :    │             │    :
    │    :    │    │                :                │                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                :                │ j = j + 1                                                             ┌────────────────────────────────────────────────────┼────────────┼───────────────────┼────┘             │    :
    │    :    │    │                :                ▼                                                                       │                                                    :            :                   :                  │    :
    │    :    │    │                :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :                  │    :
    │    :    │    │                :              │ 11-unrolled-loop-invariant.c:7:3-11:3 (synthetic)                                                                         │  :            :                   :                  │    :
    │    :    │    │                :              │ (11-unrolled-loop-invariant.c:7:10-7:16 (synthetic))                                                                      │  :            :                   :                  │    :
    │    :    │    │ Neg(k < 100)   :              │ YAML loop: 11-unrolled-loop-invariant.c:7:3-11:3                                                                          │  :            :                   :                  │    :
    │    :    │    │                :              │ GraphML: true; server: false                                                                                              │  :            :                   :                  │    :
    │    :    │    │                └············▶ │ loop: 11-unrolled-loop-invariant.c:7:3-11:3                                                                               │ ◀┼────────────┼───────────────────┼────┐             │    :
    │    :    │    │                               └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │    :
    │    :    │    │                                 │                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                                 │ Pos(j < 10)                                                                                                                :            :                   :    │             │    :
    │    :    │    │                                 ▼                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                               ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :    │             │    :
    │    :    │    │                               │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                                         │  :            :                   :    │             │    :
    │    :    │    │                               │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                                      │  :            :                   :    │             │    :
    │    :    │    │                               │ YAML loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                          │  :            :                   :    │ j = j + 1   │    :
    │    :    │    │                               │ GraphML: true; server: false                                                                                              │  :            :                   :    │             │    :
    │    :    │    └────────────────────────────── │ loop: 11-unrolled-loop-invariant.c:8:5-9:10                                                                               │ ◀┘            :                   :    │             │    :
    │    :    │                                    └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘               :                   :    │             │    :
    │    :    │                                      │                                                                       :                                                                 :                   :    │             │    :
    │    :    │                                      │ Pos(k < 100)                                                          └·································································┼···················┘    │             │    :
    │    :    │                                      ▼                                                                                                                                         :                        │             │    :
    │    :    │                                    ┌──────────────────────────────────────────────────────┐                                                                                    :                        │             │    :
    │    :    │                                    │ 11-unrolled-loop-invariant.c:9:7-9:10                │                                                                                    :                        │             │    :
    │    :    │                                    │ (11-unrolled-loop-invariant.c:9:7-9:10)              │                                                                                    :                        │             │    :
    │    :    │                                    │ YAML loc: 11-unrolled-loop-invariant.c:9:7-9:10      │                                                                                    :                        │             │    :
    │    :    └─────────────────────────────────── │ GraphML: true; server: true                          │ ◀··················································································┘                   ┌····┼·············┼····┘
    │    :                                         └──────────────────────────────────────────────────────┘                                                                                                        :    │             │
    │    :                                           :                                                                                                                                                             :    │             │
    │    └···········································┘                                                                                                                                                             :    │             │
    │                                                                                                                                                                                                              :    │             │
    │                                                                                                                                                                                                              :    │             │
    │                                                ┌·····························································································································································┘    │             │
    │                                                :                                                                                                                                                                  │             │
    │                                              ┌──────────────────────────────────────────────────────┐                                                                                                             │             │
    │                                              │ 11-unrolled-loop-invariant.c:10:5-10:8               │                                                                                                             │             │
    │                                              │ (11-unrolled-loop-invariant.c:10:5-10:8)             │                                                                                                             │             │
    │                              Neg(k < 100)    │ YAML loc: 11-unrolled-loop-invariant.c:10:5-10:8     │                                                                                                             │             │
    └────────────────────────────────────────────▶ │ GraphML: true; server: true                          │ ────────────────────────────────────────────────────────────────────────────────────────────────────────────┘             │
                                                   └──────────────────────────────────────────────────────┘                                                                                                                           │
                                                     ▲                                                      Neg(k < 100)                                                                                                              │
                                                     └────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.entry-types '["location_invariant", "loop_invariant"]' 11-unrolled-loop-invariant.c
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:3:3-4:8 with factor 5
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:8:5-9:10 with factor 5
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:7:3-11:3 with factor 5
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:7:10-7:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:7:10-7:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:7:10-7:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:7:10-7:16)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:12-8:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:7:10-7:16)
  [Info][Witness] witness generation summary:
    total generation entries: 16

  $ yamlWitnessStrip < witness.yml
  - entry_type: loop_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 8
      column: 5
      function: main
    loop_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 8
      column: 5
      function: main
    loop_invariant:
      string: (((((((k == 100 && (((j == 2 || (5 <= j && j <= 9)) || j == 1) || j ==
        4)) || ((5 <= k && k <= 100) && j == 0)) || (j == 0 && k == 4)) || (j == 0 &&
        k == 3)) || (j == 0 && k == 2)) || (j == 0 && k == 1)) || (j == 0 && k == 0))
        || (j == 3 && k == 100)
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 7
      column: 3
      function: main
    loop_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 7
      column: 3
      function: main
    loop_invariant:
      string: ((k == 100 && (((j == 2 || (5 <= j && j <= 10)) || j == 1) || j == 4))
        || (j == 0 && k == 0)) || (j == 3 && k == 100)
      type: assertion
      format: C
  - entry_type: loop_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 3
      column: 3
      function: main
    loop_invariant:
      string: (((((5 <= i && i <= 10) || i == 4) || i == 3) || i == 2) || i == 1) ||
        i == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: k == 100
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: j == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 12
      column: 3
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 10
      column: 5
      function: main
    location_invariant:
      string: k == 100
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 10
      column: 5
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 10
      column: 5
      function: main
    location_invariant:
      string: ((((j == 1 || j == 4) || j == 0) || j == 3) || j == 2) || (5 <= j && j
        <= 9)
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 9
      column: 7
      function: main
    location_invariant:
      string: j == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 9
      column: 7
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 9
      column: 7
      function: main
    location_invariant:
      string: (((((5 <= k && k <= 99) || k == 4) || k == 3) || k == 2) || k == 1) ||
        k == 0
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 6
      column: 3
      function: main
    location_invariant:
      string: i == 10
      type: assertion
      format: C
  - entry_type: location_invariant
    location:
      file_name: 11-unrolled-loop-invariant.c
      file_hash: $FILE_HASH
      line: 4
      column: 5
      function: main
    location_invariant:
      string: (((((5 <= i && i <= 9) || i == 4) || i == 3) || i == 2) || i == 1) ||
        i == 0
      type: assertion
      format: C
