  $ cfgDot --unroll 1 11-unrolled-loop-invariant.c
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:4:3-5:8 with factor 1
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:9:5-10:10 with factor 1
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:8:3-12:3 with factor 1

  $ graph-easy --as=boxart main.dot
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ main()                                               │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ (body)
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:3:3-3:12                │
                                                   │ (11-unrolled-loop-invariant.c:3:7-3:12 (synthetic))  │
                                                   │ YAML loc: 11-unrolled-loop-invariant.c:3:3-3:12      │
                                                   │ server: false                                        │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ i = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:4:3-5:8 (synthetic)     │
                                                   │ (11-unrolled-loop-invariant.c:4:10-4:16 (synthetic)) │
                                                   │ [11-unrolled-loop-invariant.c:4:3-5:8 (synthetic)    │
                                                   │ (unknown)]                                           │
                                                   │ YAML loop: 11-unrolled-loop-invariant.c:4:3-5:8      │
                                    ┌───────────── │ server: false                                        │ ·┐
                                    │              └──────────────────────────────────────────────────────┘  :
                                    │                │                                                       :
                                    │                │ Pos(i < 10)                                           :
                                    │                ▼                                                       :
                                    │              ┌──────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:5:5-5:8                 │  :
                                    │              │ (11-unrolled-loop-invariant.c:5:5-5:8)               │  :
                                    │              │ YAML loc: 11-unrolled-loop-invariant.c:5:5-5:8       │  :
                                    │              │ server: true                                         │ ·┼····································································┐
                                    │              └──────────────────────────────────────────────────────┘  :                                                                    :
                                    │                │                                                       :                                                                    :
                                    │                │ i = i + 1                                             :                                                                    :
                                    │ Neg(i < 10)    ▼                                                       ▼                                                                    :
                                    │              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:4:3-5:8 (synthetic)                                                                          │  :
                                    │              │ (11-unrolled-loop-invariant.c:4:10-4:16 (synthetic))                                                                      │  :
                                    │              │ [11-unrolled-loop-invariant.c:4:3-5:8 (synthetic)                                                                         │  :
                                    │              │ (unknown)]                                                                                                                │  :
                                    │              │ YAML loop: 11-unrolled-loop-invariant.c:4:3-5:8                                                                           │  :
                                    │              │ server: false                                                                                                             │  :
                                    │              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :
                                    │                │                                                       │               ▲                                                    :
                                    │                │ Neg(i < 10)                                           │ Pos(i < 10)   │ i = i + 1                                          :
                                    │                ▼                                                       │               │                                                    :
                                    │              ┌──────────────────────────────────────────────────────┐  │             ┌───────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:7:3-7:19                │  │             │ 11-unrolled-loop-invariant.c:5:5-5:8              │  :
                                    │              │ (11-unrolled-loop-invariant.c:7:7-7:12 (synthetic))  │  │             │ (11-unrolled-loop-invariant.c:5:5-5:8)            │  :
                                    │              │ YAML loc: 11-unrolled-loop-invariant.c:7:3-7:19      │  │             │ YAML loc: 11-unrolled-loop-invariant.c:5:5-5:8    │  :
                                    └────────────▶ │ server: false                                        │  └───────────▶ │ server: true                                      │ ◀┘
                                                   └──────────────────────────────────────────────────────┘                └───────────────────────────────────────────────────┘
                                                     │
                                                     │ j = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:7:3-7:19 (synthetic)    │
                                                   │ (11-unrolled-loop-invariant.c:7:14-7:19 (synthetic)) │
                                                   │ server: false                                        │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ k = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐                ┌───────────────────────────────────────────────────┐             ┌──────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:8:3-12:3 (synthetic)    │                │                                                   │             │                  │
                                                   │ (11-unrolled-loop-invariant.c:8:10-8:16 (synthetic)) │                │ 11-unrolled-loop-invariant.c:13:3-13:11           │             │                  │
                                                   │ [11-unrolled-loop-invariant.c:8:3-12:3 (synthetic)   │                │ (11-unrolled-loop-invariant.c:13:10-13:11)        │             │ return of main() │
                                                   │ (unknown)]                                           │                │ YAML loc: 11-unrolled-loop-invariant.c:13:3-13:11 │             │                  │
                                                   │ YAML loop: 11-unrolled-loop-invariant.c:8:3-12:3     │  Neg(j < 10)   │ server: true                                      │  return 0   │                  │
                                    ┌············· │ server: false                                        │ ─────────────▶ │                                                   │ ──────────▶ │                  │
                                    :              └──────────────────────────────────────────────────────┘                └───────────────────────────────────────────────────┘             └──────────────────┘
                                    :                │                                                                       ▲                                                   Neg(j < 10)
                                    :                │ Pos(j < 10)                                                           └──────────────────────────────────────────────────────────────────────────────────────────┐
                                    :                ▼                                                                                                                                                                  │
                                    :              ┌──────────────────────────────────────────────────────┐                                                                                                             │
                                    :              │ 11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)   │                                                                                                             │
                                    :              │ (11-unrolled-loop-invariant.c:9:12-9:19 (synthetic)) │                                                                                                             │
                                    :              │ [11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)  │                                                                                                             │
                                    :              │ (unknown)]                                           │                                                                                                             │
                                    :              │ YAML loop: 11-unrolled-loop-invariant.c:9:5-10:10    │                                                                                                             │
         ┌──────────────────────────┼───────────── │ server: false                                        │ ······································································┐                                     │
         │                          :              └──────────────────────────────────────────────────────┘                                                                       :                                     │
         │                          :                │                                                                                                                            :                                     │
         │                          :                │ Pos(k < 100)                                                                                                               :                                     │
         │                          :                ▼                                                                                                                            :                                     │
         │                          :              ┌──────────────────────────────────────────────────────┐                                                                       :                                     │
         │                          :              │ 11-unrolled-loop-invariant.c:10:7-10:10              │                                                                       :                                     │
         │                          :              │ (11-unrolled-loop-invariant.c:10:7-10:10)            │                                                                       :                                     │
         │                          :              │ YAML loc: 11-unrolled-loop-invariant.c:10:7-10:10    │                                                                       :                                     │
         │                          :              │ server: true                                         │ ······································································┼············┐                        │
         │                          :              └──────────────────────────────────────────────────────┘                                                                       :            :                        │
         │                          :                │                                                                                                                            :            :                        │
         │                          :                │ k = k + 1                                                             ┌────────────────────────────────────────────────────┼────────────┼────────────────────────┼─────────────┐
         │                          :                ▼                                                                       │                                                    :            :                        │             │
         │                          :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                        │  :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:9:12-9:19 (synthetic))                                                                      │  :            :                        │             │
         │                          :              │ [11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                       │  :            :                        │             │
         │                          :              │ (unknown)]                                                                                                                │  :            :                        │             │
         │                          :              │ YAML loop: 11-unrolled-loop-invariant.c:9:5-10:10                                                                         │  :            :                        │             │
         │                          :              │ server: false                                                                                                             │  :            :                        │             │
         │                          :              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                        │             │
         │                          :                │                                                       :               ▲                                                    :            :                        │             │
         │                          :                │ Pos(k < 100)                                          :               │ k = k + 1                                          :            :                        │             │
         │                          :                ▼                                                       :               │                                                    :            :                        │             │
         │                          :              ┌──────────────────────────────────────────────────────┐  :               │                                                    :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:10:7-10:10              │  :               │                                                    :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:10:7-10:10)            │  :               │                                                    :            :                        │             │
         │                          :              │ YAML loc: 11-unrolled-loop-invariant.c:10:7-10:10    │  :               │                                                    :            :                        │             │
         │                          :              │ server: true                                         │ ─┼───────────────┘                                                    :            :                        │             │
         │                          :              └──────────────────────────────────────────────────────┘  :                                                                    :            :                        │             │
         │                          :                :                                                       :                                                                    :            :                        │             │
    ┌────┘                          :                :                                                       :                                                                    :            :                        │             │
    │                               :                ▼                                                       :                                                                    :            :                        │             │
    │                               :              ┌──────────────────────────────────────────────────────┐  :                                                                    :            :                        │             │
    │                               :              │ 11-unrolled-loop-invariant.c:10:7-10:10              │  :                                                                    :            :                        │             │
    │                               :              │ (11-unrolled-loop-invariant.c:10:7-10:10)            │  :                                                                    :            :                        │             │
    │                               :              │ YAML loc: 11-unrolled-loop-invariant.c:10:7-10:10    │  :                                                                    :            :                        │             │
    │    ┌··························┼············▶ │ server: true                                         │ ◀┼───────────────┐                                                    :            :                        │             │
    │    :                          :              └──────────────────────────────────────────────────────┘  :               │                                                    :            :                        │             │
    │    :                          :                │                                                       :               │                                                    :            :                        │             │
    │    :                          :                │ k = k + 1                                             :               │ Pos(k < 100)                                       :            :                        │             │
    │    :                          :                ▼                                                       ▼               │                                                    :            :                        │             │
    │    :                          :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
    │    :                          :              │ 11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                        │  :            :                        │             │
    │    :                          :              │ (11-unrolled-loop-invariant.c:9:12-9:19 (synthetic))                                                                      │  :            :                        │             │
    │    :                          :              │ [11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                       │  :            :                        │             │
    │    :                          :              │ (unknown)]                                                                                                                │  :            :                        │             │
    │    :        k = k + 1         :              │ YAML loop: 11-unrolled-loop-invariant.c:9:5-10:10                                                                         │  :            :                        │             │
    │    :    ┌─────────────────────┼────────────▶ │ server: false                                                                                                             │ ◀┼············┼···················┐    │             │
    │    :    │                     :              └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │
    │    :    │                     :                │                                                                                                                            :            :                   :    │             │
    │    :    │                     :                │ Neg(k < 100)                                                                                                               :            :                   :    │             │
    │    :    │                     :                ▼                                                                                                                            :            :                   :    │             │
    │    :    │                     :              ┌──────────────────────────────────────────────────────┐                                                                       :            :                   :    │             │
    │    :    │                     :              │ 11-unrolled-loop-invariant.c:11:5-11:8               │                                                                       :            :                   :    │             │
    │    :    │                     :              │ (11-unrolled-loop-invariant.c:11:5-11:8)             │                                                                       :            :                   :    │             │
    │    :    │                     :              │ YAML loc: 11-unrolled-loop-invariant.c:11:5-11:8     │                                                                       :            :                   :    │             │
    │    :    │    ┌────────────────┼────────────▶ │ server: true                                         │ ◀·····································································┼············┼···················┼····┼·············┼····┐
    │    :    │    │                :              └──────────────────────────────────────────────────────┘                                                                       :            :                   :    │             │    :
    │    :    │    │                :                │                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                :                │ j = j + 1                                                             ┌────────────────────────────────────────────────────┼────────────┼───────────────────┼────┘             │    :
    │    :    │    │                :                ▼                                                                       │                                                    :            :                   :                  │    :
    │    :    │    │                :              ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :                  │    :
    │    :    │    │                :              │ 11-unrolled-loop-invariant.c:8:3-12:3 (synthetic)                                                                         │  :            :                   :                  │    :
    │    :    │    │                :              │ (11-unrolled-loop-invariant.c:8:10-8:16 (synthetic))                                                                      │  :            :                   :                  │    :
    │    :    │    │                :              │ [11-unrolled-loop-invariant.c:8:3-12:3 (synthetic)                                                                        │  :            :                   :                  │    :
    │    :    │    │ Neg(k < 100)   :              │ (unknown)]                                                                                                                │  :            :                   :                  │    :
    │    :    │    │                :              │ YAML loop: 11-unrolled-loop-invariant.c:8:3-12:3                                                                          │  :            :                   :                  │    :
    │    :    │    │                └············▶ │ server: false                                                                                                             │ ◀┼────────────┼───────────────────┼────┐             │    :
    │    :    │    │                               └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │    :
    │    :    │    │                                 │                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                                 │ Pos(j < 10)                                                                                                                :            :                   :    │             │    :
    │    :    │    │                                 ▼                                                                                                                            :            :                   :    │             │    :
    │    :    │    │                               ┌───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :    │             │    :
    │    :    │    │                               │ 11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                        │  :            :                   :    │             │    :
    │    :    │    │                               │ (11-unrolled-loop-invariant.c:9:12-9:19 (synthetic))                                                                      │  :            :                   :    │             │    :
    │    :    │    │                               │ [11-unrolled-loop-invariant.c:9:5-10:10 (synthetic)                                                                       │  :            :                   :    │             │    :
    │    :    │    │                               │ (unknown)]                                                                                                                │  :            :                   :    │ j = j + 1   │    :
    │    :    │    │                               │ YAML loop: 11-unrolled-loop-invariant.c:9:5-10:10                                                                         │  :            :                   :    │             │    :
    │    :    │    └────────────────────────────── │ server: false                                                                                                             │ ◀┘            :                   :    │             │    :
    │    :    │                                    └───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘               :                   :    │             │    :
    │    :    │                                      │                                                                       :                                                                 :                   :    │             │    :
    │    :    │                                      │ Pos(k < 100)                                                          └·································································┼···················┘    │             │    :
    │    :    │                                      ▼                                                                                                                                         :                        │             │    :
    │    :    │                                    ┌──────────────────────────────────────────────────────┐                                                                                    :                        │             │    :
    │    :    │                                    │ 11-unrolled-loop-invariant.c:10:7-10:10              │                                                                                    :                        │             │    :
    │    :    │                                    │ (11-unrolled-loop-invariant.c:10:7-10:10)            │                                                                                    :                        │             │    :
    │    :    │                                    │ YAML loc: 11-unrolled-loop-invariant.c:10:7-10:10    │                                                                                    :                        │             │    :
    │    :    └─────────────────────────────────── │ server: true                                         │ ◀··················································································┘                   ┌····┼·············┼····┘
    │    :                                         └──────────────────────────────────────────────────────┘                                                                                                        :    │             │
    │    :                                           :                                                                                                                                                             :    │             │
    │    └···········································┘                                                                                                                                                             :    │             │
    │                                                                                                                                                                                                              :    │             │
    │                                                                                                                                                                                                              :    │             │
    │                                                ┌·····························································································································································┘    │             │
    │                                                :                                                                                                                                                                  │             │
    │                                              ┌──────────────────────────────────────────────────────┐                                                                                                             │             │
    │                                              │ 11-unrolled-loop-invariant.c:11:5-11:8               │                                                                                                             │             │
    │                                              │ (11-unrolled-loop-invariant.c:11:5-11:8)             │                                                                                                             │             │
    │                              Neg(k < 100)    │ YAML loc: 11-unrolled-loop-invariant.c:11:5-11:8     │                                                                                                             │             │
    └────────────────────────────────────────────▶ │ server: true                                         │ ────────────────────────────────────────────────────────────────────────────────────────────────────────────┘             │
                                                   └──────────────────────────────────────────────────────┘                                                                                                                           │
                                                     ▲                                                      Neg(k < 100)                                                                                                              │
                                                     └────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant", "loop_invariant"]' 11-unrolled-loop-invariant.c
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:4:3-5:8 with factor 5
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:9:5-10:10 with factor 5
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:8:3-12:3 with factor 5
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 10
    dead: 0
    total lines: 10
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:10-8:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:10-8:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:4:10-4:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:4:10-4:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:4:10-4:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:4:10-4:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:4:10-4:16)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:10-8:16)
  [Warning][Deadcode][CWE-570] condition 'k < 100' (possibly inserted by CIL) is always false (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:10-8:16)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'k < 100' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:9:12-9:19)
  [Warning][Deadcode][CWE-571] condition 'j < 10' (possibly inserted by CIL) is always true (11-unrolled-loop-invariant.c:8:10-8:16)
  [Info][Witness] witness generation summary:
    location invariants: 11
    loop invariants: 5
    flow-insensitive invariants: 0
    total generation entries: 1

  $ yamlWitnessStrip < witness.yml
  - entry_type: invariant_set
    content:
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 5
          column: 5
          function: main
        value: (((((5 <= i && i <= 9) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 7
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 10
          column: 7
          function: main
        value: (((((5 <= k && k <= 99) || k == 4) || k == 3) || k == 2) || k == 1) ||
          k == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 10
          column: 7
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 10
          column: 7
          function: main
        value: j == 0
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 11
          column: 5
          function: main
        value: ((((j == 1 || j == 4) || j == 0) || j == 3) || j == 2) || (5 <= j &&
          j <= 9)
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 11
          column: 5
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 11
          column: 5
          function: main
        value: k == 100
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 13
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 13
          column: 3
          function: main
        value: j == 10
        format: c_expression
    - invariant:
        type: location_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 13
          column: 3
          function: main
        value: k == 100
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 4
          column: 3
          function: main
        value: (((((5 <= i && i <= 10) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 8
          column: 3
          function: main
        value: ((k == 100 && (((j == 2 || (5 <= j && j <= 10)) || j == 1) || j == 4))
          || (j == 0 && k == 0)) || (j == 3 && k == 100)
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 8
          column: 3
          function: main
        value: i == 10
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 9
          column: 5
          function: main
        value: (((((((k == 100 && (((j == 2 || (5 <= j && j <= 9)) || j == 1) || j ==
          4)) || ((5 <= k && k <= 100) && j == 0)) || (j == 0 && k == 4)) || (j == 0
          && k == 3)) || (j == 0 && k == 2)) || (j == 0 && k == 1)) || (j == 0 && k
          == 0)) || (j == 3 && k == 100)
        format: c_expression
    - invariant:
        type: loop_invariant
        location:
          file_name: 11-unrolled-loop-invariant.c
          line: 9
          column: 5
          function: main
        value: i == 10
        format: c_expression
