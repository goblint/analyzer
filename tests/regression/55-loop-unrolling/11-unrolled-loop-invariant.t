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
                                                   │ 11-unrolled-loop-invariant.c:2:7-2:12                │
                                                   │ (11-unrolled-loop-invariant.c:2:7-2:12)              │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ i = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:3:3-4:8 (synthetic)     │
                                    ┌───────────── │ (11-unrolled-loop-invariant.c:3:10-3:16 (synthetic)) │ ·┐
                                    │              └──────────────────────────────────────────────────────┘  :
                                    │                │                                                       :
                                    │                │ Pos(i < 10)                                           :
                                    │                ▼                                                       :
                                    │              ┌──────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:4:5-4:8                 │  :
                                    │              │ (11-unrolled-loop-invariant.c:4:5-4:8)               │ ·┼··························································┐
                                    │              └──────────────────────────────────────────────────────┘  :                                                          :
                                    │                │                                                       :                                                          :
                                    │ Neg(i < 10)    │ i = i + 1                                             :                                                          :
                                    │                ▼                                                       ▼                                                          :
                                    │              ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:3:3-4:8 (synthetic)                                                                │  :
                                    │              │ (11-unrolled-loop-invariant.c:3:10-3:16 (synthetic))                                                            │  :
                                    │              └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :
                                    │                │                                                       │               ▲                                          :
                                    │                │ Neg(i < 10)                                           │ Pos(i < 10)   │ i = i + 1                                :
                                    │                ▼                                                       │               │                                          :
                                    │              ┌──────────────────────────────────────────────────────┐  │             ┌─────────────────────────────────────────┐  :
                                    │              │ 11-unrolled-loop-invariant.c:6:7-6:12                │  │             │ 11-unrolled-loop-invariant.c:4:5-4:8    │  :
                                    └────────────▶ │ (11-unrolled-loop-invariant.c:6:7-6:12)              │  └───────────▶ │ (11-unrolled-loop-invariant.c:4:5-4:8)  │ ◀┘
                                                   └──────────────────────────────────────────────────────┘                └─────────────────────────────────────────┘
                                                     │
                                                     │ j = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:6:14-6:19               │
                                                   │ (11-unrolled-loop-invariant.c:6:14-6:19)             │
                                                   └──────────────────────────────────────────────────────┘
                                                     │
                                                     │ k = 0
                                                     ▼
                                                   ┌──────────────────────────────────────────────────────┐                ┌─────────────────────────────────────────┐             ┌──────────────────┐
                                                   │ 11-unrolled-loop-invariant.c:7:3-11:3 (synthetic)    │  Neg(j < 10)   │ 11-unrolled-loop-invariant.c:12:3-12:11 │  return 0   │ return of main() │
                                    ┌············· │ (11-unrolled-loop-invariant.c:7:10-7:16 (synthetic)) │ ─────────────▶ │ (unknown)                               │ ──────────▶ │                  │
                                    :              └──────────────────────────────────────────────────────┘                └─────────────────────────────────────────┘             └──────────────────┘
                                    :                │                                                                       ▲                                         Neg(j < 10)
                                    :                │ Pos(j < 10)                                                           └────────────────────────────────────────────────────────────────────────────────┐
                                    :                ▼                                                                                                                                                        │
                                    :              ┌──────────────────────────────────────────────────────┐                                                                                                   │
                                    :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)    │                                                                                                   │
         ┌──────────────────────────┼───────────── │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic)) │ ····························································┐                                     │
         │                          :              └──────────────────────────────────────────────────────┘                                                             :                                     │
         │                          :                │                                                                                                                  :                                     │
         │                          :                │ Pos(k < 100)                                                                                                     :                                     │
         │                          :                ▼                                                                                                                  :                                     │
         │                          :              ┌──────────────────────────────────────────────────────┐                                                             :                                     │
         │                          :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │                                                             :                                     │
         │                          :              │ (11-unrolled-loop-invariant.c:9:7-9:10)              │ ····························································┼············┐                        │
         │                          :              └──────────────────────────────────────────────────────┘                                                             :            :                        │
         │                          :                │                                                                                                                  :            :                        │
         │                          :                │ k = k + 1                                                             ┌──────────────────────────────────────────┼────────────┼────────────────────────┼─────────────┐
         │                          :                ▼                                                                       │                                          :            :                        │             │
         │                          :              ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                               │  :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                            │  :            :                        │             │
         │                          :              └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                        │             │
         │                          :                │                                                       :               ▲                                          :            :                        │             │
         │                          :                │ Pos(k < 100)                                          :               │ k = k + 1                                :            :                        │             │
         │                          :                ▼                                                       :               │                                          :            :                        │             │
         │                          :              ┌──────────────────────────────────────────────────────┐  :               │                                          :            :                        │             │
         │                          :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │  :               │                                          :            :                        │             │
         │                          :              │ (11-unrolled-loop-invariant.c:9:7-9:10)              │ ─┼───────────────┘                                          :            :                        │             │
         │                          :              └──────────────────────────────────────────────────────┘  :                                                          :            :                        │             │
         │                          :                :                                                       :                                                          :            :                        │             │
    ┌────┘                          :                :                                                       :                                                          :            :                        │             │
    │                               :                ▼                                                       :                                                          :            :                        │             │
    │                               :              ┌──────────────────────────────────────────────────────┐  :                                                          :            :                        │             │
    │                               :              │ 11-unrolled-loop-invariant.c:9:7-9:10                │  :                                                          :            :                        │             │
    │    ┌··························┼············▶ │ (11-unrolled-loop-invariant.c:9:7-9:10)              │ ◀┼───────────────┐                                          :            :                        │             │
    │    :                          :              └──────────────────────────────────────────────────────┘  :               │                                          :            :                        │             │
    │    :                          :                │                                                       :               │                                          :            :                        │             │
    │    :                          :                │ k = k + 1                                             :               │ Pos(k < 100)                             :            :                        │             │
    │    :                          :                ▼                                                       ▼               │                                          :            :                        │             │
    │    :                          :              ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                        │             │
    │    :        k = k + 1         :              │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                               │  :            :                        │             │
    │    :    ┌─────────────────────┼────────────▶ │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                            │ ◀┼············┼···················┐    │             │
    │    :    │                     :              └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │
    │    :    │                     :                │                                                                                                                  :            :                   :    │             │
    │    :    │                     :                │ Neg(k < 100)                                                                                                     :            :                   :    │             │
    │    :    │                     :                ▼                                                                                                                  :            :                   :    │             │
    │    :    │                     :              ┌──────────────────────────────────────────────────────┐                                                             :            :                   :    │             │
    │    :    │                     :              │ 11-unrolled-loop-invariant.c:10:5-10:8               │                                                             :            :                   :    │             │
    │    :    │    ┌────────────────┼────────────▶ │ (11-unrolled-loop-invariant.c:10:5-10:8)             │ ◀···························································┼············┼···················┼····┼·············┼····┐
    │    :    │    │                :              └──────────────────────────────────────────────────────┘                                                             :            :                   :    │             │    :
    │    :    │    │                :                │                                                                                                                  :            :                   :    │             │    :
    │    :    │    │                :                │ j = j + 1                                                             ┌──────────────────────────────────────────┼────────────┼───────────────────┼────┘             │    :
    │    :    │    │                :                ▼                                                                       │                                          :            :                   :                  │    :
    │    :    │    │                :              ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :                  │    :
    │    :    │    │                :              │ 11-unrolled-loop-invariant.c:7:3-11:3 (synthetic)                                                               │  :            :                   :                  │    :
    │    :    │    │ Neg(k < 100)   └············▶ │ (11-unrolled-loop-invariant.c:7:10-7:16 (synthetic))                                                            │ ◀┼────────────┼───────────────────┼────┐             │    :
    │    :    │    │                               └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘  :            :                   :    │             │    :
    │    :    │    │                                 │                                                                                                                  :            :                   :    │             │    :
    │    :    │    │                                 │ Pos(j < 10)                                                                                                      :            :                   :    │             │    :
    │    :    │    │                                 ▼                                                                                                                  :            :                   :    │             │    :
    │    :    │    │                               ┌─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐  :            :                   :    │             │    :
    │    :    │    │                               │ 11-unrolled-loop-invariant.c:8:5-9:10 (synthetic)                                                               │  :            :                   :    │             │    :
    │    :    │    └────────────────────────────── │ (11-unrolled-loop-invariant.c:8:12-8:19 (synthetic))                                                            │ ◀┘            :                   :    │ j = j + 1   │    :
    │    :    │                                    └─────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘               :                   :    │             │    :
    │    :    │                                      │                                                                       :                                                       :                   :    │             │    :
    │    :    │                                      │ Pos(k < 100)                                                          └·······················································┼···················┘    │             │    :
    │    :    │                                      ▼                                                                                                                               :                        │             │    :
    │    :    │                                    ┌──────────────────────────────────────────────────────┐                                                                          :                        │             │    :
    │    :    │                                    │ 11-unrolled-loop-invariant.c:9:7-9:10                │                                                                          :                        │             │    :
    │    :    └─────────────────────────────────── │ (11-unrolled-loop-invariant.c:9:7-9:10)              │ ◀········································································┘                   ┌····┼·············┼····┘
    │    :                                         └──────────────────────────────────────────────────────┘                                                                                              :    │             │
    │    :                                           :                                                                                                                                                   :    │             │
    │    └···········································┘                                                                                                                                                   :    │             │
    │                                                                                                                                                                                                    :    │             │
    │                                                                                                                                                                                                    :    │             │
    │                                                ┌···················································································································································┘    │             │
    │                                                :                                                                                                                                                        │             │
    │                                              ┌──────────────────────────────────────────────────────┐                                                                                                   │             │
    │                              Neg(k < 100)    │ 11-unrolled-loop-invariant.c:10:5-10:8               │                                                                                                   │             │
    └────────────────────────────────────────────▶ │ (11-unrolled-loop-invariant.c:10:5-10:8)             │ ──────────────────────────────────────────────────────────────────────────────────────────────────┘             │
                                                   └──────────────────────────────────────────────────────┘                                                                                                                 │
                                                     ▲                                                      Neg(k < 100)                                                                                                    │
                                                     └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘

  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled 11-unrolled-loop-invariant.c --set dbg.level debug 2>&1 | tail -n +2
  [Debug] 'goblint' '--set' 'lib.activated' '[]' '--set' 'exp.unrolling-factor' '5' '--enable' 'ana.int.interval' '--enable' 'witness.yaml.enabled' '11-unrolled-loop-invariant.c' '--set' 'dbg.level' 'debug'
  [Debug] Custom include dirs:
  [Debug]   1. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/include (exists=true)
  [Debug]   2. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/runtime/include (exists=true)
  [Debug]   3. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/src (exists=true)
  [Debug] Preprocessing files.
  [Debug] Preprocessor cpp: is_bad=false
  [Debug] 'cpp' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/include' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/runtime/include' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/src' '11-unrolled-loop-invariant.c' '-o' '.goblint/preprocessed/11-unrolled-loop-invariant.i'
  [Debug] Parsing files.
  Frontc is parsing .goblint/preprocessed/11-unrolled-loop-invariant.i
  Converting CABS->CIL
  [Debug] Constructors: 
  [Debug] Adding constructors to: main
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:3:3-4:8 with factor 5
  [Debug] Marking if (! (i < 10)) {
            break;
  } as copy of if (! (i < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end; as copy of break;
  [Debug] Marking {
          i ++;
  } as copy of {
               i ++;
  }
  [Debug] Marking i ++; as copy of i ++;
  [Debug] Marking if (! (i < 10)) {
            break;
  } as copy of if (! (i < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end; as copy of break;
  [Debug] Marking {
          i ++;
  } as copy of {
               i ++;
  }
  [Debug] Marking i ++; as copy of i ++;
  [Debug] Marking if (! (i < 10)) {
            break;
  } as copy of if (! (i < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end; as copy of break;
  [Debug] Marking {
          i ++;
  } as copy of {
               i ++;
  }
  [Debug] Marking i ++; as copy of i ++;
  [Debug] Marking if (! (i < 10)) {
            break;
  } as copy of if (! (i < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end; as copy of break;
  [Debug] Marking {
          i ++;
  } as copy of {
               i ++;
  }
  [Debug] Marking i ++; as copy of i ++;
  [Debug] Marking if (! (i < 10)) {
            break;
  } as copy of if (! (i < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end; as copy of break;
  [Debug] Marking {
          i ++;
  } as copy of {
               i ++;
  }
  [Debug] Marking i ++; as copy of i ++;
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:8:5-9:10 with factor 5
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking goto loop_end___0; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking goto loop_end___0; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking goto loop_end___0; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking goto loop_end___0; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking goto loop_end___0; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Info] unrolling loop at 11-unrolled-loop-invariant.c:7:3-11:3 with factor 5
  [Debug] Marking if (! (j < 10)) {
            break;
  } as copy of if (! (j < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end___1; as copy of break;
  [Debug] Marking {
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_0___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_1___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_2___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_3___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_4___0: /* CIL Label */ ;
          while (k < 100) {
            {
            k ++;
            }
          }
          loop_end___0: /* CIL Label */ ;
  } as copy of {
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_0___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_1___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_2___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_3___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_4___0: /* CIL Label */ ;
               while (k < 100) {
                 {
                 k ++;
                 }
               }
               loop_end___0: /* CIL Label */ ;
  }
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_0___2: /* CIL Label */ ; as copy of loop_continue_0___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_1___1: /* CIL Label */ ; as copy of loop_continue_1___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_2___1: /* CIL Label */ ; as copy of loop_continue_2___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_3___1: /* CIL Label */ ; as copy of loop_continue_3___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_4___1: /* CIL Label */ ; as copy of loop_continue_4___0: /* CIL Label */ ;
  [Debug] Marking while (k < 100) {
            {
            k ++;
            }
  } as copy of while (k < 100) {
                 {
                 k ++;
                 }
  }
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking break; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_end___2: /* CIL Label */ ; as copy of loop_end___0: /* CIL Label */ ;
  [Debug] Marking {
          j ++;
  } as copy of {
               j ++;
  }
  [Debug] Marking j ++; as copy of j ++;
  [Debug] Marking if (! (j < 10)) {
            break;
  } as copy of if (! (j < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end___1; as copy of break;
  [Debug] Marking {
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_0___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_1___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_2___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_3___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_4___0: /* CIL Label */ ;
          while (k < 100) {
            {
            k ++;
            }
          }
          loop_end___0: /* CIL Label */ ;
  } as copy of {
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_0___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_1___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_2___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_3___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_4___0: /* CIL Label */ ;
               while (k < 100) {
                 {
                 k ++;
                 }
               }
               loop_end___0: /* CIL Label */ ;
  }
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_0___3: /* CIL Label */ ; as copy of loop_continue_0___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_1___3: /* CIL Label */ ; as copy of loop_continue_1___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_2___2: /* CIL Label */ ; as copy of loop_continue_2___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_3___2: /* CIL Label */ ; as copy of loop_continue_3___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_4___2: /* CIL Label */ ; as copy of loop_continue_4___0: /* CIL Label */ ;
  [Debug] Marking while (k < 100) {
            {
            k ++;
            }
  } as copy of while (k < 100) {
                 {
                 k ++;
                 }
  }
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking break; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_end___3: /* CIL Label */ ; as copy of loop_end___0: /* CIL Label */ ;
  [Debug] Marking {
          j ++;
  } as copy of {
               j ++;
  }
  [Debug] Marking j ++; as copy of j ++;
  [Debug] Marking if (! (j < 10)) {
            break;
  } as copy of if (! (j < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end___1; as copy of break;
  [Debug] Marking {
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_0___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_1___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_2___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_3___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_4___0: /* CIL Label */ ;
          while (k < 100) {
            {
            k ++;
            }
          }
          loop_end___0: /* CIL Label */ ;
  } as copy of {
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_0___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_1___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_2___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_3___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_4___0: /* CIL Label */ ;
               while (k < 100) {
                 {
                 k ++;
                 }
               }
               loop_end___0: /* CIL Label */ ;
  }
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_0___4: /* CIL Label */ ; as copy of loop_continue_0___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_1___4: /* CIL Label */ ; as copy of loop_continue_1___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_2___4: /* CIL Label */ ; as copy of loop_continue_2___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_3___3: /* CIL Label */ ; as copy of loop_continue_3___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_4___3: /* CIL Label */ ; as copy of loop_continue_4___0: /* CIL Label */ ;
  [Debug] Marking while (k < 100) {
            {
            k ++;
            }
  } as copy of while (k < 100) {
                 {
                 k ++;
                 }
  }
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking break; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_end___4: /* CIL Label */ ; as copy of loop_end___0: /* CIL Label */ ;
  [Debug] Marking {
          j ++;
  } as copy of {
               j ++;
  }
  [Debug] Marking j ++; as copy of j ++;
  [Debug] Marking if (! (j < 10)) {
            break;
  } as copy of if (! (j < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end___1; as copy of break;
  [Debug] Marking {
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_0___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_1___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_2___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_3___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_4___0: /* CIL Label */ ;
          while (k < 100) {
            {
            k ++;
            }
          }
          loop_end___0: /* CIL Label */ ;
  } as copy of {
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_0___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_1___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_2___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_3___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_4___0: /* CIL Label */ ;
               while (k < 100) {
                 {
                 k ++;
                 }
               }
               loop_end___0: /* CIL Label */ ;
  }
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_0___5: /* CIL Label */ ; as copy of loop_continue_0___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_1___5: /* CIL Label */ ; as copy of loop_continue_1___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_2___5: /* CIL Label */ ; as copy of loop_continue_2___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_3___5: /* CIL Label */ ; as copy of loop_continue_3___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_4___4: /* CIL Label */ ; as copy of loop_continue_4___0: /* CIL Label */ ;
  [Debug] Marking while (k < 100) {
            {
            k ++;
            }
  } as copy of while (k < 100) {
                 {
                 k ++;
                 }
  }
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking break; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_end___5: /* CIL Label */ ; as copy of loop_end___0: /* CIL Label */ ;
  [Debug] Marking {
          j ++;
  } as copy of {
               j ++;
  }
  [Debug] Marking j ++; as copy of j ++;
  [Debug] Marking if (! (j < 10)) {
            break;
  } as copy of if (! (j < 10)) {
                 break;
  }
  [Debug] Marking goto loop_end___1; as copy of break;
  [Debug] Marking {
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_0___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_1___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_2___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_3___0: /* CIL Label */ ;
          if (! (k < 100)) {
            goto loop_end___0;
          }
          {
          k ++;
          }
          loop_continue_4___0: /* CIL Label */ ;
          while (k < 100) {
            {
            k ++;
            }
          }
          loop_end___0: /* CIL Label */ ;
  } as copy of {
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_0___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_1___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_2___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_3___0: /* CIL Label */ ;
               if (! (k < 100)) {
                 goto loop_end___0;
               }
               {
               k ++;
               }
               loop_continue_4___0: /* CIL Label */ ;
               while (k < 100) {
                 {
                 k ++;
                 }
               }
               loop_end___0: /* CIL Label */ ;
  }
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_0___6: /* CIL Label */ ; as copy of loop_continue_0___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_1___6: /* CIL Label */ ; as copy of loop_continue_1___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_2___6: /* CIL Label */ ; as copy of loop_continue_2___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_3___6: /* CIL Label */ ; as copy of loop_continue_3___0: /* CIL Label */ ;
  [Debug] Marking if (! (k < 100)) {
            goto loop_end___0;
  } as copy of if (! (k < 100)) {
                 goto loop_end___0;
  }
  [Debug] Marking goto loop_end___0; as copy of goto loop_end___0;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_continue_4___6: /* CIL Label */ ; as copy of loop_continue_4___0: /* CIL Label */ ;
  [Debug] Marking while (k < 100) {
            {
            k ++;
            }
  } as copy of while (k < 100) {
                 {
                 k ++;
                 }
  }
  [Debug] Marking if (! (k < 100)) {
            break;
  } as copy of if (! (k < 100)) {
                 break;
  }
  [Debug] Marking break; as copy of break;
  [Debug] Marking {
          k ++;
  } as copy of {
               k ++;
  }
  [Debug] Marking k ++; as copy of k ++;
  [Debug] Marking loop_end___6: /* CIL Label */ ; as copy of loop_end___0: /* CIL Label */ ;
  [Debug] Marking {
          j ++;
  } as copy of {
               j ++;
  }
  [Debug] Marking j ++; as copy of j ++;
  [Debug] And now...  the Goblin!
  [Debug] Startfuns: [main]
  Exitfuns: []
  Otherfuns: []
  [Debug] Activated analyses: expRelation, base, threadid, threadflag, threadreturn, escape, mutexEvents, mutex, access, race, mallocWrapper, mhp, assert
  [Debug] Activated transformations: 
  [Debug] Generating the control flow graph.
  [Debug] cfgF (bindings=102 buckets=128 max_length=3 histo=60,37,28,3 load=0.796875), cfgB (bindings=102 buckets=128 max_length=3 histo=59,40,25,4 load=0.796875)
  [Debug] Initializing 0 globals.
  [Debug] Executing 1 assigns.
  [Debug] Solving the constraint system with td3. Solver statistics are shown every 10s or by signal sigusr1.
  
  [Debug] Unstable solver start vars in 1. phase:
  [Debug] 	L:call of main (297) on 11-unrolled-loop-invariant.c:1:1-13:1
  
  [Debug] Data after solve completed:
  [Debug] |rho|=126
  [Debug] |stable|=126
  [Debug] |infl|=126
  [Debug] |wpoint|=0
  [Debug] |sides|=24
  [Debug] |side_dep|=0
  [Debug] |side_infl|=0
  [Debug] |var_messages|=0
  [Debug] |rho_write|=0
  [Debug] |dep|=101
  [Debug] Postsolving
  [Debug] Pruning result
  [Debug] Data after postsolve:
  [Debug] |rho|=127
  [Debug] |stable|=127
  [Debug] |infl|=126
  [Debug] |wpoint|=0
  [Debug] |sides|=24
  [Debug] |side_dep|=25
  [Debug] |side_infl|=33
  [Debug] |var_messages|=0
  [Debug] |rho_write|=32
  [Debug] |dep|=101
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
  [Debug] node 31 "i < 10" is not a copy
  [Debug] node 23 "i < 10" is copy of if (! (i < 10)) {
                                goto while_break;
  } (31)
  [Debug] node 18 "i < 10" is copy of if (! (i < 10)) {
                                goto while_break;
  } (31)
  [Debug] node 13 "i < 10" is copy of if (! (i < 10)) {
                                goto while_break;
  } (31)
  [Debug] node 8 "i < 10" is copy of if (! (i < 10)) {
                               goto while_break;
  } (31)
  [Debug] node 3 "i < 10" is copy of if (! (i < 10)) {
                               goto while_break;
  } (31)
  [Debug] node 124 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 252 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 247 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 112 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 104 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 232 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 99 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 224 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 94 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 219 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 89 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 214 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 84 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 209 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 204 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 72 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 64 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 192 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 59 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 184 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 54 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 179 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 49 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 174 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 44 "k < 100" is copy of if (! (k < 100)) {
                                 goto while_break___6;
  } (275)
  [Debug] node 169 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 164 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 152 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 275 "k < 100" is not a copy
  [Debug] node 144 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 139 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 267 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 134 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 262 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 129 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 257 "k < 100" is copy of if (! (k < 100)) {
                                  goto while_break___6;
  } (275)
  [Debug] node 121 "j < 10" is copy of if (! (j < 10)) {
                                 goto while_break___5;
  } (244)
  [Debug] node 244 "j < 10" is not a copy
  [Debug] node 81 "j < 10" is copy of if (! (j < 10)) {
                                goto while_break___5;
  } (244)
  [Debug] node 201 "j < 10" is copy of if (! (j < 10)) {
                                 goto while_break___5;
  } (244)
  [Debug] node 41 "j < 10" is copy of if (! (j < 10)) {
                                goto while_break___5;
  } (244)
  [Debug] node 161 "j < 10" is copy of if (! (j < 10)) {
                                 goto while_break___5;
  } (244)
  [Info][Witness] witness generation summary:
    total generation entries: 19

  $ cat witness.yml | grep -A 1 'value:'
        value: i == 10
        format: c_expression
  --
        value: i == 10
        format: c_expression
  --
        value: j == 10
        format: c_expression
  --
        value: k == 100
        format: c_expression
  --
        value: i == 10
        format: c_expression
  --
        value: k == 100
        format: c_expression
  --
        value: ((((j == 1 || j == 4) || j == 0) || j == 3) || j == 2) || (5 <= j &&
          j <= 9)
  --
        value: (((((5 <= i && i <= 9) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
  --
        value: i == 10
        format: c_expression
  --
        value: j == 0
        format: c_expression
  --
        value: (((((5 <= k && k <= 99) || k == 4) || k == 3) || k == 2) || k == 1) ||
          k == 0
  --
        value: i == 10
        format: c_expression
  --
        value: j == 0
        format: c_expression
  --
        value: (((((5 <= i && i <= 10) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
  --
        value: i == 10
        format: c_expression
  --
        value: (((((((k == 100 && (((j == 2 || (5 <= j && j <= 9)) || j == 1) || j ==
          4)) || ((5 <= k && k <= 100) && j == 0)) || (j == 0 && k == 4)) || (j == 0
  --
        value: i == 10
        format: c_expression
  --
        value: ((k == 100 && (((j == 2 || (5 <= j && j <= 10)) || j == 1) || j == 4))
          || (j == 0 && k == 0)) || (j == 3 && k == 100)


