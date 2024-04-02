  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled 10-unrolled-loop-invariant.c --set dbg.level debug
  [Debug] 2024-04-02 15:10:01
  [Debug] 'goblint' '--set' 'lib.activated' '[]' '--set' 'exp.unrolling-factor' '5' '--enable' 'ana.int.interval' '--enable' 'witness.yaml.enabled' '10-unrolled-loop-invariant.c' '--set' 'dbg.level' 'debug'
  [Debug] Custom include dirs:
  [Debug]   1. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/include (exists=true)
  [Debug]   2. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/runtime/include (exists=true)
  [Debug]   3. /home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/src (exists=true)
  [Debug] Preprocessing files.
  [Debug] Preprocessor cpp: is_bad=false
  [Debug] 'cpp' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/include' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/runtime/include' '-I' '/home/simmo/dev/goblint/sv-comp/goblint/_build/install/default/share/goblint/lib/stub/src' '10-unrolled-loop-invariant.c' '-o' '.goblint/preprocessed/10-unrolled-loop-invariant.i'
  [Debug] Parsing files.
  Frontc is parsing .goblint/preprocessed/10-unrolled-loop-invariant.i
  Converting CABS->CIL
  [Debug] Constructors: 
  [Debug] Adding constructors to: main
  [Info] unrolling loop at 10-unrolled-loop-invariant.c:3:3-4:8 with factor 5
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
  [Debug] And now...  the Goblin!
  [Debug] Startfuns: [main]
  Exitfuns: []
  Otherfuns: []
  [Debug] Activated analyses: expRelation, base, threadid, threadflag, threadreturn, escape, mutexEvents, mutex, access, race, mallocWrapper, mhp, assert
  [Debug] Activated transformations: 
  [Debug] Generating the control flow graph.
  [Debug] cfgF (bindings=16 buckets=128 max_length=1 histo=112,16 load=0.125000), cfgB (bindings=16 buckets=128 max_length=1 histo=112,16 load=0.125000)
  [Debug] Initializing 0 globals.
  [Debug] Executing 1 assigns.
  [Debug] Solving the constraint system with td3. Solver statistics are shown every 10s or by signal sigusr1.
  
  [Debug] Unstable solver start vars in 1. phase:
  [Debug] 	L:call of main (297) on 10-unrolled-loop-invariant.c:1:1-6:1
  
  [Debug] Data after solve completed:
  [Debug] |rho|=23
  [Debug] |stable|=23
  [Debug] |infl|=23
  [Debug] |wpoint|=0
  [Debug] |sides|=7
  [Debug] |side_dep|=0
  [Debug] |side_infl|=0
  [Debug] |var_messages|=0
  [Debug] |rho_write|=0
  [Debug] |dep|=15
  [Debug] Postsolving
  [Debug] Pruning result
  [Debug] Data after postsolve:
  [Debug] |rho|=24
  [Debug] |stable|=24
  [Debug] |infl|=23
  [Debug] |wpoint|=0
  [Debug] |sides|=7
  [Debug] |side_dep|=8
  [Debug] |side_infl|=9
  [Debug] |var_messages|=0
  [Debug] |rho_write|=8
  [Debug] |dep|=15
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Debug] node 31 "i < 10" is not a copy
  [Debug] node 23 "i < 10" is not a copy
  [Debug] node 18 "i < 10" is not a copy
  [Debug] node 13 "i < 10" is not a copy
  [Debug] node 8 "i < 10" is not a copy
  [Debug] node 3 "i < 10" is not a copy
  [Info][Witness] witness generation summary:
    total generation entries: 4

  $ cat witness.yml | grep -A 1 'value:'
        value: (((((5 <= i && i <= 9) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
  --
        value: i == 10
        format: c_expression
  --
        value: (((((5 <= i && i <= 10) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0


