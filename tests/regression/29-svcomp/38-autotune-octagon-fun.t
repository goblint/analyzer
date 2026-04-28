Should not annotate functions for octagon.

  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[*] octagonAnalysis 38-autotune-octagon-fun.c
  [Info] Enabled octagon domain.
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5

  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[*] octagonVars 38-autotune-octagon-fun.c
  [Info] Enabled octagon domain.
  [Info] Restricted octagon analysis to following tracked variables:
  [Info] i, count, tmp, count, i, j, i___0, j___0, k, size, r
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
