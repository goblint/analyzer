TODO: Should not annotate functions for octagon.

  $ goblint --enable ana.autotune.enabled --set ana.autotune.activated[*] octagon 38-autotune-octagon-fun.c
  [Info] Enabled octagon domain ONLY for:
  [Info] pthread_create, i, count, tmp, key, a, count, i, j, i___0, j___0, k, size, r
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5

