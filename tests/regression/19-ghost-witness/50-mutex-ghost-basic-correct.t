Check that values of non-phase ghost variables are carried over when phase
changes are propagated. Otherwise, the branch-local update in the new phase
would unsoundly confirm the invariant.

  $ goblint --set dbg.level warning --disable warn.race --disable warn.integer --enable warn.deterministic --enable ana.sv-comp.functions --set witness.yaml.validate 50-mutex-ghost-basic-correct.yml --set ana.activated[+] mutexGhost --set ana.path_sens[+] threadflag --set ana.activated[+] threadJoins --set lib.activated[+] sv-comp --enable ana.int.interval --set ana.base.privatization mutex-meet-tid --set colors never 50-mutex-ghost-basic-correct.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 27
    dead: 0
    total lines: 27
  [Warning][Unknown] mutex1 is definite
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:9:5-9:32)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:9:5-9:32)
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:12:5-12:34)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:12:5-12:34)
  [Warning][Unknown] lock mutex2, ghost ghost_2 (50-mutex-ghost-basic-correct.c:13:5-13:32)
  [Warning][Unknown] lock mutex2, ghost ghost_3 (50-mutex-ghost-basic-correct.c:13:5-13:32)
  [Warning][Unknown] mutex2 is definite (50-mutex-ghost-basic-correct.c:13:5-13:32)
  [Warning][Unknown] lock mutex2, ghost ghost_2 (50-mutex-ghost-basic-correct.c:14:5-14:34)
  [Warning][Unknown] lock mutex2, ghost ghost_3 (50-mutex-ghost-basic-correct.c:14:5-14:34)
  [Warning][Unknown] mutex2 is definite (50-mutex-ghost-basic-correct.c:14:5-14:34)
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:20:5-20:32)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:20:5-20:32)
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:23:5-23:34)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:23:5-23:34)
  [Warning][Unknown] lock mutex2, ghost ghost_2 (50-mutex-ghost-basic-correct.c:24:5-24:32)
  [Warning][Unknown] lock mutex2, ghost ghost_3 (50-mutex-ghost-basic-correct.c:24:5-24:32)
  [Warning][Unknown] mutex2 is definite (50-mutex-ghost-basic-correct.c:24:5-24:32)
  [Warning][Unknown] lock mutex2, ghost ghost_2 (50-mutex-ghost-basic-correct.c:26:5-26:34)
  [Warning][Unknown] lock mutex2, ghost ghost_3 (50-mutex-ghost-basic-correct.c:26:5-26:34)
  [Warning][Unknown] mutex2 is definite (50-mutex-ghost-basic-correct.c:26:5-26:34)
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:38:5-38:32)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:38:5-38:32)
  [Warning][Unknown] lock mutex1, ghost ghost_1 (50-mutex-ghost-basic-correct.c:40:5-40:34)
  [Warning][Unknown] mutex1 is definite (50-mutex-ghost-basic-correct.c:40:5-40:34)
  [Info][Witness] mutexGhost: global ghost_1 is only used to mark the boundary of all of the critical sections protected by mutex mutex1
  [Info][Witness] mutexGhost: global ghost_2 is only used to mark the boundary of all of the critical sections protected by mutex mutex2
  [Info][Witness] mutexGhost: global ghost_3 is only used to mark the boundary of all of the critical sections protected by mutex mutex2
  [Info][Witness] witness validation summary:
    confirmed: 1
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Success][Witness] invariant confirmed: ghost_1 == 1 || g1 == 0 (50-mutex-ghost-basic-correct.c:41:3)
