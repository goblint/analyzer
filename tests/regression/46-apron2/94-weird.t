Check that the invariant (long long )f + 2147483648LL >= (long long )e is not confirmed, as it presumes information about f and e which are supposed to be invalidated
  $ goblint --set dbg.level warning --disable warn.imprecise --disable warn.race --set ana.activated[+] apron --enable witness.invariant.after-lock --disable witness.invariant.other --disable witness.invariant.loop-head --disable sem.unknown_function.invalidate.globals --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.entry-types[*] invariant_set --set witness.yaml.validate 94-weird.yml 94-weird.c
  [Error][Imprecise][Unsound] Function definition missing for b (94-weird.c:10:3-10:35)
  [Error][Imprecise][Unsound] Created a thread from unknown function b (94-weird.c:10:3-10:35)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Warning][Witness] invariant unconfirmed: (long long )f + 2147483648LL >= (long long )e (94-weird.c:11:3)
  [Info][Witness] witness validation summary:
    confirmed: 0
    unconfirmed: 1
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 1
  [Error][Imprecise][Unsound] Function definition missing
