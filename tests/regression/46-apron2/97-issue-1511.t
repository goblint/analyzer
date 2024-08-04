  $ goblint --disable warn.unsound --disable warn.imprecise --disable sem.unknown_function.invalidate.globals --enable warn.deterministic --disable ana.dead-code.lines --disable warn.race --disable warn.behavior --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid --enable witness.yaml.enabled --disable witness.invariant.other --disable witness.invariant.loop-head 97-issue-1511.c --set witness.yaml.path 97-issue-1511.yml
  [Info][Witness] witness generation summary:
    total generation entries: 11
  [Error][Imprecise][Unsound] Function definition missing

  $ goblint --disable warn.unsound --disable warn.imprecise --disable sem.unknown_function.invalidate.globals --enable warn.deterministic --disable ana.dead-code.lines --disable warn.race --disable warn.behavior --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 97-issue-1511.yml 97-issue-1511.c
  [Info][Witness] witness validation summary:
    confirmed: 20
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 20
  [Success][Witness] invariant confirmed: (2147483647LL + (long long )d) - (long long )f >= 0LL (97-issue-1511.c:14:3)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) - (long long )f >= 0LL (97-issue-1511.c:14:3)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) + (long long )f >= 0LL (97-issue-1511.c:14:3)
  [Success][Witness] invariant confirmed: (2147483648LL - (long long )d) + (long long )f >= 0LL (97-issue-1511.c:14:3)
  [Success][Witness] invariant confirmed: d == 0 (97-issue-1511.c:14:3)
  [Success][Witness] invariant confirmed: (2147483647LL + (long long )d) - (long long )f >= 0LL (97-issue-1511.c:17:1)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) - (long long )f >= 0LL (97-issue-1511.c:17:1)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) + (long long )f >= 0LL (97-issue-1511.c:17:1)
  [Success][Witness] invariant confirmed: (2147483648LL - (long long )d) + (long long )f >= 0LL (97-issue-1511.c:17:1)
  [Success][Witness] invariant confirmed: d == 0 (97-issue-1511.c:17:1)
  [Error][Imprecise][Unsound] Function definition missing
