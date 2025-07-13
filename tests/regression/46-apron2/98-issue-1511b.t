  $ goblint --disable warn.unsound --disable warn.imprecise --disable sem.unknown_function.invalidate.globals --enable warn.deterministic --disable ana.dead-code.lines --disable warn.race --disable warn.behavior --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid --enable witness.yaml.enabled --disable witness.invariant.other --disable witness.invariant.loop-head 98-issue-1511b.c --set witness.yaml.path 98-issue-1511b.yml
  [Info][Witness] witness generation summary:
    location invariants: 26
    loop invariants: 0
    flow-insensitive invariants: 0
    total generation entries: 1

  $ goblint --disable warn.unsound --disable warn.imprecise --disable sem.unknown_function.invalidate.globals --enable warn.deterministic --disable ana.dead-code.lines --disable warn.race --disable warn.behavior --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 98-issue-1511b.yml 98-issue-1511b.c
  [Info][Witness] witness validation summary:
    confirmed: 26
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 26
  [Success][Witness] invariant confirmed: (1LL + (long long )j) - (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (1LL - (long long )j) - (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483646LL + (long long )j) + (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483646LL - (long long )d) + (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483646LL - (long long )j) + (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483647LL + (long long )d) + (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) + (long long )j >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) - (long long )j >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) + (long long )j >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) - (long long )j >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483648LL - (long long )d) - (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (2147483649LL + (long long )d) - (long long )k >= 0LL (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: j == 0 (98-issue-1511b.c:22.5)
  [Success][Witness] invariant confirmed: (1LL + (long long )j) - (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (1LL - (long long )j) - (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483646LL + (long long )j) + (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483646LL - (long long )d) + (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483646LL - (long long )j) + (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483647LL + (long long )d) + (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) + (long long )j >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483647LL - (long long )d) - (long long )j >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) + (long long )j >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483648LL + (long long )d) - (long long )j >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483648LL - (long long )d) - (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: (2147483649LL + (long long )d) - (long long )k >= 0LL (98-issue-1511b.c:27.5)
  [Success][Witness] invariant confirmed: j == 0 (98-issue-1511b.c:27.5)
