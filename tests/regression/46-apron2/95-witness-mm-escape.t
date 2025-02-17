  $ goblint --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster12 --set witness.yaml.validate 95-witness-mm-escape.yml 95-witness-mm-escape.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
  [Success][Witness] invariant confirmed: 0 <= g (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: 0 <= *b (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: g <= 127 (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: *b <= 127 (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: -8LL + (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: 2147483648LL + (long long )a >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: (2147483638LL + (long long )a) + (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: (2147483637LL - (long long )a) + (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: 10LL - (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: 2147483647LL - (long long )a >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: (2147483658LL + (long long )a) - (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: (2147483657LL - (long long )a) - (long long )g >= 0LL (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: b == & g (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: g != 0 (95-witness-mm-escape.c:19:1)
  [Success][Witness] invariant confirmed: *b != 0 (95-witness-mm-escape.c:19:1)
  [Info][Witness] witness validation summary:
    confirmed: 15
    unconfirmed: 0
    refuted: 0
    error: 0
    unchecked: 0
    unsupported: 0
    disabled: 0
    total validation entries: 15
