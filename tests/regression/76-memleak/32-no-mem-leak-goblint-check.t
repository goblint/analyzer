  $ goblint --set ana.activated[+] memLeak --set ana.malloc.unique_address_count 1 32-no-mem-leak-goblint-check.c
  [Warning][Assert] Assertion "(int )ptr" is unknown. (32-no-mem-leak-goblint-check.c:6:5-6:25)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
