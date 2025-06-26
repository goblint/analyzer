  $ goblint --set ana.activated[+] memLeak --set ana.malloc.unique_address_count 1 32-no-mem-leak-goblint-check.c
  [Error][Assert] Assertion "(unsigned long )ptr == (unsigned long )((int *)0)" will fail. (32-no-mem-leak-goblint-check.c:7.5-7.30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
