Analysis of single-threaded program with escape deactivated.
Also earlyglobs enabled, otherwise MayEscape queries are not used.
Earlyglobs should not matter for local variable.


Assertions should be unknown.

  $ goblint --set ana.activated[-] escape --enable exp.earlyglobs 43-no-escape.c
  [Warning] Without thread escape analysis, every local variable whose address is taken is considered escaped, i.e., global! (Except when exp.single-threaded is enabled.)
  [Warning][Assert] Assertion "x == 0" is unknown. (43-no-escape.c:7:3-7:26)
  [Warning][Assert] Assertion "x == 1" is unknown. (43-no-escape.c:9:3-9:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7


With single-threaded mode forced.
Assertions should succeed.

  $ goblint --set ana.activated[-] escape --enable exp.earlyglobs --enable exp.single-threaded 43-no-escape.c
  [Success][Assert] Assertion "x == 0" will succeed (43-no-escape.c:7:3-7:26)
  [Success][Assert] Assertion "x == 1" will succeed (43-no-escape.c:9:3-9:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7

