Analysis of single-threaded program with threadflag deactivated.
Also no threadid which can tell being single-threaded.


With earlyglobs disabled: no threadflag means the same behavior.
Assertions should be unknown, race should be present.

  $ goblint --set ana.activated[-] threadflag --set ana.activated[-] threadid --disable exp.earlyglobs 42-no-threadflag.c
  [Warning][Assert] Assertion "g == 0" is unknown. (42-no-threadflag.c:7:3-7:26)
  [Warning][Assert] Assertion "g == 1" is unknown. (42-no-threadflag.c:9:3-9:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Warning][Race] Memory location g (race with conf. 110): (42-no-threadflag.c:4:5-4:6)
    write with mhp:{created={Unknown thread id}} (conf. 110)  (exp: & g) (42-no-threadflag.c:8:3-8:8)
    read with mhp:{created={Unknown thread id}} (conf. 110)  (exp: & g) (42-no-threadflag.c:7:3-7:26)
    read with mhp:{created={Unknown thread id}} (conf. 110)  (exp: & g) (42-no-threadflag.c:9:3-9:26)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1


With single-threaded mode forced.
Assertions should succeed, race should not be present.

  $ goblint --set ana.activated[-] threadflag --set ana.activated[-] threadid --disable exp.earlyglobs --enable exp.single-threaded 42-no-threadflag.c
  [Success][Assert] Assertion "g == 0" will succeed (42-no-threadflag.c:7:3-7:26)
  [Success][Assert] Assertion "g == 1" will succeed (42-no-threadflag.c:9:3-9:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5


With single-threaded mode forced, but earlyglobs enabled.
Assertions should be unknown, race should not be present.

  $ goblint --set ana.activated[-] threadflag --set ana.activated[-] threadid --enable exp.earlyglobs --enable exp.single-threaded 42-no-threadflag.c
  [Warning][Assert] Assertion "g == 0" is unknown. (42-no-threadflag.c:7:3-7:26)
  [Warning][Assert] Assertion "g == 1" is unknown. (42-no-threadflag.c:9:3-9:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5

