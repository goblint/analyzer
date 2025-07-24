  $ goblint --enable dbg.print_protection --enable warn.deterministic 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Race] Mutex mutex1 read-write protects 1 variable(s): {glob1}
  [Info][Race] Mutex mutex2 read-write protects 0 variable(s): {}
  [Info][Race] Variable glob1 read-write protected by 1 mutex(es): {mutex1}
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
  [Info][Race] Mutex read-write protection summary:
    Number of mutexes: 2
    Max number variables of protected by a mutex: 1
    Total number of protected variables (including duplicates): 1
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
