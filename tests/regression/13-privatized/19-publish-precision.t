  $ goblint --enable dbg.print_protection --enable warn.deterministic 19-publish-precision.c
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (19-publish-precision.c:30:3-30:30)
  [Warning][Assert] Assertion "glob1 == 5" is unknown. (19-publish-precision.c:31:3-31:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (19-publish-precision.c:17:3-17:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (19-publish-precision.c:27:3-27:30)
  [Info][Race] Mutex mutex1 read-write protects 0 variable(s): {}
  [Info][Race] Mutex mutex2 read-write protects 1 variable(s): {glob1}
  [Info][Race] Variable glob1 read-write protected by 1 mutex(es): {mutex2}
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
    live: 20
    dead: 0
    total lines: 20
