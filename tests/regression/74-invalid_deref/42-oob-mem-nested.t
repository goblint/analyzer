  $ goblint --set ana.activated[+] memOutOfBounds --enable ana.int.interval 42-oob-mem-nested.c
  [Warning] The memOutOfBounds analysis enables cil.addNestedScopeAttr.
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:10:3-10:26)
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:12:3-12:26)
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:14:3-14:26)
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:16:3-16:26)
  [Warning][Imprecise][Program] Trying to read an index, but was not given an array (0) (42-oob-mem-nested.c:17:3-17:19)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare size of pointer (4) (in bytes) with offset by ((Unknown int([-63,63]),[-8589934592,8589934588])) (in bytes). Memory out-of-bounds access might occur (42-oob-mem-nested.c:17:3-17:19)
  [Warning][Assert] Assertion "y == 0" is unknown. (42-oob-mem-nested.c:18:3-18:26)
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:20:3-20:26)
  [Warning][Imprecise][Program] Trying to read an index, but was not given an array (0) (42-oob-mem-nested.c:21:3-21:23)
  [Success][Assert] Assertion "y == 0" will succeed (42-oob-mem-nested.c:22:3-22:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 18
    dead: 0
    total lines: 18
