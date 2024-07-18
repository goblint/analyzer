  $ goblint --set ana.activated[+] memOutOfBounds --enable ana.int.interval 01-oob-heap-simple.c 2>&1 | tee default-output.txt
  [Warning] The memOutOfBounds analysis enables cil.addNestedScopeAttr.
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Size of pointer in expression ptr + 10 is 5 (in bytes). It is offset by 10 (in bytes). Memory out-of-bounds access must occur (01-oob-heap-simple.c:10:5-10:22)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare pointer size (5) with offset (⊤). Memory out-of-bounds access may occur (01-oob-heap-simple.c:11:5-11:21)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 8
    dead: 0
    total lines: 8

  $ goblint --set ana.activated[+] memOutOfBounds --enable ana.int.interval --enable dbg.full-output 01-oob-heap-simple.c > full-output.txt 2>&1

  $ diff default-output.txt full-output.txt
  2,3c2,3
  < [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Size of pointer in expression ptr + 10 is 5 (in bytes). It is offset by 10 (in bytes). Memory out-of-bounds access must occur (01-oob-heap-simple.c:10:5-10:22)
  < [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare pointer size (5) with offset (⊤). Memory out-of-bounds access may occur (01-oob-heap-simple.c:11:5-11:21)
  ---
  > [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Size of pointer in expression ptr + 10 is (5,[5,5]) (in bytes). It is offset by (10,[10,10]) (in bytes). Memory out-of-bounds access must occur (01-oob-heap-simple.c:10:5-10:22)
  > [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare pointer size ((5,[5,5])) with offset ((Unknown int([-63,63]),[-9223372036854775808,9223372036854775807])). Memory out-of-bounds access may occur (01-oob-heap-simple.c:11:5-11:21)
  [1]
