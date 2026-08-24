  $ goblint --set ana.activated[+] memOutOfBounds --enable ana.int.interval 44-oob-speculate-overflow.c
  [Warning] The memOutOfBounds analysis enables cil.addNestedScopeAttr.
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (44-oob-speculate-overflow.c:13:7-13:23)
  [Warning][Unknown] Size of lval dereference expression *(buffer + i) + j is top. Out-of-bounds memory access may occur (44-oob-speculate-overflow.c:13:7-13:23)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Size of pointer *(buffer + i) + j is top. Memory out-of-bounds access might occur due to pointer arithmetic (44-oob-speculate-overflow.c:13:7-13:23)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in internal cast (44-oob-speculate-overflow.c:21:3-21:22)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare size of dest (42) with address offset (1) count (⊤) in function memset. Memory out-of-bounds access may occur (44-oob-speculate-overflow.c:21:3-21:22)
  [Warning][Unknown] size of error: Array is so long that its size can't be represented with an OCaml int. (44-oob-speculate-overflow.c:30:13-30:25)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare size of lval dereference expression (⊤) (in bytes) with offset by (0) (in bytes). Memory out-of-bounds access might occur (44-oob-speculate-overflow.c:31:3-31:13)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare size of pointer (⊤) (in bytes) with offset by (0) (in bytes). Memory out-of-bounds access might occur (44-oob-speculate-overflow.c:31:3-31:13)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Could not compare size of lval dereference expression (⊤) (in bytes) with offset by (0) (in bytes). Memory out-of-bounds access might occur (44-oob-speculate-overflow.c:32:3-32:19)
  [Warning][Behavior > Undefined > MemoryOutOfBoundsAccess][CWE-823] Size of pointer is ⊤ (in bytes). It is offset by -8 (in bytes) due to pointer arithmetic. Memory out-of-bounds access must occur (44-oob-speculate-overflow.c:32:3-32:19)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 24
    dead: 0
    total lines: 24
