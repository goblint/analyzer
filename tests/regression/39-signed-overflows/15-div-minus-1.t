  $ goblint --enable warn.deterministic --enable ana.int.interval 15-div-minus-1.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:5:9-5:26)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:7:5-7:16)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:9:9-9:20)
  [Warning][Integer > DivByZero][CWE-369] Second argument of division might be zero (15-div-minus-1.c:7:5-7:16)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

  $ goblint --enable warn.deterministic --enable ana.int.interval_set 15-div-minus-1.c
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:5:9-5:26)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:7:5-7:16)
  [Warning][Integer > Overflow][CWE-190] Signed integer overflow in / (15-div-minus-1.c:9:9-9:20)
  [Warning][Integer > DivByZero][CWE-369] Second argument of division might be zero (15-div-minus-1.c:7:5-7:16)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 6
    dead: 0
    total lines: 6

