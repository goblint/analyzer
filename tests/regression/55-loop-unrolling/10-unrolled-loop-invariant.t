  $ goblint --set lib.activated '[]' --set exp.unrolling-factor 5 --enable ana.int.interval --enable witness.yaml.enabled 10-unrolled-loop-invariant.c
  [Info] unrolling loop at 10-unrolled-loop-invariant.c:3:3-4:8 with factor 5
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 5
    dead: 0
    total lines: 5
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Warning][Deadcode][CWE-571] condition 'i < 10' (possibly inserted by CIL) is always true (10-unrolled-loop-invariant.c:3:10-3:16)
  [Info][Witness] witness generation summary:
    total generation entries: 4

  $ cat witness.yml | grep -A 1 'value:'
        value: (((((5 <= i && i <= 9) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0
  --
        value: i == 10
        format: c_expression
  --
        value: (((((5 <= i && i <= 10) || i == 4) || i == 3) || i == 2) || i == 1) ||
          i == 0


