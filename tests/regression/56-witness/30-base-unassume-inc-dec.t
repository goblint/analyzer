# Vojdani's analysis

## From scratch

Assertions not proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani 30-base-unassume-inc-dec.c
  [Warning][Assert] Assertion "-10 <= g" is unknown. (30-base-unassume-inc-dec.c:33:3-33:28)
  [Warning][Assert] Assertion "g <= 10" is unknown. (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Easiest to run again to get evals.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 38    evals = 73    narrow_reuses = 3

## Unassume after lock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:9:5-9:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:19:5-19:27)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals less than from scratch.
TODO: Should not be according to Simmo's PhD thesis?
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 38    evals = 58    narrow_reuses = 3

## Unassume after lock and before unlock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec2.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:9:5-9:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:11:7-11:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:10:9-10:15)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:19:5-19:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:21:7-21:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:20:9-20:16)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals less than from scratch.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec2.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 38    evals = 49    narrow_reuses = 2

## Unassume before unlock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec3.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:11:7-11:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:10:9-10:15)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:21:7-21:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:20:9-20:16)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals less than from scratch.
TODO: Why more than unassume after lock and before unlock?
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization vojdani --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec3.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 38    evals = 54    narrow_reuses = 2


# Protection-Based Reading

## From scratch

Assertions not proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection 30-base-unassume-inc-dec.c
  [Warning][Assert] Assertion "-10 <= g" is unknown. (30-base-unassume-inc-dec.c:33:3-33:28)
  [Warning][Assert] Assertion "g <= 10" is unknown. (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 39    evals = 53    narrow_reuses = 3

## Unassume after lock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:9:5-9:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:19:5-19:27)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals not less than from scratch.
Fits old TODO from Simmo's PhD thesis: "No improvement in number of steps using W set. Without it the improvement was 44 → 26."
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 39    evals = 54    narrow_reuses = 3

## Unassume after lock and before unlock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec2.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:9:5-9:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:11:7-11:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:10:9-10:15)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:19:5-19:27)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:21:7-21:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:20:9-20:16)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals less than from scratch.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec2.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 39    evals = 41    narrow_reuses = 2

## Unassume before unlock

Assertions proven.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec3.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:11:7-11:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:10:9-10:15)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:21:7-21:10)
  [Info][Witness] unassume invariant: -10 <= g && g <= 10 (30-base-unassume-inc-dec.c:20:9-20:16)
  [Success][Assert] Assertion "-10 <= g" will succeed (30-base-unassume-inc-dec.c:33:3-33:28)
  [Success][Assert] Assertion "g <= 10" will succeed (30-base-unassume-inc-dec.c:34:3-34:27)
  [Warning][Deadcode] Function 't_fun' has dead code:
    on line 14 (30-base-unassume-inc-dec.c:14-14)
  [Warning][Deadcode] Function 't_fun2' has dead code:
    on line 24 (30-base-unassume-inc-dec.c:24-24)
  [Warning][Deadcode] Logical lines of code (LLoC) summary:
    live: 20
    dead: 2
    total lines: 22
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:8:10-8:11)
  [Warning][Deadcode][CWE-571] condition '1' (possibly inserted by CIL) is always true (30-base-unassume-inc-dec.c:18:10-18:11)
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Evals less than from scratch.
Evals same as unassume after lock and before unlock, matches Simmo's PhD thesis.
  $ goblint --enable ana.int.interval --set solvers.td3.side_widen always --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.unassume 30-base-unassume-inc-dec3.yml --enable ana.widen.tokens 30-base-unassume-inc-dec.c -v 2>&1 | grep 'evals'
  [Info] vars = 39    evals = 41    narrow_reuses = 2
