Miné succeeds:

  $ goblint --set ana.base.privatization mine 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

Miné without thread IDs even succeeds:

  $ goblint --set ana.base.privatization mine-nothread 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

Our Miné with W doesn't succeed with W partitioning due to initialization:

  $ goblint --set ana.base.privatization mine-W 95-mine-W-part-by-S.c
  [Warning][Assert] Assertion "g == 8" is unknown. (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

But the noinit variant succeeds:

  $ goblint --set ana.base.privatization mine-W-noinit 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2



Protection-Based succeeds:

  $ goblint --set ana.base.privatization protection 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

Write-Centered succeeds:

  $ goblint --set ana.base.privatization write 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2

Lock-Centered (with may-V) also succeeds:

  $ goblint --set ana.base.privatization lock 95-mine-W-part-by-S.c
  [Success][Assert] Assertion "g == 8" will succeed (95-mine-W-part-by-S.c:28:3-28:26)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 17
    dead: 0
    total lines: 17
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
