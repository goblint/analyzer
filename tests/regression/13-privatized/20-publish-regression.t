Vojdani's analysis succeeds:
  $ goblint --set ana.base.privatization vojdani 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Protection-Based Reading fails:
  $ goblint --set ana.base.privatization protection 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Warning][Assert] Assertion "glob1 == 0" is unknown. (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Protection-Based Reading with protection unassume succeeds:
  $ goblint --set ana.base.privatization protection --set ana.activated[+] unassume --set witness.yaml.entry-types[+] protected_by --set witness.yaml.unassume 20-publish-regression.yml 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Info][Witness] mutex unassumed glob1 protected_by: {mutex1} (20-publish-regression.c:30:3-30:40)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1

Protection-Based Reading with extra check from Vojdani's analysis succeeds:
  $ goblint --set ana.base.privatization protection-read 20-publish-regression.c
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:29:3-29:30)
  [Success][Assert] Assertion "glob1 == 5" will succeed (20-publish-regression.c:20:3-20:30)
  [Success][Assert] Assertion "glob1 == 0" will succeed (20-publish-regression.c:32:3-32:30)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Info][Race] Memory locations race summary:
    safe: 1
    vulnerable: 0
    unsafe: 0
    total memory locations: 1
