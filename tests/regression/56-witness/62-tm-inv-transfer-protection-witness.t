Same as regression test, but with manually written witness from Simmo's PhD thesis:

  $ goblint --set solvers.td3.side_widen never --enable ana.int.interval --set ana.base.privatization protection --set "ana.activated[+]" unassume --set witness.yaml.unassume 62-tm-inv-transfer-protection-witness-manual.yml --enable ana.widen.tokens 62-tm-inv-transfer-protection-witness.c
  [Info][Witness] unassume invariant: 40 <= g && g <= 41 (62-tm-inv-transfer-protection-witness.c:26:3-26:25)
  [Success][Assert] Assertion "g >= 40" will succeed (62-tm-inv-transfer-protection-witness.c:27:3-27:27)
  [Warning][Assert] Assertion "g <= 41" is unknown. (62-tm-inv-transfer-protection-witness.c:28:3-28:27)
  [Info][Witness] unassume invariant: 40 <= g && g <= 42 (62-tm-inv-transfer-protection-witness.c:29:3-29:27)
  [Success][Assert] Assertion "g >= 40" will succeed (62-tm-inv-transfer-protection-witness.c:31:3-31:27)
  [Success][Assert] Assertion "g <= 42" will succeed (62-tm-inv-transfer-protection-witness.c:32:3-32:27)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 19
    dead: 0
    total lines: 19
  [Warning][Race] Memory location g (race with conf. 110): (62-tm-inv-transfer-protection-witness.c:5:5-5:11)
    write with [lock:{B}, thread:[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:10:3-10:9)
    write with [lock:{B}, thread:[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:11:3-11:9)
    write with thread:[main, t_fun2@62-tm-inv-transfer-protection-witness.c:24:3-24:42] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:17:3-17:9)
    read with [mhp:{created={[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40], [main, t_fun2@62-tm-inv-transfer-protection-witness.c:24:3-24:42]}}, lock:{B}, thread:[main]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:27:3-27:27)
    read with [mhp:{created={[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40], [main, t_fun2@62-tm-inv-transfer-protection-witness.c:24:3-24:42]}}, lock:{B}, thread:[main]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:28:3-28:27)
    read with [mhp:{created={[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40], [main, t_fun2@62-tm-inv-transfer-protection-witness.c:24:3-24:42]}}, thread:[main]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:31:3-31:27)
    read with [mhp:{created={[main, t_fun@62-tm-inv-transfer-protection-witness.c:23:3-23:40], [main, t_fun2@62-tm-inv-transfer-protection-witness.c:24:3-24:42]}}, thread:[main]] (conf. 110)  (exp: & g) (62-tm-inv-transfer-protection-witness.c:32:3-32:27)
  [Info][Race] Memory locations race summary:
    safe: 0
    vulnerable: 0
    unsafe: 1
    total memory locations: 1

