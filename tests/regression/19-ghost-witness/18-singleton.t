Does not succeed without witness

  $ goblint --enable warn.deterministic --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --sets ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --sets exp.architecture 64bit 18-singleton.c --disable witness.yaml.enabled
  [Warning] --sets is deprecated, use --set instead.
  [Warning] --sets is deprecated, use --set instead.
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, expression, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  SV-COMP result: unknown
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:34:3-34:13)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:42:3-42:13)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:72:3-72:48)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:72:3-72:48)
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 32
    dead: 0
    total lines: 32
