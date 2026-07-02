Does not succeed without witness

  $ goblint --conf ../../../conf/svcomp26/common.json --conf ../../../conf/svcomp26/verify.json --conf ../../../conf/svcomp26/level04.json --sets ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )" --set ana.base.privatization protection-atomic-ghost --sets exp.architecture 32bit 18-singleton.c --disable witness.yaml.enabled
  [Warning] --sets is deprecated, use --set instead.
  [Warning] --sets is deprecated, use --set instead.
  [Error] Machine definition not available for selected architecture, defaulting to host
  [Info] Enabled congruence domain.
  [Info] Enabled widening thresholds
  [Info] Enabled octagon domain ONLY for:
  [Info] i, count, tmp, count, i, j, i___0, j___0, k, size, r, expression, tmp, expression, __x, __x, __x
  [Info] SV-COMP specification: CHECK( init(main()), LTL(G ! call(reach_error())) )
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:35:3-35:13)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:43:3-43:13)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:73:3-73:48)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (18-singleton.c:73:3-73:48)
  [Error][Assert] Assertion "0" will fail. (18-singleton.c:14:22-14:41)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 32
    dead: 0
    total lines: 32
  SV-COMP result: unknown
  [Info][Race] Memory locations race summary:
    safe: 2
    vulnerable: 0
    unsafe: 0
    total memory locations: 2
  [Error][Unsound] Machine definition not available for selected architecture
