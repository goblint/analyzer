Should have no overflow warning (on line 32).
Now has NULL dereference warning there still.

  $ goblint --enable ana.int.interval_set 12-loop-no-overflows.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:34:9-34:550)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:36:9-36:33)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:36:9-36:33)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:22:5-22:45)
  [Warning][Imprecise][Program] Trying to read an index, but was not given an array ({
                                                         next -> {&ldv_global_msg_list}
                                                         prev -> {&ldv_global_msg_list}
                                                       }) (12-loop-no-overflows.c:22:5-22:45)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:22:5-22:45)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:17:5-17:22)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:18:5-18:22)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:37:9-37:26)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:26:5-26:20)
  [Warning][Behavior > Undefined > InvalidMemoryDeallocation][CWE-590] Points-to set for pointer msg->data in function free is top. Potentially invalid memory deallocation may occur (12-loop-no-overflows.c:26:5-26:20)
  [Warning][Behavior > Undefined > InvalidMemoryDeallocation][CWE-590] Points-to set for pointer (void *)msg in function free is top. Potentially invalid memory deallocation may occur (12-loop-no-overflows.c:27:5-27:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:34:444-34:470)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

