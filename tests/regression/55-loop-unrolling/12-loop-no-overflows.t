Should have no overflow warning (on line 32).
Now has NULL dereference warning there still.

  $ goblint --enable ana.int.interval_set 12-loop-no-overflows.c
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:32:9-32:550)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:34:9-34:33)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:34:9-34:33)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:20:5-20:45)
  [Warning][Imprecise][Program] Trying to read an index, but was not given an array ({
                                                         next -> {&ldv_global_msg_list}
                                                         prev -> {&ldv_global_msg_list}
                                                       }) (12-loop-no-overflows.c:20:5-20:45)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:20:5-20:45)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:15:5-15:22)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:16:5-16:22)
  [Info][Unsound] Unknown address given as function argument (12-loop-no-overflows.c:35:9-35:26)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:24:5-24:20)
  [Warning][Behavior > Undefined > InvalidMemoryDeallocation][CWE-590] Points-to set for pointer msg->data in function free is top. Potentially invalid memory deallocation may occur (12-loop-no-overflows.c:24:5-24:20)
  [Warning][Behavior > Undefined > InvalidMemoryDeallocation][CWE-590] Points-to set for pointer msg in function free is top. Potentially invalid memory deallocation may occur (12-loop-no-overflows.c:25:5-25:14)
  [Warning][Behavior > Undefined > NullPointerDereference][CWE-476] May dereference NULL pointer (12-loop-no-overflows.c:32:444-32:470)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 16
    dead: 0
    total lines: 16

