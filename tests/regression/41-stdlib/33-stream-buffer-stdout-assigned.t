After assigning stdout, attaching a local buffer must report the stream-buffer
lifetime assumption because the stream is no longer tracked.

  $ goblint --disable warn.imprecise --disable warn.unsound --enable cil.addNestedScopeAttr 33-stream-buffer-stdout-assigned.c 2>&1 | grep -F '[Info][Assumption]'
  [Info][Assumption] libc.stream-buffer-lifetime: a buffer attached to a stream with setvbuf or setbuf stays live until the stream is closed or the program exits (33-stream-buffer-stdout-assigned.c:8:3-8:48)
  [Info][Assumption] Used assumption libc.stream-buffer-lifetime: a buffer attached to a stream with setvbuf or setbuf stays live until the stream is closed or the program exits
