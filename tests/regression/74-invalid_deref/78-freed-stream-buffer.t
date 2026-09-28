setvbuf hands stdout a heap buffer. The fputs before free writes a live buffer;
the fputs after it writes a freed one.

  $ goblint --set ana.activated[+] useAfterFree 78-freed-stream-buffer.c 2>&1 | grep UseAfterFree
  [Warning][Behavior > Undefined > UseAfterFree][CWE-416] lval ((alloc@sid:13@tid:[main])) points to a maybe freed memory region (78-freed-stream-buffer.c:11:3-11:22)
