setvbuf hands stdout a heap buffer. The fputs before free writes a live buffer;
the fputs after it writes a freed one. Normalize the allocation statement ID,
which depends on the platform headers.

  $ goblint --set ana.activated[+] useAfterFree 78-freed-stream-buffer.c 2>&1 | sed -E 's/alloc@sid:[0-9]+/alloc@sid:SID/g' | grep UseAfterFree
  [Warning][Behavior > Undefined > UseAfterFree][CWE-416] (alloc@sid:SID@tid:[main]), heap memory, ends here while stdout may hold it as its buffer: a later use of stdout, or the flush and close when the program exits, can access it after its lifetime (78-freed-stream-buffer.c:10:3-10:10)
  [Warning][Behavior > Undefined > UseAfterFree][CWE-416] lval ((alloc@sid:SID@tid:[main])) points to a maybe freed memory region (78-freed-stream-buffer.c:11:3-11:22)
