The pointer passed to free has lost its target, so it may be the heap buffer
stdout holds: base warns that the buffer may end there.

  $ goblint 34-standard-streams-buffer-unknown-free.c 2>&1 | grep -F 'as its buffer'
  [Warning][Behavior > Undefined > UseAfterFree][CWE-416] (alloc@sid:13@tid:[main]), heap memory, may end here, freed through a pointer whose target is not known, while stdout may hold it as its buffer: a later use of stdout, or the flush and close when the program exits, can access it after its lifetime (34-standard-streams-buffer-unknown-free.c:15:3-15:19)
