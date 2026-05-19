  $ goblint --set ana.activated[+] useAfterFree 18-simple-uaf.c 2>&1 | sed -r 's/sid:[0-9]+/sid:$SID/'
  [Warning][Behavior > Undefined > UseAfterFree][CWE-416] lval ((alloc@sid:$SID@tid:[main])) points to a maybe freed memory region (18-simple-uaf.c:11:5-11:14)
  [Warning][Behavior > Undefined > DoubleFree][CWE-415] lval ((alloc@sid:$SID@tid:[main])) points to a maybe freed memory region (18-simple-uaf.c:12:5-12:14)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 7
    dead: 0
    total lines: 7
