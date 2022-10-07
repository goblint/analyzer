There should be no CIL warning about multiple definitions:

  $ goblint --set pre.cppflags[+] -DGOBLINT_NO_QSORT 03-noqsort.c
  [Warning][Deadcode] Function 'qsort' is uncalled: 1 LLoC (03-noqsort.c:5:1-6:1)
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 2
    dead: 1 (1 in uncalled functions)
    total: 3
