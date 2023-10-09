Testing that there isn't a warning about treating long double as double constant.
  $ goblint 37-long-double.c
  [Info][Deadcode] Logical lines of code (LLoC) summary:
    live: 3
    dead: 0
    total lines: 3
