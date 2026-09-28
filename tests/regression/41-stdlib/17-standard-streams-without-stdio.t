printf writes the object of stdout, initialized though the file does not
declare stdout, so nothing writes through an unknown address.

  $ goblint 17-standard-streams-without-stdio.c 2>&1 | grep Unsound
  [1]
