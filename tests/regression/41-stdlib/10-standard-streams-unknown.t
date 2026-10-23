After a call of a function without a definition, stdout and the streams'
objects are unknown, and a stream from fopen has an unknown address: every call
that writes such a stream through an unknown address says so. Normalize the
Darwin name for stdout in the diagnostics.

  $ goblint 10-standard-streams-unknown.c 2>&1 | sed 's/__stdoutp/stdout/g' | grep -F -e '[Info][Unsound]' -e 'Assert'
  [Info][Unsound] Unknown address in stdout has escaped. (10-standard-streams-unknown.c:17:3-17:14)
  [Info][Unsound] Unknown address in [stdout] has escaped. (10-standard-streams-unknown.c:17:3-17:14)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (10-standard-streams-unknown.c:17:3-17:14)
  [Warning][Assert] Assertion "gbuf.x == 1" is unknown. (10-standard-streams-unknown.c:18:3-18:31)
  [Warning][Assert] Assertion "(unsigned long )stdout != (unsigned long )((FILE *)0)" is unknown. (10-standard-streams-unknown.c:20:3-20:30)
  [Info][Unsound] Unknown address in (FILE * __restrict  )f has escaped. (10-standard-streams-unknown.c:25:5-25:48)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (10-standard-streams-unknown.c:25:5-25:48)
  [Info][Unsound] Unknown address in f has escaped. (10-standard-streams-unknown.c:26:5-26:18)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (10-standard-streams-unknown.c:26:5-26:18)
  [Info][Unsound] Unknown address in stdout has escaped. (10-standard-streams-unknown.c:28:5-28:16)
  [Info][Unsound] Unknown value in ? could be an escaped pointer address! (10-standard-streams-unknown.c:28:5-28:16)
