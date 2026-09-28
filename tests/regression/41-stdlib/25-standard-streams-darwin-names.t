With Darwin's names for the standard streams, __stdoutp points to the object
of stdout: nothing goes through an unknown address, and fputc and printf write
the attached buffer.

  $ goblint 25-standard-streams-darwin-names.c 2>&1 | grep -e Assert -e Unsound
  [Success][Assert] Assertion "(unsigned long )__stdoutp != (unsigned long )((FILE *)0)" will succeed (25-standard-streams-darwin-names.c:22:3-22:34)
  [Success][Assert] Assertion "(unsigned long )__stdoutp != (unsigned long )__stderrp" will succeed (25-standard-streams-darwin-names.c:23:3-23:42)
  [Warning][Assert] Assertion "out.x == 1" is unknown. (25-standard-streams-darwin-names.c:27:3-27:30)
  [Warning][Assert] Assertion "out.x == 1" is unknown. (25-standard-streams-darwin-names.c:30:3-30:30)
