// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] closedStdStreams --enable ana.int.interval --disable warn.info
// A standard stream must not be used after it has been closed.
#include <stdio.h>

int main() {
  char buf[10];
  FILE *in = stdin;
  fgets(buf, 10, stdin); // NOWARN
  fclose(stdin); // NOWARN
  fgets(buf, 10, stdin); // WARN
  fgets(buf, 10, in); // WARN
  fprintf(stderr, "%s\n", buf); // NOWARN
  fclose(stdin); // WARN
  return 0;
}
