// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// A standard stream modified through a pointer (in another function) is checked like any other pointer.
#include <stdio.h>

void reassign(FILE **s) {
  *s = (FILE *)0;
}

int main() {
  char buf[10];
  fgets(buf, 10, stdin); // NOWARN
  reassign(&stdin);
  fgets(buf, 10, stdin); // WARN
  fprintf(stderr, "%s\n", buf); // NOWARN
  return 0;
}
