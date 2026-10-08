// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// Without the closedStdStreams analysis, standard streams are not tracked.
#include <stdio.h>

int main() {
  char buf[10];
  fgets(buf, 10, stdin); // WARN
  return 0;
}
