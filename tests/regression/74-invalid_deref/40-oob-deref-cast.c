// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

int main() {
  char* s = malloc(10);
  s += 10; // NOWARN
  unsigned char ch = (*(unsigned char *) s); // WARN!
  return 0;
}
