// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info --disable warn.imprecise --disable warn.unsound
// A standard stream may be modified by an unknown function.
#include <stdio.h>

extern void unknown(void);

int main() {
  char buf[10];
  fgets(buf, 10, stdin); // NOWARN
  unknown();
  fgets(buf, 10, stdin); // WARN
  return 0;
}
