// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
#include <goblint.h>

int main() {
  int x = 0;
  int y = 10;
  int z = 5;

  for (int i = 0; i < 3; i++) {
    x = z;
    y = i;
    __goblint_check(x == z); // SUCCESS
    z = 2;
    __goblint_check(y == i);  // SUCCESS
    __goblint_check(z == 2); // SUCCESS
  }

  return 0;
}