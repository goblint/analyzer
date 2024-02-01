// SKIP PARAM: --set ana.activated[+] lin2vareq --enable ana.int.interval
#include <goblint.h>
#include <limits.h>

int main(void) {
  int x = 10;
  int b;
  int a;
  a = a %2;
  b = a + 2;

  if (x + 2147483647 == 2147483657) {
    return 0;
  }

  __goblint_check(1);

  // Overflow
  int c = 2147483647;
  c = c + 1;
  __goblint_check(c < 2147483647); // UNKNOWN!

  x = 300 % b;
  x = x * 1147483647; // should overflow
  __goblint_check (x < 2147483647); // UNKNOWN!

  int y = 300 % a; //might overflow
  __goblint_check (y < 2147483647); // UNKNOWN!

  int z = y << (a-1); //might overflow
  __goblint_check (z < 2147483647); // UNKNOWN!

}
