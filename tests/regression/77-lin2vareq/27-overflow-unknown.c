// SKIP PARAM: --set ana.activated[+] lin2vareq
#include <goblint.h>
#include <limits.h>

int main(void) {
  int x = 10;

  if (x + 2147483647 == 2147483657) {
    return 0;
  }

  __goblint_check(1);

  // Overflow
  int c = 2147483647;
  c = c + 1;

  __goblint_check(c < 2147483647); // UNKNOWN!
}
