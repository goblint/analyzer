// CRAM
#include <goblint.h>


int main() {
  int x = 0;
  __goblint_check(x == 0);
  x = 1;
  __goblint_check(x == 1);

  int *p = &x; // confuse naive escape check
  return 0;
}
