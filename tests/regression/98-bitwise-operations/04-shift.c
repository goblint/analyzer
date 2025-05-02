// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  if (x > 10) {
    x = 10;
  }

  unsigned int y = x >> 0;

  __goblint_check(y <= 10); 

  unsigned int z = x >> 1;

  __goblint_check(z <= 5); 

  unsigned int w = x >> 2;

  __goblint_check(w <= 2); 

  unsigned int a;

  if (a == 0) {
    a = 1;
  }

  if (a > 10) {
    a = 10;
  }

  unsigned int b = a << 0;

  __goblint_check(b >= 1); // TODO

  unsigned int c = a << 1;

  __goblint_check(c >= 2); // TODO

  unsigned int d = a << 2;

  __goblint_check(d >= 4); // TODO

  return 0;
}