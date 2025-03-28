// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = x & 2;

  unsigned int z = y & 4;

  __goblint_check(z <= 2);
  
  __goblint_check(z <= 3); // def exci test
  __goblint_check(z >= 0);

  int a;

  if (a >= 0) {
    a = -1;
  }

  int b = a & 2;

  __goblint_check(b >= 0); // TODO

  int c;
  
  if (c >= 0) {
    c = -1;
  }

  int d = c & -2;

  __goblint_check(d <= 0);

  return 0;
}