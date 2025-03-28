// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = x | 2;

  __goblint_check(y >= 2);  
  
  unsigned int z = y | 3;

  __goblint_check(z >= 3);

  int a;

  int b = a | -2;

  __goblint_check(b <= 0); 

  int c;

  if (c >= 0) {
    c = -1;
  }

  int d = c | 2;

  __goblint_check(d <= 0);
  return 0;
}