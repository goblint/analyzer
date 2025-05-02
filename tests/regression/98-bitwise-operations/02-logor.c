// PARAM: --enable ana.int.interval_set
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

  int e;

  if (e > 2) {
    e = 2;
  }

  int f = e | -2;
  __goblint_check(f <= 0);

  int g;

  if (g > 2) {
    g = 2;
  }

  int h;

  if (h < -2) {
    h = -2;
  }

  int i = g | h;

  __goblint_check(i <= __INT_MAX__);
  __goblint_check(i >= -__INT_MAX__ - 1); 

  int first = 0;
  int second;

  if (second < -2 || second > 0) {
    second = 0;
  }

  __goblint_check((first | second) <= 0);
  __goblint_check((first | second) >= -2);
  return 0;
}