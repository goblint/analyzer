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

  int aa;

  int test = -55;

  int test2;

  if (test2 > 0 || test2 < -26) {
    test2 = 0;
  }

  int test3 = test2 & test;
  __goblint_check(test3 <= 63);

  long long lltest = 4829301800843266965LL;

  int lltest2;

  long long lltestcon = lltest2 & lltest;

  __goblint_check(lltestcon <= __LONG_LONG_MAX__);
  return 0;
}