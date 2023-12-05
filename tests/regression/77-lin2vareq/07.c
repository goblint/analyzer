#include <goblint.h>

int main() {
  int a = 2;
  int b = 3;
  int c = 1;
  int d = 0;

  int x = a * a + 2 * c;
  __goblint_check(x == 5); //FAIL

  int y = 100 * b + 4 * c + 400 - 100;
  __goblint_check(y == 7 * c + 300); //FAIL
  int z = a * 2 - c * (1 * (5 - d)) + 3;
  __goblint_check(z == 4); //FAIL

  int p = 2 * a + 3 * b + c + d;
  __goblint_check(2 * p == 4 * a + 6 * b + 2 * c + 2 * d); //SUCCESS
  return 0;
}

