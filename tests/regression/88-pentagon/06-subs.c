#include <goblint.h>

int main() {
  int x = rand(), y = x, z = x; // we can't store equalities ==> this just adds less tmp vars than (x = rand(), y = rand(), z = rand();), but is otherwise equivalent
  __goblint_assume(x > y);

  __goblint_check(y < __INT32_MAX__); // SUCCESS
  __goblint_check(y < x);     // SUCCESS
  __goblint_check(x - y > 0); // SUCCESS

  __goblint_assume(x <= 1000);
  __goblint_assume(y <= 10000);
  z = x - y + 4;
  __goblint_check(z > 0);     // SUCCESS
  __goblint_check(z > 4);     // SUCCESS

  x = rand(), y = x, z = x;
  __goblint_assume(x <= 11);
  __goblint_assume(y <= 1);
  __goblint_assume(z >= 5);
  __goblint_assume(z <= 10);

  if (9*x + 10*y > 20*z - 1) {
    __goblint_check(x >= 10); // UNKNOWN (we might want to implement this to be more precise, or just use the interval domain)
    __goblint_check(y == 1);  // UNKNOWN
    __goblint_check(x >= 2);  // SUCCESS
  }
}