// PARAM: --enable ana.int.interval
#include <goblint.h>
int main() {
  int x, y, z;
  __goblint_assume(0 <= x);
  __goblint_assume(x <= 10);
  __goblint_assume(5 <= y);
  __goblint_assume(y <= 15);
  __goblint_assume(-10 <= z);
  __goblint_assume(z <= 10);

  if (x >= y) {
    __goblint_check(5 <= x);
    __goblint_check(y <= 10); // why doesn't Miné refine this?
  }

  if (z >= x) {
    __goblint_check(0 <= z);
  }

  if (x >= y && z >= x) { // CIL transform does branches sequentially (good order)
    __goblint_check(5 <= x);
    __goblint_check(y <= 10); // why doesn't Miné refine this?
    __goblint_check(0 <= z);

    __goblint_check(5 <= z);
  }

  if (z >= x && x >= y) { // CIL transform does branches sequentially (bad order)
    __goblint_check(5 <= x);
    __goblint_check(y <= 10); // why doesn't Miné refine this?
    __goblint_check(0 <= z);

    __goblint_check(5 <= z); // TODO
  }

  return 0;
}
