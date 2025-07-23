// PARAM: --enable ana.int.def_exc --disable ana.int.interval
#include <goblint.h>

int main() {
  int x;

  if (-10 <= x) {
    __goblint_check(-128 <= x);
  }

  if (x <= 10) {
    __goblint_check(x <= 127); // TODO
  }
  return 0;
}
