// SKIP PARAM: --conf ./conf/pentagon.json

#include <goblint.h>

int main(void) {
  int x, y;
  int z;

  // Neither strict order is tracked, so the difference may have either sign.
  __goblint_check(x - y < 0); // UNKNOWN

  z = x - y;
  __goblint_check(z < 0); // UNKNOWN
}
