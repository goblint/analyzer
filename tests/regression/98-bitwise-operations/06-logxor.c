// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  int x;
  int y = -1;

  if (x > 0 || x < -14) {
    x = 0;
  }
  int z = x ^ y;
  return 0;
}
