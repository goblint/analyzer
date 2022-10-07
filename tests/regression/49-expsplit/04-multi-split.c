// PARAM: --set ana.activated[+] expsplit
#include <stddef.h>
#include <assert.h>
#include <goblint.h>

int main() {
  int r, r2; // rand
  int x, y, z;

  __goblint_split_begin(x);
  __goblint_split_begin(y);
  if (r) {
    x = 1;
    if (r2) {
      y = 1;
      z = 1;
    }
    else {
      y = 2;
      z = 2;
    }
  }
  else {
    x = 2;
    if (r2) {
      y = 1;
      z = 3;
    }
    else {
      y = 2;
      z = 4;
    }
  }

  __goblint_check((x == 1 && y == 1 && z == 1) || (x == 1 && y == 2 && z == 2) || (x == 2 && y == 1 && z == 3) || (x == 2 && y == 2 && z == 4));

  __goblint_split_end(x);

  __goblint_check((x == 1 && y == 1 && z == 1) || (x == 1 && y == 2 && z == 2) || (x == 2 && y == 1 && z == 3) || (x == 2 && y == 2 && z == 4)); // UNKNOWN (intentionally)

  __goblint_split_end(y);

  __goblint_check((x == 1 && y == 1 && z == 1) || (x == 1 && y == 2 && z == 2) || (x == 2 && y == 1 && z == 3) || (x == 2 && y == 2 && z == 4)); // UNKNOWN (intentionally)

  return 0;
}