#include <goblint.h>

union U {
  int x;
  int y;
};

int main() {
  union U u;

  int r, r2; // rand

  switch (r) {
    case 0:
      u.x = 0;
      __goblint_check(u.x == 0);
      __goblint_check(u.y == 0);
      break;

    case 1:
      u.y = 0;
      __goblint_check(u.x == 0);
      __goblint_check(u.y == 0);
      break;

    case 2:
      if (r2)
        u.x = 0;
      else
        u.y = 0;

      __goblint_check(u.x == 0);
      __goblint_check(u.y == 0);
      break;

    default:
      break;
  }
  return 0;
}
