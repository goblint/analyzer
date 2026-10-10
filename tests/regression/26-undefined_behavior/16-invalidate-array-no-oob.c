// PARAM: --enable ana.arrayoob --enable ana.int.interval --disable warn.imprecise --disable warn.unsound
// Invalidating or computing the reachable addresses of a value that contains an array is not an array access by the program.
#include <goblint.h>

struct s {
  int a[2];
  int *p;
};

extern void unknown(struct s *x);

int main() {
  int y = 1;
  struct s x = {{0, 0}, &y};
  unknown(&x); // NOWARN
  x.a[1] = 1; // NOWARN
  x.a[2] = 1; // WARN
  return 0;
}
