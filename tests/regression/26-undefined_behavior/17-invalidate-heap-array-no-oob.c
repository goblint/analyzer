// PARAM: --enable ana.arrayoob --enable ana.int.interval --disable warn.imprecise --disable warn.unsound
// Invalidating an array whose type is not known to be an array (here: within untyped heap memory) is not an array access by the program either.
#include <stdlib.h>

struct s {
  int a[2];
};

extern void unknown(void *x);

int main() {
  void *v = malloc(sizeof(struct s));
  struct s *x = v;
  x->a[0] = 0;
  unknown(v); // NOWARN
  return 0;
}
