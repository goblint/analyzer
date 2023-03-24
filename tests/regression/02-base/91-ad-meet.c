// TODO: be sound and claim that assert may hold instead of must not hold
// assert passes when compiled
#include <goblint.h>

struct s {
  int fst;
};

int main() {
  struct s a;
  void *p = &a.fst;
  void *q = ((int(*)[1]) (&a))[0];
  assert(p == q); //UNKNOWN
  return 0;
}
