// be sound and claim that assert may hold instead of must not hold
// assert passes when compiled
#include <assert.h>

struct s {
  int fst;
};

int main() {
  struct s a;
  void *p = &a;
  void *q = &a.fst;
  assert(p == q);
  return 0;
}
