// SKIP
// TODO: be sound and claim that assert may hold instead of must not hold
// assert passes when compiled
#include <assert.h>

int main() {
  int a[10];
  void *p = &a;
  void *q = &a[0];
  assert(p == q);
  return 0;
}
