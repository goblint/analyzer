// TODO: be sound and claim that assert may hold instead of must not hold
// assert passes when compiled
#include <goblint.h>

union u {
  int fst;
  float snd;
};

int main() {
  union u a;
  void *p = &a.fst;
  void *q = &a.snd;
  assert(p == q);
  return 0;
}
