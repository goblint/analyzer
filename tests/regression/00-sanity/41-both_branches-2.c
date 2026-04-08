// PARAM: --disable sem.unknown_function.invalidate.globals
#include <goblint.h>
struct S {
  int *f[1];
};

int main() {
  struct S* s;
  s = magic();

  int *p = s->f[0];
  if (p)
    __goblint_check(1); // reachable
  else
    __goblint_check(1); // reachable
  return 0;
}
