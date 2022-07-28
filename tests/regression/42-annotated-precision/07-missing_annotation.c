// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include <assert.h>

int f(int in) __attribute__((goblint_precision("def_exc")));
int main() __attribute__((goblint_precision("no-def_exc")));


int f(int in) {
  return in + 1;
}

int main() {
  int a = 1;
  __goblint_check(a); // UNKNOWN!
  a = f(a);
  return 0;
}
