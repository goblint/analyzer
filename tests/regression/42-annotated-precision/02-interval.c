// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("no-def_exc","interval")));
int main() __attribute__ ((goblint_precision("def_exc", "interval"))) ;

int f(int in) {
  in++;
  return in;
}

int main() {
  int a = 0;
  assert(a); // FAIL!
  a = f(a);
  assert(a);
  return 0;
}
