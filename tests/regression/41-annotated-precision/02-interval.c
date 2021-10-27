// PARAM: --enable precision.annotation --set ana.int.refinement fixpoint
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("no-def_exc","interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((goblint_precision("def_exc", "interval"))) {
  int a = 0;
  assert(a); // FAIL!
  a = f(a);
  assert(a);
  return 0;
}