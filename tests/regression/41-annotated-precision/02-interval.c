// PARAM: --enable exp.annotated.precision --set ana.int.refinement fixpoint --set ana.int.def_exc false
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("interval"))) {
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