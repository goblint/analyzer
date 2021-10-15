// PARAM: --enable exp.annotated.precision --set ana.int.refinement fixpoint --set ana.int.def_exc false
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((goblint_precision("congruence"))) {
  int a = 0;
  int b = f(a);
  assert(b);
  a = b % 2;
  b = f(a);
  assert(b == 2);
  return 0;
}