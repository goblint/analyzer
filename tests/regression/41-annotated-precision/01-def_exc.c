// PARAM: --enable exp.annotated.precision --set ana.int.refinement fixpoint
#include<assert.h>

int f(int in) __attribute__ ((precision("def_exc", "interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((precision("def_exc"))) {
  int a = 0;
  assert(a); // FAIL!
  a = f(a);
  assert(a);
  return 0;
}