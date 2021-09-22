// PARAM: --set ana.int.refinement fixpoint --set ana.int.def_exc false
#include<assert.h>

int f(int in) __attribute__ ((precision("interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((precision("def_exc", "interval"))) {
  int a = 0;
  assert(a); // FAIL!
  a = f(a);
  assert(a);
  return 0;
}