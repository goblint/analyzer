// PARAM: --set ana.int.refinement fixpoint --set ana.int.def_exc false
#include<assert.h>

int f(int in) __attribute__ ((precision("interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((precision("congruence"))) {
  int a = 0;
  int b = f(a);
  assert(b);
  a = b % 2;
  b = f(a);
  assert(b == 2);
  return 0;
}