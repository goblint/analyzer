// PARAM: --enable precision.annotation --set ana.int.refinement fixpoint
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("no-def_exc","interval"))) {
  in++;
  return in;
}

int main() __attribute__ ((goblint_precision("no-def_exc","congruence"))) {
  int a = 0;
  int b = f(a);
  assert(b);
  a = b % 2;
  b = f(a);
  assert(b == 2);
  return 0;
}