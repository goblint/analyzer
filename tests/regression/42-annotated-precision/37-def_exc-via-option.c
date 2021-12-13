// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint --set annotation.goblint_precision.def_exc[+] f --set annotation.goblint_precision.interval[+] f  --set annotation.goblint_precision.def_exc[+] main
#include<assert.h>

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
