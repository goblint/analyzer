// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include<assert.h>

int f(int in) __attribute__ ((goblint_precision("no-def_exc","interval")));
int main() __attribute__ ((goblint_precision("no-def_exc","congruence")));

int f(int in) {
  in++;
  return in;
}

int main() {
  int a = 0;
  int b = f(a);
  __goblint_check(b);
  a = b % 2;
  b = f(a);
  __goblint_check(b == 2);
  return 0;
}
