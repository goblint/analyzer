// PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint
#include<assert.h>

struct a {
  char *name;
  int i;
};

void f(struct a *in) __attribute__ ((goblint_precision("no-def_exc","interval", "congruence")));
int main() __attribute__ ((goblint_precision("congruence")));

void f(struct a *in) {
  in->i += 4;
  return;
}

int main() {
  struct a a1, b1 = {"Jane", 3};

  a1.name = "John";
  a1.i = 6;
  __goblint_check(a1.i == 6);

  f(&a1);
  __goblint_check(a1.i == 10);
  __goblint_check(a1.i == b1.i); // FAIL!
  b1.i = a1.i % 5;
  __goblint_check(b1.i); // FAIL!
  return 0;
}
