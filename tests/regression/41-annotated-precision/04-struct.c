// PARAM: --enable exp.annotated.precision --set ana.int.refinement fixpoint --set ana.int.def_exc false
#include<assert.h>

struct a {
  char *name;
  int i;
};

void f(struct a *in) __attribute__ ((goblint_precision("interval", "congruence"))) {
  in->i += 4;
  return;
}

int main() __attribute__ ((goblint_precision("def_exc", "congruence"))) {
  struct a a1, b1 = {"Jane", 3};

  a1.name = "John";
  a1.i = 6;
  assert(a1.i == 6);

  f(&a1);
  assert(a1.i == 10);
  assert(a1.i == b1.i); // FAIL!
  b1.i = a1.i % 5;
  assert(b1.i); // FAIL!
  return 0;
}