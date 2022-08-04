// PARAM: --enable annotation.int.enabled --enable exp.earlyglobs --set ana.int.refinement fixpoint
#include<assert.h>

int inc(int in) __attribute__ ((goblint_precision("no-def_exc", "interval")));
int main() __attribute__ ((goblint_precision("def_exc")));

int g1 = 0;
int g2 = 0;

int inc(int in) {
    int b = in + 1;
    __goblint_check(b);
    g2 = 1;
    return b;
}

int main() {
  int a = 0;
  __goblint_check(g1); // FAIL!
  a = inc(g1);
  __goblint_check(a == g1); // FAIL!
  __goblint_check(a == g2); // UNKNOWN!
  return 0;
}
