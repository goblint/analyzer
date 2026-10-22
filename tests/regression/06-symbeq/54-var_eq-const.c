// PARAM: --set ana.activated[+] var_eq
#include <stdlib.h>
#include <goblint.h>

int main() {
  int r, r2; // rand
  int i, j;

  int *p = r ? &i : &j; // create ambiguity which base cannot handle, but var_eq can

  *p = r2 ? 4 : 0x4; // different syntax for the same constant shouldn't bother var_eq
  __goblint_check(*p == 4); // TODO

  *p = 4U; // var_eq should account for the implicit cast
  __goblint_check(*p == 4); // TODO

  *p = sizeof(int); // var_eq should do machdep-based constant folding and account for the implicit cast
  __goblint_check(*p == 4); // TODO
  return 0;
}
