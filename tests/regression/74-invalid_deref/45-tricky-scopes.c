// PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <goblint.h>

void foo(int **p);

void bar(int *q) {
  // q points to x from outer foo (in scope)
  __goblint_check(*q == 0);

  int *r = NULL;
  foo(&r);
  // r points to x from inner foo (out of scope)

  // there's just one x (not weak), and both q and r point to it

  *r = 1; // WARN (out of scope access)

  // should not have modified the outer x
  // these checks should pass (or be unknown), but certainly not fail
  __goblint_check(*q == 0);
  *q = 1; // NOWARN (in scope access)
  __goblint_check(*q == 1);
}

void foo(int **p) {
  int x = 0;
  if (p) { // inner call from bar
    // x is not weak here because outer x is not reachable (by argument or otherwise)
    *p = &x;
  }
  else { // outer call from main
    bar(&x);
  }
}

int main() {
  foo(NULL);
  return 0;
}
