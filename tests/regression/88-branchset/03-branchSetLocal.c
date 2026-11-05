// PARAM: --set ana.activated[+] branchSet
// CRAM: Demonstrates the difference between branchSet and branchSetLocal.
#include <goblint.h>

int x, y;

void foo() {
  int r;
  if (r)
    x = y = 5;
  else
    x = y = 10;
}

int main() {
  foo();
  __goblint_check(x == y);
  return 0;
}
