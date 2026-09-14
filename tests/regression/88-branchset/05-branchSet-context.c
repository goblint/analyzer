// PARAM: --set ana.activated[+] branchSetFull --disable ana.base.context.int
// CRAM: Demonstrates the difference between branchSetFull and branchSet.
// This is very artificial because it requires disabling some context.
// Usually the path split in the caller should be reflected in different (value) contexts of the callee.
// In that case the actual branching in the caller is irrelevant.
#include <goblint.h>

int x, y;

void foo() {
  __goblint_check(x == y);
}

void bar() {
  __goblint_check(x == y);
  foo();
}

int main() {
  int r;
  if (r)
    x = y = 5;
  else
    x = y = 10;
  bar();
  return 0;
}
