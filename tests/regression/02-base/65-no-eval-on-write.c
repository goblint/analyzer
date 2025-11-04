//PARAM: --enable ana.int.interval --enable exp.earlyglobs --enable ana.int.enums -v
#include <goblint.h>

// Test case that shows how avoiding reading integral globals can reduce the number of solver evaluations.
// Avoiding to evaluate integral globals when setting them reduced the number of necessary evaluations from 27 to 14 in this test case.
int glob = 10;

void foo() {
  glob = 3;
  glob = 4;
  glob = 1;
}

void bar() {
  glob = 2;
}

int main() {
  foo();
  bar();
  __goblint_check(glob >= 1);
  __goblint_check(glob <= 10);
  return 0;
}
