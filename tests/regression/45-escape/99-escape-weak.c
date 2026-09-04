#include <goblint.h>

int *g;

void foo() {
  int x = 42;
  g = &x;
}

int main() {
  foo();
  __goblint_check(*g == 42); // ignoring use after free...
  *g = 100;
  foo();
  __goblint_check(*g == 42); // UNKNOWN! ignoring use after free...
  __goblint_check(*g == 100); // UNKNOWN! ignoring use after free...
  return 0;
}
