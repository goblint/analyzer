// CRAM
#include <goblint.h>

int x, y;

void foo() {
  __goblint_check(x == y);
}

int main() {
  int r;
  if (r)
    x = y = 5;
  else
    x = y = 10;
  foo();
  return 0;
}
