#include <assert.h>

void foo() {
  int x = 2;
  __goblint_check(x == 3); //FAIL
}

int main() {
  int a = 1;

  foo();

  return 0;
}
