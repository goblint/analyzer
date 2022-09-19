#include <assert.h>

void foo() {

}

int main() {
  int a = 1;

  if (a) // WARN
    __goblint_check(a);

  foo();

  return 0;
}