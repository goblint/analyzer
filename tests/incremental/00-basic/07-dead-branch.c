#include <assert.h>

void foo() {

}

int main() {
  int a = 1;

  if (a) // WARN
    assert(a);

  foo();

  return 0;
}