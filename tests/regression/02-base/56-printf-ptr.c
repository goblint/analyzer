#include <stdio.h>
#include <assert.h>

int main() {
  int x = 42;
  printf("&x = %p\n", &x); // doesn't invalidate x despite taking address
  assert(x == 42);
  return 0;
}
