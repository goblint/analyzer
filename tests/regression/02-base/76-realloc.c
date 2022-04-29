#include <stdlib.h>
#include <assert.h>

void test1_f() {
  assert(1); // reachable
}

void test1() {
  void (**fpp)(void) = malloc(sizeof(void(**)(void)));
  *fpp = &test1_f;

  fpp = realloc(fpp, sizeof(void(**)(void))); // same size

  // (*fpp)();
  void (*fp)(void) = *fpp;
  fp(); // should call test1_f
}

int main() {
  test1();
  return 0;
}
