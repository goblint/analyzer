#include <pthread.h>
#include <assert.h>

void foo() {
  assert(1); // assert reachable
}

void bar() {
  assert(1); // assert reachable
}

void (*funs[2])() = {
  &foo,
  &bar
};

extern void magic1();
extern void magic2(void (*funs[])());

int main() {
  magic1(); // invalidate funs a bit
  magic2(funs);
  return 0;
}
