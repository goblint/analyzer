#include <pthread.h>
#include <assert.h>

void foo() {
  __goblint_check(1); // assert reachable
}

void bar() {
  __goblint_check(1); // assert reachable
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
