#include <pthread.h>
#include <assert.h>

void foo() {
  assert(1);
}

void bar() {
  assert(1);
}

void (*funs[2])() = {
  &foo,
  &bar
};


extern void magic1();
extern void magic2(void (*funs[])());

void *t_fun(void *arg) {
  // just for going to multithreaded mode
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded mode

  magic1(); // invalidate funs a bit
  magic2(funs);
  return 0;
}
