extern int __VERIFIER_nondet_int();

#include <assert.h>
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void fun1() {
  __goblint_check(0); // FAIL
}

void fun2() {
  __goblint_check(0); // FAIL
}

int main() {
  int x = __VERIFIER_nondet_int();

  void (*fp)();

  if (x) {
    pthread_mutex_lock(&mutex);
    fp = fun1;
  }
  else {
    fp = fun2;
  }

  fp();
}