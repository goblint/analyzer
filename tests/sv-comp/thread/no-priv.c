#include <pthread.h>

extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int g = 5;

void *t_fun(void *arg) {
  int x;
  x = g;
  __VERIFIER_assert(x == 5);
  g = -10;
  __VERIFIER_assert(g == -10);
  g = x;
  return NULL;
}

int main(void) {
  pthread_t id;
  __VERIFIER_assert(g == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  g++;
  __VERIFIER_assert(g == 6);
  g--;
  pthread_join(id, NULL);
  return 0;
}
