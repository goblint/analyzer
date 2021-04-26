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
  __VERIFIER_assert(g == 5);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __VERIFIER_assert(g == 5);
  return 0;
}
