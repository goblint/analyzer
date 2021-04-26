#include <pthread.h>

extern void __VERIFIER_error() __attribute__ ((__noreturn__));
void __VERIFIER_assert(int cond) {
    if (!(cond)) {
        ERROR: __VERIFIER_error();
    }
    return;
}

int g = 5;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x;
  pthread_mutex_lock(&m);
  x = g;
  __VERIFIER_assert(x == 5);
  g = -10;
  __VERIFIER_assert(g == -10);
  g = x;
  pthread_mutex_unlock(&m);
  return NULL;
}

int main(void) {
  pthread_t id;
  __VERIFIER_assert(g == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m);
  g++;
  __VERIFIER_assert(g == 6);
  g--;
  pthread_mutex_unlock(&m);
  pthread_join(id, NULL);
  return 0;
}
