extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int global = 5;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  int r = __VERIFIER_nondet_int();
  pthread_t id;

  if (r) {
    pthread_create(&id, NULL, t_fun, NULL);
  }
  else {
    global = 10;
  }
  // sync join needs to publish global also to protected/mutex_inits like enter_multithreaded

  pthread_mutex_lock(&m);
  assert(global == 5); // UNKNOWN!
  pthread_mutex_unlock(&m);

  return 0;
}