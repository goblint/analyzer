// SKIP PARAM: --set ana.activated[+] apron
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 5;
int h = 5;
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
    h = 10;
  }
  // sync join needs to publish globals also to protected/mutex_inits like enter_multithreaded
  // might need join strengthening to reveal unsoundness instead of going to top directly

  pthread_mutex_lock(&m);
  assert(g == h); // UNKNOWN!
  pthread_mutex_unlock(&m);

  return 0;
}