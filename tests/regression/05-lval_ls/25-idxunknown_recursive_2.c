// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] pthreadMutexType
_Bool __VERIFIER_nondet_bool();

#define _GNU_SOURCE
#include <pthread.h>

int g;

#ifdef __APPLE__
pthread_mutex_t m[2] = {PTHREAD_RECURSIVE_MUTEX_INITIALIZER, PTHREAD_RECURSIVE_MUTEX_INITIALIZER};
#else
pthread_mutex_t m[2] = {PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP, PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP};
#endif

void *t_fun(void *arg) {
  int i;
  i = __VERIFIER_nondet_bool();

  pthread_mutex_lock(&m[0]); // must lock m[0]
  pthread_mutex_unlock(&m[i]); // may unlock m[0]
  pthread_mutex_lock(&m[0]); // must lock m[0]
  pthread_mutex_unlock(&m[0]); // must unlock m[0]
  // m[0] should not be in must-lockset here!
  g++; // RACE!
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m[0]);
  g++; // RACE!
  pthread_mutex_unlock(&m[0]);
  return 0;
}
