// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] abortUnless --set ana.path_sens[+] threadflag
// Minimized SV-COMP version of our regression test
// NOTIMEOUT
#include <assert.h>
#include <pthread.h>

void reach_error() {
  assert(0); // NOWARN
}
void __VERIFIER_assert(int cond) { // NOWARN
  if(!(cond)) { ERROR: {reach_error();abort();} }
}

int glob1 = 5;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&mutex1);
  t = glob1;
  __VERIFIER_assert(t == 5); // NOWARN
  glob1 = -10;
  __VERIFIER_assert(glob1 == -10); // NOWARN
  glob1 = t;
  pthread_mutex_unlock(&mutex1);
  return ((void *)0);
}
int main(void) {
  pthread_t id;
  __VERIFIER_assert(glob1 == 5); // NOWARN
  pthread_create(&id, ((void *)0), t_fun, ((void *)0));
  pthread_mutex_lock(&mutex1);
  glob1++;
  __VERIFIER_assert(glob1 == 6); // NOWARN
  glob1--;
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, ((void *)0));
  return 0;
}
