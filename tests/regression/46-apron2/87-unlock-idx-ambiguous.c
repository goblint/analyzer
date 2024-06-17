// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization mutex-meet --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag
// TODO: why nonterm without threadflag path-sens?
#include <pthread.h>
#include <goblint.h>
extern _Bool __VERIFIER_nondet_bool();

int g;
pthread_mutex_t m[2] = {PTHREAD_MUTEX_INITIALIZER, PTHREAD_MUTEX_INITIALIZER};

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[0]);
  pthread_mutex_lock(&m[1]); // so we're unlocking a mutex we definitely hold
  g++;
  int r = __VERIFIER_nondet_bool();
  pthread_mutex_unlock(&m[r]); // TODO NOWARN (definitely held either way)
  // could have unlocked m[0], so should have published g there
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m[0]);
  __goblint_check(g == 0); // UNKNOWN!
  pthread_mutex_unlock(&m[0]);
  return 0;
}
