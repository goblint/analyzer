// CRAM PARAM: --set ana.activated[+] phaseGhost --enable warn.deterministic --set lib.activated[+] sv-comp
#include <pthread.h>
#include <goblint.h>

extern int unknown;

void *worker_unique(void *arg) {
  __VERIFIER_atomic_begin();
  __VERIFIER_atomic_end();
  return NULL;
}

void *worker_nonunique(void *arg) {
  __VERIFIER_atomic_begin();
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  int i;
  pthread_t t_unique;
  pthread_t t_nonunique[2];

  __VERIFIER_atomic_begin();
  __VERIFIER_atomic_end();

  pthread_create(&t_unique, NULL, worker_unique, NULL);
  for (i = 0; i < 2; i++) {
    pthread_create(&t_nonunique[i], NULL, worker_nonunique, NULL);
  }

  pthread_join(t_unique, NULL);
  for (i = 0; i < 2; i++) {
    pthread_join(t_nonunique[i], NULL);
  }

  return 0;
}
