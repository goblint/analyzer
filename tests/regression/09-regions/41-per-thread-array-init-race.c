// PARAM: --set ana.activated[+] region --enable ana.sv-comp.functions
// Per-thread array pointers passed via argument but initialized before thread create.
// Extracted from silver searcher.
#include <stdlib.h>
#include <pthread.h>
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}
extern int __VERIFIER_nondet_int();

void *thread(void *arg) {
  int *p = arg;
  int i = *p; // RACE!
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  assume_abort_if_not(threads_total >= 0);

  pthread_t *tids = malloc(threads_total * sizeof(pthread_t));
  int *is = calloc(threads_total, sizeof(int));

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, &is[i]); // may fail but doesn't matter
    is[i] = i; // RACE!
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  free(tids);
  free(is);

  return 0;
}
