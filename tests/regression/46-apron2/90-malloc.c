// SKIP PARAM: --set ana.activated[+] apron  --set ana.relation.privatization mutex-meet --set ana.apron.domain interval --set sem.int.signed_overflow assume_none
// Checks that assinging to malloc'ed memory does not cause both branches to be dead
#include <pthread.h>
#include <goblint.h>
void nop(void* arg) {
}

void main() {
  pthread_t thread;
  pthread_create(&thread, 0, &nop, 0);

  long *k = malloc(sizeof(long));
  *k = 5;
  if (1)
    ;

  __goblint_check(*k >= 5); // Reachable and true

  *k = *k+1;
  __goblint_check(*k >= 5); // Reachable and true
}
