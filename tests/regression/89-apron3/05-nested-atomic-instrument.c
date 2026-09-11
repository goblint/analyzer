// SKIP PARAM: --enable ana.sv-comp.functions  --set lib.activated[+] sv-comp --set ana.activated[+] apron --set ana.apron.domain octagon --set ana.relation.privatization mutex-meet-tid-atomic-ghost --set sem.int.signed_overflow assume_none --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <goblint.h>

extern void __VERIFIER_atomic_begin(void);
extern void __VERIFIER_atomic_end(void);
extern void __VERIFIER_atomic_instrument_end(void);
extern void __VERIFIER_atomic_instrument_begin(void);

int i = 3;
int ghost_t1_phase = 0;

void *t1(void *arg) {
  __VERIFIER_atomic_instrument_begin();
  __VERIFIER_atomic_begin();
  ghost_t1_phase ++;
  __VERIFIER_atomic_instrument_end();
  i = i + 1;
  __VERIFIER_atomic_end();

  return 0;
}


int main(int argc, char **argv) {
  pthread_t id1;

  pthread_create(&id1, NULL, t1, NULL);

  __goblint_check(i == 3); //UNKNOWN!

  return 0;
}
