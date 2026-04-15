// CRAM PARAM: --set ana.activated[+] phaseGhost --enable warn.deterministic --set lib.activated[+] sv-comp
#include <pthread.h>
#include <goblint.h>

extern int __VERIFIER_nondet_int(void);

int ghost_a = 0; // phase ghost: accessed only by main and incremented by 1.
int ghost_b = 0; // not a phase ghost: accessed only by main, but assigned a constant not equal to old + 1.
int ghost_c = 0; // phase ghost: accessed only by main and updated syntactically as 1 + ghost_c.
int ghost_d = 0; // not a phase ghost: accessed only by main, but incremented by 2.
int ghost_e = 0; // not a phase ghost: accessed only by main, but increment amount is nondeterministic.
int ghost_f = 0; // not exercised: never accessed in this test.
int ghost_g = 0; // not a phase ghost: incremented by both main and a different unique worker thread.
int ghost_h = 0; // not a phase ghost: incremented by a non-unique thread id.
int ghost_i = 0; // phase ghost: accessed only by one unique worker thread and incremented by 1.
int ghost_j = 0; // not a phase ghost: accessed only by one unique worker thread, but assigned 7.
int ghost_k = 0; // phase ghost: accessed only by one unique worker thread and incremented by 1 twice.
int ghost_l = 0; // not a phase ghost: accessed only by one unique worker thread, but decremented.
int ghost_m = 0; // not a phase ghost: incremented by both main and a different unique worker thread.
int ghost_n = 0; // not a phase ghost: updated by 0, so it is not an increment by 1.
int ghost_o = 0; // not a phase ghost: updated by the constant expression 1 - 1, which is not +1.
int ghost_p = 0; // not a phase ghost: assigned by a non-unique thread id.
int ghost_q = 4; // phase ghost: accessed only by main and updated from known constant 4 to 5.
int ghost_r = 4; // not a phase ghost: accessed only by main, but updated from 4 to 6.
int ghost_s = 0; // not exercised: never accessed in this test.
int ghost_t = 0; // not exercised: never accessed in this test.
int ghost_u = 0; // phase ghost: main first sets it from 0 to 1, then increments it by 1.
int ghost_v = 0; // not a phase ghost: main sets it from 0 to 2.
int ghost_w = 0; // phase ghost: worker_unique sets it from 0 to 1.
int ghost_x = 0; // not a phase ghost: incremented by a non-unique thread id.
int ghost_y = 0; // phase ghost: accessed only by main and incremented by 1 twice.

void *worker_unique(void *arg) {
  __VERIFIER_atomic_begin();
  ghost_g = ghost_g + 1;
  ghost_i = ghost_i + 1;
  ghost_j = 7;
  ghost_k = ghost_k + 1;
  ghost_k = ghost_k + 1;
  ghost_l = ghost_l - 1;
  ghost_m = ghost_m + 1;
  ghost_w = 1;
  __VERIFIER_atomic_end();
  return NULL;
}

void *worker_nonunique(void *arg) {
  __VERIFIER_atomic_begin();
  ghost_h = ghost_h + 1;
  ghost_p = 3;
  ghost_x = ghost_x + 1;
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  int i;
  pthread_t t_unique;
  pthread_t t_nonunique[2];

  __VERIFIER_atomic_begin();
  ghost_a = ghost_a + 1;
  ghost_b = 5;
  ghost_c = 1 + ghost_c;
  ghost_d = ghost_d + 2;
  ghost_e = ghost_e + __VERIFIER_nondet_int();
  ghost_g = ghost_g + 1;
  ghost_m = ghost_m + 1;
  ghost_n = ghost_n + 0;
  ghost_o = ghost_o + (1 - 1);
  ghost_q = 5;
  ghost_r = 6;
  ghost_u = 1;
  ghost_u = ghost_u + 1;
  ghost_v = 2;
  ghost_y = ghost_y + 1;
  ghost_y = ghost_y + 1;
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
