// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 1;

void *t_fun(void *arg) {
  g = 2; // write something non-initial so base wouldn't find success
  return NULL;
}

int main(void) {
  int x = __VERIFIER_nondet_int(); // rand
  int y = __VERIFIER_nondet_int(); //rand

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  x = g;
  y = g;
  // unlock(m_g)-s must forget relation with unprotected
  assert(x == y); // UNKNOWN!
  return 0;
}
