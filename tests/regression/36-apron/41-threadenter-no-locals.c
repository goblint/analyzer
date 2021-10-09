extern int __VERIFIER_nondet_int();

// SKIP PARAM: --set ana.activated[+] apron
#include <pthread.h>
#include <assert.h>

void *t_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); // threadenter shouldn't pass value for x here
  assert(x == 3); // UNKNOWN!
  return NULL;
}

int main(void) {
  int x = 3;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  return 0;
}
