// PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions
#include <goblint.h>
#include <pthread.h>

static int *target;

static void *modify(void *arg) {
  *target = __VERIFIER_nondet_int();
  return NULL;
}

static void enter_multithreaded_and_modify(void) {
  pthread_t thread;
  pthread_create(&thread, NULL, modify, NULL);
  pthread_join(thread, NULL);
}

int main(void) {
  int left = __VERIFIER_nondet_int();
  int right = left;
  target = &right;

  enter_multithreaded_and_modify();
  __goblint_check(left == right); // UNKNOWN!
}
