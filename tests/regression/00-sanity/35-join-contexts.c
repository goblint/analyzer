// SKIP PARAM: --set ana.ctx_insens[+] threadflag --set ana.ctx_insens[+] threadid --set ana.ctx_insens[+] base
// Fully context-insensitive
#include <goblint.h>
#include <pthread.h>

int g = 1;

void foo() {
  // Single-threaded: g = 1 in local state
  // Multi-threaded: g = 2 in global unprotected invariant
  // Joined contexts: g is unprotected, so read g = 2 from global unprotected invariant (only)
  // Currently unsoundly claim that check will succeed!
  int x = g;
  __goblint_check(x == 2); // TODO UNKNOWN!
}

void *t_fun(void *arg) {
  foo();
}

int main() {
  foo();
  g = 2;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  return 0;
}
