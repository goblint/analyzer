// PARAM: --enable solvers.td3.narrow-sides.enabled --enable ana.int.interval --enable solvers.td3.narrow-sides.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>

int b = 0;
int *a = &b;

void g (int k, int *i) {
  if (k > 20) {
    a = &i;
  }
}

void* f(void *d) {
  int i = 0;

  for(int j = 0, k = 0; j < 10; j++) {
    g(k, &i);
    k = j;
  }
  i++;

  __goblint_check(i == 1);
  __goblint_check(a != &i);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);

  // a is thought to (possibly) point to the *flow-insensitively* tracked i,
  // which is widened by the i++.
  __goblint_check(*a <= 1);

  return 0;
}
