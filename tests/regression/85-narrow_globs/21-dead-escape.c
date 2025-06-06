// PARAM: --enable solvers.td3.narrow-globs.enabled --enable ana.int.interval --enable solvers.td3.narrow-globs.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>

int b = 0;
int *a = &b;

void* f(void *d) {
  int i = 0;

  for(int j = 0, k = 0; j < 10; j++) {
    if (k > 20) {
      a = &i;
    }
    k = j;
  }
  i++;

  // The unknowns for protected:i etc. exist.
  // Also, i is in the global set of escapees.
  // However, locally i is known not to have escaped,
  // so none of these unknowns are queried and this check
  // succeeds whether eliminate-dead is on or not.
  __goblint_check(i == 1);
  // a != &i is not known without eliminate-dead
  // The pointer has escaped at some point and was recorded at the global a.
  // We don't filter points-to-sets read from globals to exclude things which are locally known  (by the ThreadEscape analysis) to not have escaped.
  // I don't think this is particularly common, and iterating and querying for each lval is probably just expensive.
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
