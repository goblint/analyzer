// PARAM: --set ana.int.interval_set true --set solver new
// https://github.com/goblint/analyzer/pull/805#discussion_r933232518
#include<pthread.h>
#include <goblint.h>

int glob = 0;
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mtx);
  glob = 999;
  pthread_mutex_unlock(&mtx);
  return NULL;
}

int main() {
  int i = 3;
  pthread_t id;

  __goblint_check(glob == 0);

  // Create the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // Simple assignments to only locals
  __goblint_check(i == 3);
  i = 9;
  __goblint_check(i == 9);

  glob = 10;

  i = glob;
  __goblint_check(i >= 0);
  __goblint_check(i > 100); // UNKNOWN

  return 0;
}
