// SKIP PARAM: --set ana.base.privatization write+lock --enable ana.int.interval --set witness.yaml.validate 04-base-priv-sync-prune.yml
// TODO: fix unsoundness in base priv syncs
#include <pthread.h>

int g = 0;

void *t_fun(void *arg) {
  ; // FAIL (witness)
  return NULL;
}

int main() {
  int r, r2; // rand

  if (r)
    g = 1;

  pthread_t id;
  for (int i = 0; i < r2; r++)
    pthread_create(&id, NULL, t_fun, NULL);
  return 0;
}
