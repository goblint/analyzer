// SKIP PARAM: --set ana.base.privatization none --set ana.activated[+] apron --set witness.yaml.entry-types[*] location_invariant --set ana.activated[+] unassume --set witness.yaml.unassume 24-apron-unassume-priv2.yml  --set solvers.td3.side_widen always --enable ana.widen.tokens
#include <pthread.h>
#include <assert.h>

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);

  if (g < 10)
    g++;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  assert(g >= 0);
  assert(g <= 10);
  pthread_mutex_unlock(&A);
  return 0;
}
