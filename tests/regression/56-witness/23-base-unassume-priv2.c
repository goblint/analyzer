// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 23-base-unassume-priv2.yml --set solvers.td3.side_widen always
#include <pthread.h>
#include <assert.h>

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  assert(1); // TODO: fix Lock and Unassume event order so this extra node isn't necessary
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
  assert(g <= 10); // TODO: widening tokens on globals
  pthread_mutex_unlock(&A);
  return 0;
}
