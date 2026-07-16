// PARAM: --set ana.base.privatization protection --enable ana.sv-comp.functions
#include <pthread.h>
#include <goblint.h>

int g;
pthread_mutex_t m[2] = {PTHREAD_MUTEX_INITIALIZER, PTHREAD_MUTEX_INITIALIZER};

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[0]);
  g++;
  pthread_mutex_t *r; // rand
  pthread_mutex_unlock(r);
  // could have unlocked m[0], so should have published g there
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m[0]);
  __goblint_check(g == 0); // UNKNOWN!
  pthread_mutex_unlock(&m[0]);
  return 0;
}
