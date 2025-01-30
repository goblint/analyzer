// PARAM: --set ana.activated[+] mutexGhosts --set ana.malloc.unique_address_count 1
#include <pthread.h>
#include <goblint.h>

int g1, g2;
pthread_mutex_t *m1;
pthread_mutex_t *m2;

void *t_fun(void *arg) {
  pthread_mutex_lock(m1);
  g1 = 1;
  g1 = 0;
  pthread_mutex_unlock(m1);
  pthread_mutex_lock(m2);
  g2 = 1;
  g2 = 0;
  pthread_mutex_unlock(m2);
  return NULL;
}

int main() {
  m1 = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(m1, NULL);
  m2 = malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(m2, NULL);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(m1);
  __goblint_check(g1 == 0);
  pthread_mutex_unlock(m1);
  pthread_mutex_lock(m2);
  __goblint_check(g2 == 0);
  pthread_mutex_unlock(m2);
  return 0;
}
