// PARAM: --set ana.activated[+] mutexGhosts
#include <pthread.h>
#include <goblint.h>
int g1, g2;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;
// CRAM
void *t_fun(void *arg) {
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  g1 = 1;
  g1 = 0;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);

  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  g2 = 1;
  pthread_mutex_unlock(&m2);
  pthread_mutex_lock(&m2);
  g2 = 0;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  /* pthread_mutex_lock(&m1);
  __goblint_check(g1 == 0); // NOWARN (commented out)
  __goblint_check(g2 == 0); // NOWARN (commented out)
  pthread_mutex_unlock(&m1);

  pthread_mutex_lock(&m2);
  __goblint_check(g1 == 0); // NOWARN (commented out)
  __goblint_check(g2 == 0); // NOWARN (commented out)
  pthread_mutex_unlock(&m2); */
  return 0;
}
