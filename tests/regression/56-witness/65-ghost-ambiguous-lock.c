// PARAM: --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType
#include <pthread.h>
#include <goblint.h>

int g1, g2;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m1);
  g1 = 1;
  g1 = 0;
  pthread_mutex_unlock(&m1);
  pthread_mutex_lock(&m2);
  g2 = 1;
  g2 = 0;
  pthread_mutex_unlock(&m2);
  return NULL;
}

void fun(pthread_mutex_t *m) {
  pthread_mutex_lock(m);
  // what g2 can read?
  pthread_mutex_unlock(m);
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_t *m;
  int r; // rand
  m = r ? &m1 : &m2;

  pthread_mutex_lock(m);
  // what g1 can read?
  pthread_mutex_unlock(m);

  if (r)
    fun(&m1);
  else
    fun(&m2);
  return 0;
}
