// PARAM: --set ana.activated[+] mutexGhosts --set ana.activated[+] pthreadMutexType
#include <pthread.h>
#include <goblint.h>

int g1;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m1);
  g1 = 1;
  g1 = 0;
  pthread_mutex_unlock(&m1);
  return NULL;
}

int main() {



  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m1);
  __goblint_check(g1 == 0);
  // no unlock
  return 0; // there should be no ghost updates for unlock here
}
