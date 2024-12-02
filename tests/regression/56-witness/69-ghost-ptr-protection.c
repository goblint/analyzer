// PARAM: --set ana.activated[+] mutexGhosts
// CRAM
#include <pthread.h>
#include <goblint.h>

int g = 0;
int *p = &g;
pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = 10;
  pthread_mutex_lock(&m2);
  p = &x;
  p = &g;
  pthread_mutex_unlock(&m2);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m1);
  g = 1;
  // m2_locked || (p == & g && *p == 0) would be violated here
  __goblint_check(*p != 0); // 1 from g or 10 from x in t_fun
  g = 0;
  pthread_mutex_unlock(&m1);
  return 0;
}
