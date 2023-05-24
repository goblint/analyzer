// PARAM: --set ana.int.interval_set true
#include<pthread.h>
#include <goblint.h>

int glob1 = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  glob1 = 5;
  __goblint_check(glob1 == 5);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  __goblint_check(glob1 == 0); // UNKNOWN!
  __goblint_check(glob1 == 5); // UNKNOWN!
  pthread_join (id, NULL);
  return 0;
}
