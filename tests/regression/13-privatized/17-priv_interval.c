// PARAM: --set ana.int.interval true
#include<pthread.h>
#include<assert.h>

int glob1 = 5;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&mutex1);
  t = glob1;
  __goblint_check(t == 5);
  glob1 = -10;
  __goblint_check(glob1 == -10);
  glob1 = t;
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(glob1 == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  glob1++;
  __goblint_check(glob1 == 6);
  glob1--;
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
