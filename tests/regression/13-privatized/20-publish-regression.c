// PARAM: --set ana.int.interval true

#include<pthread.h>
#include<assert.h>

int glob1 = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

// The question is how to compute these S[g] sets?
// They are given in the paper. Should it be as large as possible?

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&mutex2);
  glob1 = 5;
  pthread_mutex_unlock(&mutex2);
  // But if s[g] = {mutex1,mutex2}, we publish here.
  pthread_mutex_lock(&mutex2);
  __goblint_check(glob1 == 5);
  glob1 = 0;
  pthread_mutex_unlock(&mutex1);
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  __goblint_check(glob1 == 0);
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
