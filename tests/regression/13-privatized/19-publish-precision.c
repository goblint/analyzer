// PARAM: --set ana.int.interval true --set solver "'td3'"
#include<pthread.h>
#include<assert.h>

int glob1 = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  pthread_mutex_lock(&mutex2);
  glob1 = 5;

  pthread_mutex_unlock(&mutex2);
  pthread_mutex_lock(&mutex2);

  assert(glob1 == 5);
  glob1 = 0;

  pthread_mutex_unlock(&mutex2);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex2);
  assert(glob1 == 0); // UNKNOWN!
  assert(glob1 == 5); // UNKNOWN!
  pthread_mutex_unlock(&mutex2);
  pthread_join (id, NULL);
  return 0;
}
