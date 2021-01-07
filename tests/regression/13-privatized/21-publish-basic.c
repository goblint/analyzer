// PARAM: --set ana.int.interval true --set solver "'td3'"
#include<pthread.h>
#include<assert.h>

int glob1 = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&mutex);
  glob1 = 5;
  assert(glob1 == 5);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  assert(glob1 == 0); // UNKNOWN!
  assert(glob1 == 5); // UNKNOWN!
  pthread_join (id, NULL);
  return 0;
}
