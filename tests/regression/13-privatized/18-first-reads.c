// PARAM: --set ana.int.interval true --set solver "'td3'"
#include<pthread.h>
#include<assert.h>

int glob1 = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&mutex1);
  if(t == 42) {
      glob1 = 1;
  }
  t = glob1;

  assert(t == 0); //UNKNOWN

  assert(t == 1); //UNKNOWN

  glob1 = 0;

  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  assert(glob1 == 0);
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
