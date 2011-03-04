#include<pthread.h>
#include<assert.h>

int glob1 = 5;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&mutex1);
  t = glob1;
  assert(t == 5); // UNKNOWN
  glob1 = -10;
  assert(glob1 == -10); // UNKNOWN
  glob1 = t;
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(glob1 == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex2);
  glob1++; 
  assert(glob1 == 6); // UNKNOWN
  glob1--;
  pthread_mutex_unlock(&mutex2);
  pthread_join (id, NULL);
  return 0;
}
