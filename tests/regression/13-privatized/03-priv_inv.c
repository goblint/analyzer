#include<pthread.h>
#include<assert.h>

int x = 1;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  if (x == 1) { // NORACE
    pthread_mutex_unlock(&mutex1);
    return NULL;
  }
  pthread_mutex_unlock(&mutex1);
  x = -1; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(x == 1);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  x = 0; // NORACE
  assert(x == 0);
  x = 1; // NORACE
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
