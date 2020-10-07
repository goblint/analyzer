#include <pthread.h>

int g1,g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  g1++; // NORACE
  pthread_mutex_unlock(&mutex1);

  pthread_mutex_lock(&mutex2);
  g2++; // NORACE
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex1);
  while(pthread_mutex_trylock(&mutex2)){
    pthread_mutex_unlock(&mutex1);
    pthread_mutex_lock(&mutex1);
  }

  g1=g2+1; // NORACE

  pthread_mutex_unlock(&mutex2);
  pthread_mutex_unlock(&mutex1);

  pthread_join (id, NULL);
  return 0;
}

