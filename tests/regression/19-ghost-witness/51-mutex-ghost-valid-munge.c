#include <pthread.h>
#include <stdio.h>

int g1 = 42, g2 = 37;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;

void munge1(pthread_mutex_t *m) {
  pthread_mutex_lock(m); // ghost_1 = 1
  g1 = 37;
  g1 = 42;
  pthread_mutex_unlock(m); // ghost_1 = 0
}

void munge2(pthread_mutex_t *m) {
  pthread_mutex_lock(m); // ghost_2 = 1
  g2 = 42;
  g2 = 37;
  pthread_mutex_unlock(m); // ghost_2 = 0
}

void *t_fun(void *arg) {
  munge1(&mutex2);
  munge2(&mutex3);
  return NULL;
}


int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge1(&mutex1);
  munge2(&mutex3);
  pthread_join (id, NULL);
  return 0;
}
