#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m) {
  pthread_mutex_lock(m);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(m);
}

void *t_fun(void *arg) {
  munge(&mutex2);
  return NULL;
}


int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
