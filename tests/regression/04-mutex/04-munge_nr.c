#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m) {
  pthread_mutex_lock(m);
  myglobal=myglobal+1; // NOWARN!
  pthread_mutex_unlock(m);
}

void *t_fun(void *arg) {
  munge(&mutex); // NOWARN!
  return NULL;
}


int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge(&mutex); // NOWARN!
  pthread_join (id, NULL);
  return 0;
}
