#include <pthread.h>
#include <stdio.h>

int myglobal1;
int myglobal2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m, int *v) {
  pthread_mutex_lock(m);
  *v=*v+1; // NORACE
  pthread_mutex_unlock(m);
}

void *t_fun(void *arg) {
  munge(&mutex1, &myglobal1);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge(&mutex2,&myglobal2);
  pthread_join (id, NULL);
  return 0;
}
