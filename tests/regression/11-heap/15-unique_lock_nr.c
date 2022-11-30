// PARAM: --set ana.malloc.unique_address_count 1
#include <pthread.h>
#include <stdio.h>

struct { pthread_mutex_t mutex; } *mylock;
int myglobal;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mylock->mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mylock->mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  mylock = malloc(sizeof(*mylock));
  pthread_mutex_init(&mylock->mutex, NULL);

  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mylock->mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mylock->mutex);
  pthread_join (id, NULL);
  return 0;
}
