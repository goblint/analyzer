// PARAM: --set ana.malloc.unique_address_count 1
#include <pthread.h>
#include <stdlib.h>

struct { pthread_mutex_t mutex; } *mylock;
int myglobal;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mylock->mutex);
  myglobal=1; // NORACE
  pthread_mutex_unlock(&mylock->mutex);
  return NULL;
}

int main(void) {
  pthread_t id[10];
   
  mylock = malloc(sizeof(*mylock));
  pthread_mutex_init(&mylock->mutex, NULL);

  for (int i; i<10; i++) {
    pthread_create(&id[i], NULL, t_fun, NULL);
  }
       
  return 0;
}
