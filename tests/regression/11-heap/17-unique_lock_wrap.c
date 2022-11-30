// PARAM: --set ana.malloc.unique_address_count 1
#include <pthread.h>
#include <stdlib.h>

struct { pthread_mutex_t mutex; } *mylock;

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

typedef void *(*stdThreadRoutine)(void *);


static void *internal_start(void *args){
    stdThreadRoutine start = (stdThreadRoutine)args;
    start(NULL);
    return NULL;
}

void stdThreadCreate(stdThreadRoutine start)
{
    pthread_t handle;
    pthread_create(&handle, NULL, internal_start, start);
}



int myglobal;


void *t_fun(void *arg) {
  pthread_mutex_lock(&mylock->mutex);
  myglobal=1; // NORACE
  pthread_mutex_unlock(&mylock->mutex);
  return NULL;
}

int main(void) {
  pthread_t id1, id2;
   
  mylock = malloc(sizeof(*mylock));
  pthread_mutex_init(&mylock->mutex, NULL);

  pthread_create(&id1, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  
  stdThreadCreate(t_fun);
  stdThreadCreate(t_fun); // commenting out one of these makes it work!?
     
  return 0;
}
