#include <pthread.h>
#include <stdio.h>
#include <assert.h>

__thread int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int top;
  int* ptr = (int*) arg;

  if(top) { 
    ptr = &myglobal;
  }

  __goblint_check(&myglobal == ptr); //UNKNOWN!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, (void*) &myglobal);
  pthread_join (id, NULL);
  return 0;
}