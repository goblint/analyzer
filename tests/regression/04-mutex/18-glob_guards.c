#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  if (myglobal) // NOWARN!
    printf("How do you do?\n");
  return NULL;
}

int main(void) {
  pthread_t id;
  // invalidate myglobal
  int k;
  myglobal = k;
  // create thread
  pthread_create(&id, NULL, t_fun, NULL);
  if (myglobal)  // NOWARN!
    printf("Hello!\n");
  pthread_join (id, NULL);
  return 0;
}
