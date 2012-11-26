// PARAM: --set exp.failing-locks true
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  int i=3;
  pthread_create(&id, NULL, t_fun, NULL);
  if (pthread_mutex_lock(&mutex1)) {
    printf("Locking failed!");
    myglobal=myglobal+1; // RACE!
  } else 
    pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
