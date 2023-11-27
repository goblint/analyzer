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

void incr() {
  myglobal=myglobal+1; // RACE!
}

void wrap() {
  pthread_mutex_lock(&mutex2);
  incr();
  pthread_mutex_unlock(&mutex2);
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  wrap();
  pthread_join (id, NULL);
  return 0;
}
