#include <pthread.h>
#include <stdio.h>

int myglobal;
int debug;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  debug = 0; // NORACE
  myglobal=myglobal+1; // NORACE
  pthread_create(&id, NULL, t_fun, NULL);
  debug++; // NORACE
  return 0;
}
