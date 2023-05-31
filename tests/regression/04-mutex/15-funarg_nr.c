
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int add1 (int x) {
  return x+1;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex1);
  printf("myglobal equals %d\n",myglobal); // NORACE
  pthread_mutex_unlock(&mutex1);

  pthread_mutex_lock(&mutex1);
  add1(myglobal); // NORACE
  pthread_mutex_unlock(&mutex1);

  pthread_join (id, NULL);
  return 0;
}
