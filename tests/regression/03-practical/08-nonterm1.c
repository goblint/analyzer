#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void fun(void) {
  int i;
  pthread_mutex_lock(&mutex2);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex2);
  while (1) i++;
  // return;
}

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  fun();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  myglobal=myglobal+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  printf("myglobal equals %d\n",myglobal);
  return 0;
}
