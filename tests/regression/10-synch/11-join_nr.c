// SKIP PARAM: --sets ana.activated[+] thread
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; //NOWARN
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; //NOWARN
  pthread_mutex_unlock(&mutex);
  pthread_join (id, NULL);
  myglobal=myglobal+1; //NOWARN
  return 0;
}
