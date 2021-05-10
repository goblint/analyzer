// PARAM: --sets ana.activated[+] thread --sets ana.path_sens[+] threadflag
#include <pthread.h>
#include <stdio.h>

int myglobal;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;

  int x;
  if (x) {
    pthread_create(&id, NULL, t_fun, NULL);
    int y = 5;
  }

  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);

  if (x) {
    pthread_join (id, NULL);
    int z = 5;
  }
  else {
    int w = 5;
  }

  myglobal=myglobal+1; // NORACE
  return 0;
}
