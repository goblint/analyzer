// PARAM: --set ana.activated[+] thread --set ana.path_sens[+] threadflag --set ana.path_sens[+] thread
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
  int x1;
  pthread_t id1;
  if (x1)
    pthread_create(&id1, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);

  int x2;
  pthread_t id2;
  if (x2)
    pthread_create(&id2, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);

  if (x1)
    pthread_join (id1, NULL);

  pthread_mutex_lock(&mutex);
  myglobal=myglobal+1; // NORACE
  pthread_mutex_unlock(&mutex);

  if (x2)
    pthread_join (id2, NULL);

  myglobal=myglobal+1; // NORACE
  return 0;
}
