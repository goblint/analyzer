// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>
pthread_mutex_t lock;

void *f(void *arg) {
  pthread_mutex_lock(&lock);
  int x = 3;
  pthread_mutex_unlock(&lock);
}

void main() {
  pthread_mutex_init(&lock, NULL);
  pthread_t id_threadG;

  pthread_create(&id_threadG, NULL, &f, NULL);

  pthread_mutex_lock(&lock);
  int x = 7;
  pthread_mutex_unlock(&lock);

  pthread_join(id_threadG, NULL);

  pthread_mutex_lock(&lock);

  // pthread_mutex_destroy(&lock);
}