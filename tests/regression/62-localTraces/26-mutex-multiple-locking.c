// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int counter;
pthread_mutex_t lock;

void *f(void *arg) {
  pthread_mutex_lock(&lock);
  counter = 7;
  pthread_mutex_unlock(&lock);
  int x = 3;
  pthread_mutex_lock(&lock);
  counter = 7;
  pthread_mutex_unlock(&lock);
}

void *g(void *arg) {
  pthread_mutex_lock(&lock);
  counter = -12;
  pthread_mutex_unlock(&lock);
  int y = 4;
  pthread_mutex_lock(&lock);
  counter = -12;
  pthread_mutex_unlock(&lock);
}

void main() {
  pthread_mutex_init(&lock, NULL);
  pthread_t id_threadF;
  pthread_create(&id_threadF, NULL, &f, NULL);

  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);

  pthread_mutex_destroy(&lock);
}