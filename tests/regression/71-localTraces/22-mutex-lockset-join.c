// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int counter;
pthread_mutex_t lock;
pthread_mutex_t other_lock;

void *f(void *arg) {
  pthread_mutex_lock(&lock);
  counter = 7;
  pthread_mutex_lock(&other_lock);
  pthread_mutex_unlock(&lock);
}

void *g(void *arg) {
  pthread_mutex_lock(&lock);
  counter = -12;
  pthread_mutex_unlock(&lock);
}

void main() {
  pthread_mutex_init(&lock, NULL);
  pthread_mutex_init(&other_lock, NULL);
  pthread_t id_threadF;
  pthread_create(&id_threadF, NULL, &f, NULL);

  pthread_join(id_threadF, NULL);

  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);
  pthread_mutex_destroy(&lock);
  pthread_mutex_destroy(&other_lock);  // WARN
}