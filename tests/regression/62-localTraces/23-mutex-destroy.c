// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int counter;
pthread_mutex_t lock;

void *g(void *arg) {
  pthread_mutex_lock(&lock);
  counter = -12;
}

void main() {
  pthread_mutex_init(&lock, NULL);
  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);
  counter = 3;
  pthread_join(id_threadG, NULL);
  pthread_mutex_destroy(&lock);  // WARN
}