// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int counter;

void *f(void *arg) { counter = 7; }

void *g(void *arg) { counter = -12; }

void main() {
  pthread_t id_threadF;
  pthread_create(&id_threadF, NULL, &f, NULL);

  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);
}