// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int c = 2;
int z = 7;

void *f(void *arg) {
  c = z + 3;
  // int x = c;
}

void *g(void *arg) {
  z = c - 2;
  // int y = z;
}

void main() {
  pthread_t id_threadF;
  pthread_create(&id_threadF, NULL, &f, NULL);

  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);
}