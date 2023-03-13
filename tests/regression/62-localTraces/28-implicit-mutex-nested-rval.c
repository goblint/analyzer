// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

int x = 2;
int y = -10;
int z = 7;

void *f(void *arg) {
  y = x + 3;
  int local = y;
}

void *g(void *arg) {
  z = y - 2;
  int local = z;
}

void *h(void *arg) {
  x = z + 26;
  int local = x;
}

void main() {
  pthread_t id_threadF;
  pthread_create(&id_threadF, NULL, &f, NULL);

  pthread_t id_threadG;
  pthread_create(&id_threadG, NULL, &g, NULL);

  pthread_t id_threadH;
  pthread_create(&id_threadH, NULL, &h, NULL);
}