// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *f(void *arg) { int z = 9; }

void *f_exit(void *arg) {
  int z = 12;
  pthread_exit(NULL);
}

int main() {
  int z = 0;
  while (z < 2) {
    pthread_create(NULL, NULL, &f, NULL);
    z = z + 1;
  }
  pthread_create(NULL, NULL, &f_exit, NULL);
  pthread_join(5, NULL);
  return 0;
}