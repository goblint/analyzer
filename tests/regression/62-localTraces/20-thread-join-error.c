// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *f_exit(void *arg) {
  int z = 12;
  pthread_exit(NULL);
}

void *f(void *arg) {
  int z = 9;
  pthread_create(NULL, NULL, &f_exit, NULL);

  pthread_join(7, NULL);
  pthread_join(7, NULL);
}

int main() {
  int z = 0;
  pthread_create(NULL, NULL, &f, NULL);

  pthread_join(40000, NULL);
  return 0;
}