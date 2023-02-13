// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
#include <stdio.h>

void *f_exit(void *arg) {
  int unknown;
  int z = unknown;
  pthread_exit(NULL);
}

int main() {
  pthread_create(NULL, NULL, &f_exit, NULL);
  pthread_join(3, NULL);
  return 0;
}