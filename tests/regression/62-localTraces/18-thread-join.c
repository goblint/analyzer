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
  pthread_t id_thread;
  pthread_create(&id_thread, NULL, &f_exit, NULL);
  pthread_join(id_thread, NULL);
  return 0;
}