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
  pthread_t id_thread;
  pthread_t id_thread2;
  while (z < 2) {
    pthread_create(&id_thread, NULL, &f, NULL);
    z = z + 1;
  }
  pthread_create(&id_thread2, NULL, &f_exit, NULL);
  pthread_join(id_thread, NULL);
  return 0;
}