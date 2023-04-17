// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
int c = -6;
int a = 2;
int b = 1;

void *g(void *arg) { a = c + 10; }

void main() {
  pthread_t id_thread;
  pthread_create(&id_thread, NULL, &g, NULL);
  if (3 < a) {
    int z = c;
    int y = a;
  } else {
    pthread_join(id_thread, NULL);
  }
  int y = b;
}