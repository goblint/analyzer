// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
int c = -6;
int a = 2;

void *g(void *arg) { a = 10; }
int f(int x, int y) { return x - y; }

void main() {
  pthread_t id_thread;
  pthread_create(&id_thread, NULL, &g, NULL);
  int z = f(a + 12, c - 4);
}
