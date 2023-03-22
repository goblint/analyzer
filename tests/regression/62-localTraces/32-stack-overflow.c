// PARAM: --set ana.activated[+] "localTraces"
#include <goblint.h>
#include <pthread.h>
int c = -6;
int a = 2;
int b = 1;

void *g(void *arg) { a = c + 10; }
int f(int x, int y) { return x - y; }

void main() {
  pthread_t id_thread;
  pthread_create(&id_thread, NULL, &g, NULL);
  // int z = f(a + 12, c - 4);
  if (3 < a) {
    int z = c;
    int y = a;
  } else {
    pthread_join(id_thread, NULL);
  }
  int y = b;
}