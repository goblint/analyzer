// SKIP PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g;

void *t_fun(void *arg) {
  return NULL;
}

int main(void) {
  int x, y, r;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  g = r;

  x = g;
  y = g;
  assert(x == y); // TODO (like 13/66)
  return 0;
}
