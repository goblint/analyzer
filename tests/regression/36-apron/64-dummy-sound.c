// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization top
#include <pthread.h>
#include <goblint.h>

int g;

void *t_fun(void *arg) {
  // shouldn't have g, x, y in local apron state
  g = 43;
  int *p = arg;
  *p = 11;
  return NULL;
}

int main() {
  g = 42;
  int x = 10;
  int y = 20;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, &x);

  // shouldn't have g, x, y in local apron state
  __goblint_check(g == 42); // UNKNOWN!
  __goblint_check(x == 10); // UNKNOWN!
  return 0;
}