// SKIP PARAM: --set ana.activated[+] apron
#include <pthread.h>
#include <goblint.h>

extern int g;

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  if (g) // NOWARN
    __goblint_check(1); // reachable
  else
    __goblint_check(1); // reachable

  return 0;
}
