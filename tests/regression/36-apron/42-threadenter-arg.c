// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <assert.h>

void *t_fun(int arg) {
  __goblint_check(arg == 3); // TODO (cast through void*)
  return NULL;
}

int main(void) {
  int x = 3;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, x);

  return 0;
}
