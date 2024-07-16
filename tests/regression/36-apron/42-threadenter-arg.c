// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <goblint.h>

void *t_fun(int arg) { // check that apron doesn't crash with tracked thread argument
  __goblint_check(arg == 3); // cast through void*, passes by base
  return NULL;
}

int main(void) {
  int x = 3;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, x);

  return 0;
}
