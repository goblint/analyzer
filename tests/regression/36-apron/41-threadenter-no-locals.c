// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <assert.h>

void *t_fun(void *arg) {
  int x; // threadenter shouldn't pass value for x here
  assert(x == 3); // UNKNOWN!
  return NULL;
}

int main(void) {
  int x = 3;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  return 0;
}
