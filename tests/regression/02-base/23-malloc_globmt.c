// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mutexEvents','mutex','access','mallocWrapper','assert']"
#include <stdlib.h>
#include <pthread.h>
#include <assert.h>

int *x;
int *y;

void *t_fun(void *arg) {
  *x = 3;
  return NULL;
}

int main() {
  pthread_t id;

  x = malloc(sizeof(int));
  y = malloc(sizeof(int));

  *x = 0;
  *y = 1;

  __goblint_check(*x == 0);
  __goblint_check(*y == 1);

  pthread_create(&id, NULL, t_fun, NULL);

  __goblint_check(*x == 0); // UNKNOWN
  __goblint_check(*y == 1);

  return 0;
}
