// PARAM: --set ana.int.interval true --set ana.base.arrays.domain partitioned
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>

int *x;
int *y;

void *t_fun(void *arg) {
  *x = 3;
  return NULL;
}

int main() {
  pthread_t id;

  x = calloc(1, sizeof(int));
  y = calloc(1, sizeof(int));

  *x = 0;
  *y = 1;

  __goblint_check(*x == 0);
  __goblint_check(*y == 1); // UNKNOWN

  pthread_create(&id, NULL, t_fun, NULL);

  __goblint_check(*x == 0); // UNKNOWN
  __goblint_check(*y == 1); // UNKNOWN

  return 0;
}
