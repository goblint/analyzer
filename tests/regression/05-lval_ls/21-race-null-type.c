// PARAM: --enable ana.race.direct-arithmetic
#include <pthread.h>
#include <stdio.h>

void *t_fun(void *arg) {
  void *top;
  time(top); // RACE
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  int r; // rand
  int zero = 0; // IntDomain zero
  void *null;
  __goblint_assume(null == NULL); // AddressDomain NULL
  int one = 1; // IntDomain one
  void *unknown;
  __goblint_assume(unknown != NULL); // AddressDomain unknown
  void *top;
  switch (r) {
    case 0:
      time(NULL); // NORACE
      break;
    case 1:
      time(0); // NORACE
      break;
    case 2:
      time(zero); // NORACE
      break;
    case 3:
      time(1); // RACE
      break;
    case 4:
      time(one); // RACE
      break;
    case 5:
      time(r); // RACE
      break;
    case 6:
      time(null); // NORACE
      break;
    case 7:
      time(unknown); // RACE
      break;
    case 8:
      time(top); // RACE
      break;
    default:
      break;
  }
  return 0;
}
