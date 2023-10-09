#include <pthread.h>
#include <stdio.h>

void *t_fun(void *arg) {
  void **top;
  free(top); // RACE
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
      pthread_join(id, NULL); // NORACE
      break;
    case 1:
      pthread_join(id, 0); // NORACE
      break;
    case 2:
      pthread_join(id, zero); // NORACE
      break;
    case 3:
      pthread_join(id, 1); // RACE
      break;
    case 4:
      pthread_join(id, one); // RACE
      break;
    case 5:
      pthread_join(id, r); // RACE
      break;
    case 6:
      pthread_join(id, null); // NORACE
      break;
    case 7:
      pthread_join(id, unknown); // RACE
      break;
    case 8:
      pthread_join(id, top); // RACE
      break;
    default:
      break;
  }
  return 0;
}
