#include <pthread.h>
#include <stdio.h>

void *t_fun(void *arg) {
  void *top;
  fclose(top); // RACE
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
      feof(NULL); // NORACE
      break;
    case 1:
      feof(0); // NORACE
      break;
    case 2:
      feof(zero); // NORACE
      break;
    case 3:
      feof(1); // RACE
      break;
    case 4:
      feof(one); // RACE
      break;
    case 5:
      feof(r); // RACE
      break;
    case 6:
      feof(null); // NORACE
      break;
    case 7:
      feof(unknown); // RACE
      break;
    case 8:
      feof(top); // RACE
      break;
    default:
      break;
  }
  return 0;
}
