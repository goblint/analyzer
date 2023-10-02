// PARAM: --disable sem.unknown_function.invalidate.globals --disable sem.unknown_function.spawn
#include <pthread.h>

struct s {
  int f;
};

extern void magic(struct s *p);

void *t_fun(void *arg) {
  void *top;
  magic(top); // RACE
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
      magic(NULL); // NORACE
      break;
    case 1:
      magic(0); // NORACE
      break;
    case 2:
      magic(zero); // NORACE
      break;
    case 3:
      magic(1); // RACE
      break;
    case 4:
      magic(one); // RACE
      break;
    case 5:
      magic(r); // RACE
      break;
    case 6:
      magic(null); // NORACE
      break;
    case 7:
      magic(unknown); // RACE
      break;
    case 8:
      magic(top); // RACE
      break;
    default:
      break;
  }
  return 0;
}
