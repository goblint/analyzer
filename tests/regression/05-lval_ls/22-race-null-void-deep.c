#include <pthread.h>
#include <stdio.h>

pthread_key_t key;

void *t_fun(void *arg) {
  void *top;
  pthread_setspecific(key, top); // RACE
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
      pthread_setspecific(key, NULL); // NORACE
      break;
    case 1:
      pthread_setspecific(key, 0); // NORACE
      break;
    case 2:
      pthread_setspecific(key, zero); // NORACE
      break;
    case 3:
      pthread_setspecific(key, 1); // RACE
      break;
    case 4:
      pthread_setspecific(key, one); // RACE
      break;
    case 5:
      pthread_setspecific(key, r); // RACE
      break;
    case 6:
      pthread_setspecific(key, null); // NORACE
      break;
    case 7:
      pthread_setspecific(key, unknown); // RACE
      break;
    case 8:
      pthread_setspecific(key, top); // RACE
      break;
    default:
      break;
  }
  return 0;
}
