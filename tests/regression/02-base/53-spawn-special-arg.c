#include <pthread.h>
#include <goblint.h>

extern void* magic(void* arg);

int main() {
  int x = 0;
  pthread_t id;
  pthread_create(&id, NULL, magic, &x);

  __goblint_check(x == 0); // UNKNOWN! (magic may invalidate)
  return 0;
}
