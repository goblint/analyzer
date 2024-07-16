#include <pthread.h>
#include <goblint.h>

extern void* magic(void* arg);
int g;

int main() {
  pthread_t id;
  pthread_create(&id, NULL, magic, NULL);

  __goblint_check(g == 0); // UNKNOWN! (magic may invalidate)
  return 0;
}
