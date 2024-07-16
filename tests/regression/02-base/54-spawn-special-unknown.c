#include <pthread.h>
#include <goblint.h>

int g;

int main() {
  void (*unknown)(void*);

  pthread_t id;
  pthread_create(&id, NULL, unknown, NULL);

  __goblint_check(g == 0); // UNKNOWN! (unknown thread may invalidate)
  return 0;
}
