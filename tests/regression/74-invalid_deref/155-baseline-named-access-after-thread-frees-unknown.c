// PARAM: --set ana.activated[+] useAfterFree
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>

uintptr_t hidden;

// t frees, through a pointer whose target the pointer analysis does not know,
// the object main allocated into p. main's own path frees nothing after p's
// allocation, which does not show p's object live: t's free is not on that
// path.
static void *t(void *arg) {
  free((int *) hidden);
  return NULL;
}

int main(void) {
  int *p = malloc(sizeof(int));
  hidden = (uintptr_t) p;
  pthread_t th;
  pthread_create(&th, NULL, t, NULL);
  pthread_join(th, NULL);
  *p = 1; // WARN
  return 0;
}
