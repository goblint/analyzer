// PARAM: --set ana.activated[+] useAfterFree
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>

int *g;
uintptr_t hidden;

// t frees g's object. main reads it back through an integer, so the pointer
// analysis does not know p's target, which may be the object t freed.
static void *t(void *arg) {
  free(g);
  return NULL;
}

int main(void) {
  g = malloc(sizeof(int));
  hidden = (uintptr_t) g;
  pthread_t th;
  pthread_create(&th, NULL, t, NULL);
  pthread_join(th, NULL);
  int *p = (int *) hidden;
  if (p == NULL)
    return 1;
  *p = 2; // WARN
  return 0;
}
