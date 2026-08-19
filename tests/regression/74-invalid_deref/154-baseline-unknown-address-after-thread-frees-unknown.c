// PARAM: --set ana.activated[+] useAfterFree
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>

uintptr_t hidden;

// t frees the object hidden holds the address of, through a pointer whose
// target the pointer analysis does not know, and main then uses it the same
// way.
static void *t(void *arg) {
  free((int *) hidden);
  return NULL;
}

int main(void) {
  hidden = (uintptr_t) malloc(sizeof(int));
  pthread_t th;
  pthread_create(&th, NULL, t, NULL);
  pthread_join(th, NULL);
  int *p = (int *) hidden;
  if (p == NULL)
    return 1;
  *p = 2; // WARN
  return 0;
}
