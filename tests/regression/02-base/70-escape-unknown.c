#include <pthread.h>
#include <assert.h>

int *p;

void *t_fun(void *arg) {
  if (arg != NULL) {
    *((int*)arg) = 42;
  }
  return NULL;
}

int main() {
  pthread_t id, id2;
  int *r; // unknown
  int i = 5;

  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded

  p = r;
  p = &i;

  pthread_create(&id2, NULL, t_fun, p); // i should escape, even if p contains unknown

  assert(i == 5); // UNKNOWN!

  return 0;
}