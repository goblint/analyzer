// SKIP: Low Priority -- this compiles with warnings, but goblint crashes.
#include <pthread.h>

int data[10];

void *t_fun(int i) {
  int *x = &data[i];
  return NULL;
}

int main() {
  pthread_t id;
  int n = 0;
  pthread_create(&id, NULL, t_fun, (int*) n);
  return 0;
}
