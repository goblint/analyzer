#include <pthread.h>

void *t_fun(void *arg) {
  int *iarg = (int*) arg;
  *iarg = 1; // RACE (without mallocFresh)
  return NULL;
}

int main() {
  pthread_t id;
  int *iarg;

  for (int i = 0; i < 10; i++) {
    iarg = malloc(sizeof(int));
    *iarg = 0; // RACE (without mallocFresh)
    pthread_create(&id, NULL, t_fun, (void*) iarg);
  }

  return 0;
}