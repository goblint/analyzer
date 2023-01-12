// PARAM: --enable witness.yaml.enabled --enable witness.invariant.accessed
#include <pthread.h>

int cleaner_start() {
  return 0;
}

void start_scan() {
  pthread_t tid;
  pthread_create(&tid, NULL, cleaner_start, NULL);
}

int main() {
  start_scan(); // FIXPOINT: mutex:start_scan
  return 0;
}
