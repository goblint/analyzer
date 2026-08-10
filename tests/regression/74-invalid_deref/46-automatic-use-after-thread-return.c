// PARAM: --set ana.activated[+] useAfterFree --set ana.activated[+] threadJoins

#include <pthread.h>

static int *escaped;

static void *worker(void *arg) {
  int local = 42;
  escaped = &local;
  return 0;
}

int main(void) {
  pthread_t thread;
  pthread_create(&thread, 0, worker, 0);
  pthread_join(thread, 0);

  int value = *escaped; // WARN (the worker's automatic storage has ended)
  return value;
}
