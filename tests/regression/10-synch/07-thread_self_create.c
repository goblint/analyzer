// PARAM: --set ana.activated[+] thread
// Checks termination of thread analysis with a thread who is its own single parent.
#include <pthread.h>

void *t_fun(void *arg) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  return 0;
}
