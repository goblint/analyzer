//PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>
#include <goblint.h>

int g = 0;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);

  __goblint_assume_join(id2); // WARN joining unknown thread ID, make joined set All threads

  g++; // NORACE

  pthread_create(&id, NULL, t_fun, NULL); // WARN make joined set different from All threads
  g++; // RACE!

  return 0;
}
