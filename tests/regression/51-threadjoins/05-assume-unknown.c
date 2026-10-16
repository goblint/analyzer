//PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>
#include <goblint.h>

int g = 0;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main() {
  int top;
  pthread_t id, id2;

  if(top) {
    pthread_create(&id2, NULL, t_fun, NULL);
  }

  __goblint_assume_join(id2); // WARN joining unknown thread ID, continue with known threads

  g++; // NORACE

  pthread_create(&id, NULL, t_fun, NULL);
  g++; // RACE!

  return 0;
}
