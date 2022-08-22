//PARAM: --set ana.activated[+] threadJoins --disable ana.thread.include-node --set ana.thread.domain plain
#include <pthread.h>
#include <assert.h>

int g = 0;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  __goblint_assume_join(id); // should add to must-joined

  pthread_create(&id, NULL, t_fun, NULL); // should remove from must-joined again

  g++; // RACE!

  return 0;
}
