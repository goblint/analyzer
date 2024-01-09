//PARAM: --set ana.activated[+] threadJoins --set lib.activated[+] klever
#include <pthread.h>
#include <goblint.h>

int g = 0;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create_N(&id, NULL, t_fun, NULL); // spawns multiple threads
  pthread_join(id, NULL);

  g++; // RACE!

  pthread_join_N(id, NULL); // TODO: should this join one (do nothing) or all (like assume join)?

  g++; // RACE!

  return 0;
}
