// PARAM: --enable allglobs --set ana.activated[+] threadJoins
#include <stdlib.h>
#include <pthread.h>

void *t_benign(void *arg) {
  return NULL;
}

int main() {
  rand();
  pthread_t id;
  pthread_create(&id, NULL, t_benign, NULL);
  pthread_join(id, NULL);
  rand();
  return 0;
}