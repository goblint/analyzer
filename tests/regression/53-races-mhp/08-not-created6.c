// PARAM: --set ana.activated[+] mhp --disable ana.thread.include-node
#include <pthread.h>

int g;

void *b(void *arg) {
  return NULL;
}

void *c(void *arg) {
  int *gp = arg;
  if (gp)
    (*gp)++; // RACE (self-race in non-unique thread)
  return NULL;
}

void *a(void *arg) {
  pthread_t id, id2, id3, id4;
  pthread_create(&id, NULL, b, NULL);
  pthread_create(&id2, NULL, b, NULL);
  g++; // NORACE
  pthread_create(&id, NULL, c, NULL);
  pthread_create(&id2, NULL, c, &g);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, a, NULL);
  return 0;
}
