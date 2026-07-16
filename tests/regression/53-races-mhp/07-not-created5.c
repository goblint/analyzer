// PARAM: --set ana.activated[+] mhp --disable ana.thread.include-node
#include <pthread.h>

int g;

void *a(void *arg) {
  int *gp = arg;
  if (gp)
    (*gp)++; // RACE (self-race in non-unique thread)
  return NULL;
}

void *b(void *arg) {
  pthread_t id, id2;
  pthread_create(&id, NULL, a, NULL);
  pthread_create(&id2, NULL, a, &g);
  return NULL;
}

int main() {
  pthread_t id, id2, id3;
  pthread_create(&id, NULL, a, NULL);
  pthread_create(&id, NULL, a, NULL);
  g++; // NORACE
  pthread_create(&id, NULL, b, NULL);
  return 0;
}
