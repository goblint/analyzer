// PARAM: --set ana.activated[+] mhp --disable ana.thread.include-node
#include <pthread.h>

int g;

void *b(void *arg) {
  int *gp = arg;
  if (gp)
    (*gp)++; // NORACE
  return NULL;
}

void *a(void *arg) {
  pthread_t id;
  pthread_create(&id, NULL, b, arg);
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, b, NULL);
  g++; // NORACE
  pthread_create(&id2, NULL, a, &g);
  return 0;
}
