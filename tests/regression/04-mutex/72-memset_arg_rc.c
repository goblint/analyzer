#include <pthread.h>
#include <stdio.h>
#include <string.h>

int g;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

int main(void) {
  int x;
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  memset(&x, g, sizeof(int)); // RACE!
  pthread_join (id, NULL);
  return 0;
}
