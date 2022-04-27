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
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  memset(&g, 0, sizeof(int)); // RACE!
  pthread_join (id, NULL);
  return 0;
}
