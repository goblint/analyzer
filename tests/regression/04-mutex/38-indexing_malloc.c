#include <pthread.h>
#include <stdlib.h>

int* s;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  s[0] = 8; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  s = (int*)malloc(sizeof(int));
  pthread_create(&id, NULL, t_fun, NULL);
  s[0] = 9; // RACE!
  pthread_join (id, NULL);
  return 0;
}