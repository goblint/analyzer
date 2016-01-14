// SKIP
#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>

int *d;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  d[2] = 3; // NORACE
  d[3] = 8; // RACE
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id;
  d = calloc(10, sizeof(int));

  pthread_create(&id, NULL, t_fun, NULL);
  
  pthread_mutex_lock(&m);
  printf("%d\n",d[2]); // NORACE
  pthread_mutex_unlock(&m);
  printf("%d\n",d[3]); // RACE
  
  return 0;
}
