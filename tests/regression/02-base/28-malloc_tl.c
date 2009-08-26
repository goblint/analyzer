// SKIP
#include <stdlib.h>
#include <pthread.h>

void *t_fun(void *arg) {
  int *x;
  x = malloc(sizeof(int));
  *x = 3; // NORACE
  (*x)++; // NORACE
  free(x);
  return NULL;
}

int main() {
  pthread_t id1, id2;

  pthread_create(&id1, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  
  return 0;
}


