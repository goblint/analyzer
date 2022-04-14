#include <pthread.h>
#include <stdio.h>

void *t_fun(void *arg) {
  int *p = malloc(sizeof(int));
  *p = 42; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id[10];
  int i;
  for (i=0; i<10; i++)
    pthread_create(&id[i], NULL, t_fun, NULL);
  for (i=0; i<10; i++)
    pthread_join (id[i], NULL);
  return 0;
}
