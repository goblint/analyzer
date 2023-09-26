// PARAM: --enable ana.race.free
#include <pthread.h>
#include <stdlib.h>

void *t_fun(void *arg) {
  int *p = (int *) arg;
  (*p)++; // RACE!
  return NULL;
}

void *t_fun2(void *arg) {
  int *p = (int *) arg;
  free(p); // NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  int *p;
  p = malloc(sizeof(int));
  pthread_create(&id, NULL, t_fun, (void *) p);
  free(p); // RACE!
  pthread_join (id, NULL);

  p = malloc(sizeof(int));
  pthread_create(&id, NULL, t_fun2, (void *) p);
  free(p); // NORACE
  pthread_join (id, NULL);
  return 0;
}
