#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>

typedef struct {
  int x;
  int y;
} data;

data *d;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  d->x = 3; // NORACE
  d->y = 8; // RACE!
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id;
  data *z;

  d = malloc(sizeof(data));
  z = d;

  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m);
  printf("%d\n",d->x); // NORACE
  pthread_mutex_unlock(&m);
  printf("%d\n",z->y); // RACE!

  return 0;
}
