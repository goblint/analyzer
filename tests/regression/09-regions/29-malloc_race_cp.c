// PARAM: --set ana.activated[+] "'region'"
// Copy of 02/25 with region enabled
#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>

int *x;
int *y;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  *x = 3; // NORACE
  *y = 8; // RACE!
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id;
  int *z;

  x = malloc(sizeof(int));
  y = malloc(sizeof(int));
  z = y;

  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m);
  printf("%d\n",*x); // NORACE
  pthread_mutex_unlock(&m);
  printf("%d\n",*z); // RACE!

  return 0;
}
