#include <pthread.h>
#include <stdio.h>

int g;

void *t_fun(void *arg) {
  g=g+1; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  int a = sizeof(int[g]); // RACE!
  int b = sizeof(int[2][g]); // RACE!
  return 0;
}
