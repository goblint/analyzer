extern int __VERIFIER_nondet_int();

#include <stdio.h>
#include <pthread.h>

struct {
  int x;
  int y;
} data;

void *t_fun(void *arg) {
  data.x++; // RACE!
  return NULL;
}

int main() {
  int *i,j = __VERIFIER_nondet_int();
  pthread_t id;
  if (j) i = &data.x;
  else i = &data.y;
  pthread_create(&id, NULL, t_fun, NULL);
  (*i)++; // RACE!
  return 0;
}

