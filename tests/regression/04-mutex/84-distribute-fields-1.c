#include <pthread.h>
#include <stdlib.h>

struct S {
  int data;
  int data2;
};

struct S s;

void *t_fun(void *arg) {
  s.data = 1; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  struct S s2;
  s = s2; // RACE!
  pthread_join (id, NULL);
  return 0;
}
