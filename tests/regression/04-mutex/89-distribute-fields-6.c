#include <pthread.h>
#include <stdlib.h>

struct S {
  int data;
  int data2;
};

struct T {
  struct S s;
  struct S s2;
  int data3;
};

struct T t;

void *t_fun(void *arg) {
  struct T t3;
  t = t3; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  struct T t2;
  t = t2; // RACE!
  pthread_join (id, NULL);
  return 0;
}
