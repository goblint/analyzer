#include <pthread.h>
#include <stdio.h>

struct S {
  int field;
};

struct S s;

extern struct S* getS();

void *t_fun(void *arg) {
  printf("%d",getS()->field); // RACE!

  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  struct S s1;
  s = s1; // RACE!
  return 0;
}

