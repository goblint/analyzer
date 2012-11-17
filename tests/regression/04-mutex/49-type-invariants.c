//PARAM: --set exp.type-inv true
#include <pthread.h>
#include <stdio.h>

struct S {
  int field;
};

struct S s;

void *t_fun(void *arg) {
  printf("%d",s.field); // RACE!

  return NULL;
}

extern struct S* getS();

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  getS()->field = 10; // RACE!!11
  return 0;
}
 
