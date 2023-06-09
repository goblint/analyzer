//PARAM: --disable ana.mutex.disjoint_types
#include <pthread.h>
#include <stdio.h>

struct S {
  int field;
};

struct T {
  struct S s;
};

// struct S s;
// struct T t;

extern struct S* getS();
extern struct T* getT();

// getS could return the same struct as is contained in getT

void *t_fun(void *arg) {
  getS()->field = 1; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  getT()->s.field = 2; // RACE!
  return 0;
}
