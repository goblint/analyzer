//PARAM: --enable ana.race.direct-arithmetic
#include <pthread.h>
#include <stdio.h>

struct S {
  int field;
};

struct T {
  struct S s;
};

struct U {
  struct T t;
};

// struct S s;
// struct T t;

extern struct S* getS();
extern struct T* getT();
extern struct U* getU();

// getS could return the same struct as is contained in getT

void *t_fun(void *arg) {
  // should write to (struct U).t.s.field in addition to (struct S).field
  // but easier to implement the other way around?
  getS()->field = 1; // RACE!
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  getU()->t.s.field = 2; // RACE!
  return 0;
}
