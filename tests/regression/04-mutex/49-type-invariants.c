#include <pthread.h>
// Read the field without introducing unrelated standard-stream accesses.

struct S {
  int field;
};

struct S s;

void *t_fun(void *arg) {
  volatile int value = s.field; // RACE!

  return NULL;
}

extern struct S* getS();

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  getS()->field = 10; // RACE!
  return 0;
}

