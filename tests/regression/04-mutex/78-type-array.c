#include <pthread.h>
#include <stdio.h>

struct S {
  int field;
  int arr[2];
};

extern struct S* getS();

void *t_fun(void *arg) {
  // should not distribute access to (struct S).field
  getS()->arr[1] = 1; // NORACE
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  getS()->field = 2; // NORACE
  return 0;
}
