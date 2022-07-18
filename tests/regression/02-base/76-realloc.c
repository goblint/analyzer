// PARAM: --enable ana.race.free
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>

void test1_f() {
  assert(1); // reachable
}

void test1() {
  void (**fpp)(void) = malloc(sizeof(void(**)(void)));
  *fpp = &test1_f;

  fpp = realloc(fpp, sizeof(void(**)(void))); // same size

  // (*fpp)();
  void (*fp)(void) = *fpp;
  fp(); // should call test1_f
}

void* test2_f(void *arg) {
  int *p = arg;
  *p = 1; // RACE!
  return NULL;
}

void test2() {
  int *p = malloc(sizeof(int));
  pthread_t id;
  pthread_create(&id, NULL, test2_f, p);
  realloc(p, sizeof(int)); // RACE!
}

void* test3_f(void *arg) {
  int *p = arg;
  int x = *p; // RACE!
  return NULL;
}

void test3() {
  int *p = malloc(sizeof(int));
  pthread_t id;
  pthread_create(&id, NULL, test3_f, p);
  realloc(p, sizeof(int)); // RACE!
}

int main() {
  test1();
  test2();
  test3();
  return 0;
}
