#include <pthread.h>

int g;

void *t_fun(void *arg) {
  g++; // RACE!
  return NULL;
}

void foo() {
  g++; // RACE!
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  foo();
  return 0;
}