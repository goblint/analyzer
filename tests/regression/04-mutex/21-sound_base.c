#include <pthread.h>
#include <stdio.h>

int global;

void bad() { global++; }
void good() { printf("Hello!"); }

void (*f)() = good;

void *t_fun(void *arg) {
  f();
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  f = bad;
  printf("global: %d\n", global);
  return 0;
}
