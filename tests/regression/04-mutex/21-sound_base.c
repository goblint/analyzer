#include <pthread.h>
#include <stdio.h>

int global;

void bad() { global++; } // RACE!
void good() { printf("Hello!"); }

void (*f)() = good;

void *t_fun(void *arg) {
  f(); // RACE!
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  f = bad; // RACE!
  printf("global: %d\n", global); // RACE!
  return 0;
}
