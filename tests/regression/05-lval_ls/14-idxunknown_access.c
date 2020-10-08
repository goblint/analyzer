extern int __VERIFIER_nondet_int();

#include <pthread.h>

int data[10];

void *t_fun(void *arg) {
  data[4]++; // RACE!
  return NULL;
}

int main() {
  int i = __VERIFIER_nondet_int();
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  data[i]++; // RACE!
  return 0;
}

