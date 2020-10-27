extern int __VERIFIER_nondet_int();
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

#include <pthread.h>

int data[10];

void *t_fun(void *arg) {
  data[4]++; // RACE!
  return NULL;
}

int main() {
  int i = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= i && i < 10);
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  data[i]++; // RACE!
  return 0;
}

