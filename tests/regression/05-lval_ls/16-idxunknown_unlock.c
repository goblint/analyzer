extern int __VERIFIER_nondet_int();
extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

#include <pthread.h>

int data;
pthread_mutexattr_t mutexattr;
pthread_mutex_t m[10];

void *t_fun(void *arg) {
  pthread_mutex_lock(&m[4]);
  data++; // RACE!
  pthread_mutex_unlock(&m[4]);
  return NULL;
}

int main() {
  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_ERRORCHECK);
  for (int i = 0; i < 10; i++)
    pthread_mutex_init(&m[i], &mutexattr);

  int i = __VERIFIER_nondet_int();
  assume_abort_if_not(0 <= i && i < 10);
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&m[4]);
  pthread_mutex_unlock(&m[i]); // no UB because ERRORCHECK
  data++; // RACE!
  pthread_mutex_unlock(&m[4]); // no UB because ERRORCHECK
  return 0;
}

