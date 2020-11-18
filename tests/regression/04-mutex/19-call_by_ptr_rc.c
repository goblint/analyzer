extern int __VERIFIER_nondet_int();

#include<pthread.h>

void foo(int (*callback)()) {
  for (int i = 0; i < 10; i++) {
    if (__VERIFIER_nondet_int())
      callback();
  }
}


int glob;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex2);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int bar() {
  pthread_mutex_lock(&mutex1);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex1);
  return 0;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  foo(bar);
  pthread_join (id, NULL);
  return 0;
}
