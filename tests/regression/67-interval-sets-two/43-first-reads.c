// PARAM: --set ana.int.interval_set true
extern int __VERIFIER_nondet_int();

#include<pthread.h>
#include <goblint.h>

int glob1 = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t = __VERIFIER_nondet_int();
  pthread_mutex_lock(&mutex1);
  if(t == 42) {
      glob1 = 1;
  }
  t = glob1;

  __goblint_check(t == 0); //UNKNOWN!

  __goblint_check(t == 1); //UNKNOWN!

  glob1 = 0;

  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(glob1 == 0);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex1);
  __goblint_check(glob1 == 0);
  pthread_mutex_unlock(&mutex1);
  pthread_join (id, NULL);
  return 0;
}
