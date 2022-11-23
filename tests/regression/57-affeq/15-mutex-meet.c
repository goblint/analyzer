//SKIP PARAM:  --set ana.activated[+] "affeq" --set ana.affeq.matrix "array" --set sem.int.signed_overflow assume_none --set ana.relation.privatization mutex-meet

#include <assert.h>

extern int __VERIFIER_nondet_int();
#include <pthread.h>

int g = 42; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 42;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
    pthread_mutex_lock(&A);
    pthread_mutex_unlock(&A);
    __goblint_check(g == 42);
  return 0;
}
