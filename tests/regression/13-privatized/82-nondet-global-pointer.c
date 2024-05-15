// PARAM: --set ana.base.privatization protection --enable ana.int.enums
// Like 81-nondet-struct-ptr.c, but with syntactic globals instead of escaping ones.
#include<pthread.h>
#include<stdlib.h>
struct a {
  int b;
};

int *ptr;
int *immer_da_oane;

int da_oane = 0;
int de_andre = 42;

pthread_mutex_t m;

void doit() {
  pthread_mutex_lock(&m);
  *ptr = 5;

  // Should be either 0 or 5, depending on which one ptr points to
  int fear = *immer_da_oane;
  __goblint_check(fear == 5); //UNKNOWN!

  pthread_mutex_unlock(&m);

  pthread_mutex_lock(&m);
  // This works
  int hope = *immer_da_oane;
  __goblint_check(hope == 5); //UNKNOWN!
  pthread_mutex_unlock(&m);

}

void* k(void *arg) {
  // Force MT
  return NULL;
}

int main() {
    int top;

    if(top) {
      ptr = &da_oane;
    } else {
      ptr = &de_andre;
    }

    immer_da_oane = &da_oane;

    pthread_t t1;
    pthread_create(&t1, 0, k, 0);

    doit();
    return 0;
}
