// PARAM: --set ana.base.privatization protection --enable ana.int.enums
// Like 80-nondet-struct-ptr.c, but somewhat simplified to not use structs and malloc etc
#include<pthread.h>
#include<stdlib.h>
struct a {
  int b;
};

int *ptr;
int *immer_da_oane;

pthread_mutex_t m;

void doit() {
  pthread_mutex_lock(&m);
  *ptr = 5;

  // Should be either 5 or 0, depending on which one the pointer points to
  int fear = *immer_da_oane;
  __goblint_check(fear == 5); //UNKNOWN!

  pthread_mutex_unlock(&m);

  pthread_mutex_lock(&m);
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

    int da_oane = 0;
    int de_andre = 42;

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
