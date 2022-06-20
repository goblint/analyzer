// SKIP PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.path_sens[+] threadflag
// TODO: why does this need path-sensitive threadflag even with mutex-meet to succeed?
// sensible version of sv-benchmarks airline with non-static capacity
#include <assert.h>
#include <pthread.h>

extern int __VERIFIER_nondet_int();

#define NUM_THREADS 5

int capacity;
int sold;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *seller(void *arg) {
  while (1) {
    pthread_mutex_lock(&A);
    if (sold >= capacity)
      break;
    else
      sold = sold + 1;
    pthread_mutex_unlock(&A);
  }
  pthread_mutex_unlock(&A); // also unlock after break

  return NULL;
}

int main(int argc, char **argv) {
  capacity = __VERIFIER_nondet_int();
  if (capacity >= 0) {
    sold = 0;

    pthread_t threads[NUM_THREADS];
    for (int i = 0; i < NUM_THREADS; i++)
      pthread_create(&threads[i], NULL, seller, NULL);

    // for (int i = 0; i < NUM_THREADS; i++)
    //   pthread_join(threads[i], NULL);

    pthread_mutex_lock(&A);
    assert(sold <= capacity); // not oversold
    pthread_mutex_unlock(&A);
  }

  return 0;
}
