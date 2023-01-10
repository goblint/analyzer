// SKIP PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.relation.privatization mutex-meet --set ana.apron.domain polyhedra
// TODO: why doesn't mutex-meet-tid succeed? a widening loses some upper bound and we forget a possible overflow, succeeds with assume_none
#include <pthread.h>
#include <goblint.h>

extern int __VERIFIER_nondet_int();

int capacity;
int used;
int free;
pthread_mutex_t C_sema = PTHREAD_MUTEX_INITIALIZER;	// protect consumer sema
pthread_mutex_t P_sema = PTHREAD_MUTEX_INITIALIZER;	// protect producer sema
pthread_cond_t C_cond = PTHREAD_COND_INITIALIZER; 	// Consumer
pthread_cond_t P_cond = PTHREAD_COND_INITIALIZER;	// Producer

void consume() {
  pthread_mutex_lock(&C_sema);
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);

  // if (used >= 1) {
  //   used--;
  //   free++;
  // }

  while (used == 0) {
    pthread_cond_wait(&C_cond, &C_sema);
  }

  used--;
  pthread_mutex_unlock(&C_sema);

  // consume!
  
  pthread_mutex_lock(&P_sema);
  free++;
  pthread_mutex_signal(&P_cond);
  pthread_mutex_unlock(&P_sema);


/*
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);
 */
}

void produce() {
  pthread_mutex_lock(&P_sema);
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);

  // if (free >= 1) {
  //   free--;
  //   used++;
  // }

  while (free == 0) {
    pthread_cond_wait(&POP, &Q);
  }

  free--;
  pthread_mutex_unlock(&P_sema);


/*
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);
  */

  pthread_mutex_lock(&C_sema);
  used++;
  pthread_cond_signal(&C_cond);
  pthread_mutex_unlock(&C_sema);
}

void *worker(void *arg) {
  while (1)
    consume();
  return NULL;
}

int main() {
  capacity = __VERIFIER_nondet_int();
  if (capacity >= 0) {
    free = capacity;
    used = 0;

    __goblint_check(free >= 0);
    __goblint_check(free <= capacity);
    __goblint_check(used >= 0);
    __goblint_check(used <= capacity);
    __goblint_check(used + free == capacity);

    pthread_t worker1;
    pthread_t worker2;
    pthread_create(&worker1, NULL, worker, NULL);
    pthread_create(&worker2, NULL, worker, NULL);

    while (1)
      produce();
  }

  return 0;
}
