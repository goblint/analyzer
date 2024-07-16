// SKIP PARAM: --set ana.activated[+] elina --enable ana.sv-comp.functions --set ana.relation.privatization mutex-meet --set ana.apron.domain polyhedra
// TODO: why doesn't mutex-meet-tid succeed? a widening loses some upper bound and we forget a possible overflow, succeeds with assume_none
#include <pthread.h>
#include <goblint.h>

extern int __VERIFIER_nondet_int();

int capacity;
int used;
int free;
pthread_mutex_t Q = PTHREAD_MUTEX_INITIALIZER;

void pop() {
  pthread_mutex_lock(&Q);
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);

  if (used >= 1) {
    used--;
    free++;
  }

  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);
  pthread_mutex_unlock(&Q);
}

void push() {
  pthread_mutex_lock(&Q);
  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);

  if (free >= 1) {
    free--;
    used++;
  }

  __goblint_check(free >= 0);
  __goblint_check(free <= capacity);
  __goblint_check(used >= 0);
  __goblint_check(used <= capacity);
  __goblint_check(used + free == capacity);
  pthread_mutex_unlock(&Q);
}

void *worker(void *arg) {
  while (1)
    pop();
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
      push();
  }

  return 0;
}
