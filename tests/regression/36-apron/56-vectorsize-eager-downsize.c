// SKIP PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.apron.privatization mutex-meet-tid --set ana.path_sens[+] threadflag --set ana.apron.domain polyhedra
// TODO: why doesn't mutex-meet succeed?
#include <pthread.h>
#include <assert.h>
#include <limits.h>

extern int __VERIFIER_nondet_int();

// MAX_CAPACITY avoids having to deal with resize over INT_MAX
#define MAX_CAPACITY 1000

int capacity;
int used;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t U = PTHREAD_MUTEX_INITIALIZER;

void remove() {
  int amount;
  amount = __VERIFIER_nondet_int();
  if (amount >= 0) {
    pthread_mutex_lock(&U);
    pthread_mutex_lock(&C);
    __goblint_check(used >= 0);
    __goblint_check(capacity >= 1);
    __goblint_check(capacity <= MAX_CAPACITY);
    __goblint_check(used <= capacity);
    __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *

    if (amount <= used) {
      used -= amount;

      // overly eager downsize on remove
      if (used == 0)
        capacity = 1; // special case, since we require capacity >= 1
      else
        capacity = used;
    }

    __goblint_check(used >= 0);
    __goblint_check(capacity >= 1);
    __goblint_check(capacity <= MAX_CAPACITY);
    __goblint_check(used <= capacity);
    __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *
    pthread_mutex_unlock(&C);
    pthread_mutex_unlock(&U);
  }
}

void append() {
  int amount;
  amount = __VERIFIER_nondet_int();
  if (amount >= 0) {
    pthread_mutex_lock(&U);
    pthread_mutex_lock(&C);
    __goblint_check(used >= 0);
    __goblint_check(capacity >= 1);
    __goblint_check(capacity <= MAX_CAPACITY);
    __goblint_check(used <= capacity);
    __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *

    if (used <= MAX_CAPACITY - amount) { // used + amount <= MAX_CAPACITY, but without overflow in +
      int new_used = used + amount;

      // conservative upsize on append
      if (new_used > capacity)
        capacity = new_used;
      used = new_used;
    }

    __goblint_check(used >= 0);
    __goblint_check(capacity >= 1);
    __goblint_check(capacity <= MAX_CAPACITY);
    __goblint_check(used <= capacity);
    __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *
    pthread_mutex_unlock(&C);
    pthread_mutex_unlock(&U);
  }
}

void *worker(void *arg) {
  while (1) {
    remove();
    append();
  }
  return NULL;
}

int main() {
  used = 0;
  capacity = 1;
  __goblint_check(used >= 0);
  __goblint_check(capacity >= 1);
  __goblint_check(capacity <= MAX_CAPACITY);
  __goblint_check(used <= capacity);
  __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *

  pthread_t worker1;
  pthread_t worker2;
  pthread_create(&worker1, NULL, worker, NULL);
  pthread_create(&worker2, NULL, worker, NULL);

  while (1) {
    pthread_mutex_lock(&U);
    pthread_mutex_lock(&C);
    __goblint_check(used >= 0);
    __goblint_check(capacity >= 1);
    __goblint_check(capacity <= MAX_CAPACITY);
    __goblint_check(used <= capacity);
    __goblint_check(used >= capacity - used - 1); // 2 * used >= capacity - 1, but without overflow in *
    pthread_mutex_unlock(&C);
    pthread_mutex_unlock(&U);
  }

  return 0;
}
