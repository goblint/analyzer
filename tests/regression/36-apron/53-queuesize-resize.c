// SKIP PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions --set ana.relation.privatization mutex-meet-tid --set ana.path_sens[+] threadflag --set ana.apron.domain polyhedra
// TODO: why doesn't mutex-meet succeed?
#include <pthread.h>
#include <assert.h>

extern int __VERIFIER_nondet_int();

int capacity;
int used;
int free;
pthread_mutex_t Q = PTHREAD_MUTEX_INITIALIZER;

void pop() {
  pthread_mutex_lock(&Q);
  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);

  if (used >= 1) {
    used--;
    free++;
  }

  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);
  pthread_mutex_unlock(&Q);
}

void push() {
  pthread_mutex_lock(&Q);
  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);

  if (free >= 1) {
    free--;
    used++;
  }

  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);
  pthread_mutex_unlock(&Q);
}

void resize() {
  pthread_mutex_lock(&Q);
  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);

  int new_capacity;
  new_capacity = __VERIFIER_nondet_int();
  if (new_capacity >= 0) {
    if (used <= new_capacity) // scale up
      free += new_capacity - capacity;
    else { // scale down
      used = new_capacity; // throw away some used
      free = 0;
    }
    capacity = new_capacity;
  }

  assert(free >= 0);
  assert(free <= capacity);
  assert(used >= 0);
  assert(used <= capacity);
  assert(used + free == capacity);
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

    assert(free >= 0);
    assert(free <= capacity);
    assert(used >= 0);
    assert(used <= capacity);
    assert(used + free == capacity);

    pthread_t worker1;
    pthread_t worker2;
    pthread_create(&worker1, NULL, worker, NULL);
    pthread_create(&worker2, NULL, worker, NULL);

    while (1) {
      push();
      resize();
    }
  }

  return 0;
}
