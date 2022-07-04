// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain octagon --enable ana.apron.threshold_widening
// TODO: why needs threshold widening to succeed when queuesize doesn't?
#include <pthread.h>
#include <assert.h>

#define CAPACITY 1000

int used;
int free;
pthread_mutex_t Q = PTHREAD_MUTEX_INITIALIZER;

void pop() {
  pthread_mutex_lock(&Q);
  assert(free >= 0);
  assert(free <= CAPACITY);
  assert(used >= 0);
  assert(used <= CAPACITY);
  assert(used + free == CAPACITY);

  if (used >= 1) {
    used--;
    free++;
  }

  assert(free >= 0);
  assert(free <= CAPACITY);
  assert(used >= 0);
  assert(used <= CAPACITY);
  assert(used + free == CAPACITY);
  pthread_mutex_unlock(&Q);
}

void push() {
  pthread_mutex_lock(&Q);
  assert(free >= 0);
  assert(free <= CAPACITY);
  assert(used >= 0);
  assert(used <= CAPACITY);
  assert(used + free == CAPACITY);

  if (free >= 1) {
    free--;
    used++;
  }

  assert(free >= 0);
  assert(free <= CAPACITY);
  assert(used >= 0);
  assert(used <= CAPACITY);
  assert(used + free == CAPACITY);
  pthread_mutex_unlock(&Q);
}

void *worker(void *arg) {
  while (1)
    pop();
  return NULL;
}

int main() {
  free = CAPACITY;
  used = 0;

  assert(free >= 0);
  assert(free <= CAPACITY);
  assert(used >= 0);
  assert(used <= CAPACITY);
  assert(used + free == CAPACITY);

  pthread_t worker1;
  pthread_t worker2;
  pthread_create(&worker1, NULL, worker, NULL);
  pthread_create(&worker2, NULL, worker, NULL);

  while (1)
    push();

  return 0;
}
