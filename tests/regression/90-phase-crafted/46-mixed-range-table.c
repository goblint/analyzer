// CRAM
// Work-stealing deque bookkeeping: owner pushes, thief steals, rebalancer advances head.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t deque_lock;
struct Deque { int head; int tail; int stolen; } deque;

void *owner_thread(void *arg) {
  int pushed = 0;
  for (int i = 0; i < 3; i++)
    pushed++;
  pthread_mutex_lock(&deque_lock);
  /* GHOST owner_thread 1 */ deque.tail += pushed;
  pthread_mutex_unlock(&deque_lock);
  return 0;
}

void *thief_thread(void *arg) {
  int taken = 0;
  while (taken < 2)
    taken++;
  pthread_mutex_lock(&deque_lock);
  /* GHOST thief_thread 1 */ deque.stolen += taken;
  pthread_mutex_unlock(&deque_lock);
  pthread_mutex_lock(&deque_lock);
  /* GHOST thief_thread 2 */ deque.head += taken;
  pthread_mutex_unlock(&deque_lock);
  return 0;
}

void *rebalance_thread(void *arg) {
  pthread_mutex_lock(&deque_lock);
  /* GHOST rebalance_thread 1 */ deque.head += 1;
  pthread_mutex_unlock(&deque_lock);
  return 0;
}

int main(void) {
  pthread_t owner, thief, rebalance;
  pthread_mutex_init(&deque_lock, 0);
  deque.head = 4;
  deque.tail = 9;
  deque.stolen = 1;
  pthread_create(&owner, 0, owner_thread, 0);
  pthread_create(&thief, 0, thief_thread, 0);
  pthread_create(&rebalance, 0, rebalance_thread, 0);
  pthread_join(thief, 0);
  pthread_join(owner, 0);
  pthread_join(rebalance, 0);
  if (deque.head != 7 || deque.tail != 12 || deque.stolen != 3) {
    reach_error();
    abort();
  }
  return 0;
}
