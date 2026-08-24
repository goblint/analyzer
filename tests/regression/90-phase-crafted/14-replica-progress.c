// CRAM
// Replication progress has ack/apply/snapshot phases and a relational assertion.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t repl_lock;
struct Replica { int received; int applied; int snapshot; } replica;

void *network_thread(void *arg) {
  int span = 0;
  for (int x = 1; x <= 4; x++)
    span += 2;
  pthread_mutex_lock(&repl_lock);
  /* GHOST network_thread 1 */ replica.received += span;
  pthread_mutex_unlock(&repl_lock);
  return 0;
}

void *apply_thread(void *arg) {
  int applied = 0;
  for (int i = 0; i < 3; i++)
    applied += i + 2;
  pthread_mutex_lock(&repl_lock);
  /* GHOST apply_thread 1 */ replica.applied += applied;
  pthread_mutex_unlock(&repl_lock);
  pthread_mutex_lock(&repl_lock);
  /* GHOST apply_thread 2 */ replica.snapshot += 1;
  pthread_mutex_unlock(&repl_lock);
  return 0;
}

void *snapshot_thread(void *arg) {
  pthread_mutex_lock(&repl_lock);
  /* GHOST snapshot_thread 1 */ replica.snapshot *= 2;
  pthread_mutex_unlock(&repl_lock);
  return 0;
}

int main(void) {
  pthread_t n, a, s;
  pthread_mutex_init(&repl_lock, 0);
  replica.received = 200;
  replica.applied = 150;
  replica.snapshot = 3;
  pthread_create(&n, 0, network_thread, 0);
  pthread_create(&a, 0, apply_thread, 0);
  pthread_create(&s, 0, snapshot_thread, 0);
  pthread_join(n, 0);
  pthread_join(a, 0);
  pthread_join(s, 0);
  if (replica.received - replica.applied != 49 || replica.snapshot < 7 || replica.snapshot > 8) {
    reach_error();
    abort();
  }
  return 0;
}
