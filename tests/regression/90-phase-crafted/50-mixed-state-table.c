// CRAM
// RCU-style grace period sketch: updater publishes, readers quiesce, collector retires.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t rcu_lock;
struct RcuState { int version; int readers; int retired; } rcu;

void *updater_thread(void *arg) {
  pthread_mutex_lock(&rcu_lock);
  /* GHOST updater_thread 1 */ rcu.version += 1;
  pthread_mutex_unlock(&rcu_lock);
  pthread_mutex_lock(&rcu_lock);
  /* GHOST updater_thread 2 */ rcu.retired += 2;
  pthread_mutex_unlock(&rcu_lock);
  return 0;
}

void *reader_thread(void *arg) {
  int quiescent = 0;
  while (quiescent < 2)
    quiescent++;
  pthread_mutex_lock(&rcu_lock);
  /* GHOST reader_thread 1 */ rcu.readers -= quiescent;
  pthread_mutex_unlock(&rcu_lock);
  return 0;
}

void *collector_thread(void *arg) {
  pthread_mutex_lock(&rcu_lock);
  /* GHOST collector_thread 1 */ rcu.retired -= 1;
  pthread_mutex_unlock(&rcu_lock);
  return 0;
}

int main(void) {
  pthread_t updater, reader, collector;
  pthread_mutex_init(&rcu_lock, 0);
  rcu.version = 40;
  rcu.readers = 5;
  rcu.retired = 7;
  pthread_create(&reader, 0, reader_thread, 0);
  pthread_create(&updater, 0, updater_thread, 0);
  pthread_join(reader, 0);
  pthread_create(&collector, 0, collector_thread, 0);
  pthread_join(updater, 0);
  pthread_join(collector, 0);
  if (rcu.version != 41 || rcu.readers != 3 || rcu.retired != 8) {
    reach_error();
    abort();
  }
  return 0;
}
