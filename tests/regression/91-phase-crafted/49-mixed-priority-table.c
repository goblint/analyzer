// CRAM
// Priority inheritance sketch: waiter raises priority, owner releases, monitor records handoff.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t pi_lock;
struct PiState { int owner_prio; int waiter_prio; int handoffs; } pi_state;

void *waiter_thread(void *arg) {
  int boost = 0;
  for (int i = 0; i < 2; i++)
    boost += 3;
  pthread_mutex_lock(&pi_lock);
  /* GHOST waiter_thread 1 */ pi_state.waiter_prio += boost;
  pthread_mutex_unlock(&pi_lock);
  return 0;
}

void *owner_thread(void *arg) {
  pthread_mutex_lock(&pi_lock);
  /* GHOST owner_thread 1 */ pi_state.owner_prio += 4;
  pthread_mutex_unlock(&pi_lock);
  pthread_mutex_lock(&pi_lock);
  /* GHOST owner_thread 2 */ pi_state.handoffs += 1;
  pthread_mutex_unlock(&pi_lock);
  return 0;
}

void *monitor_thread(void *arg) {
  pthread_mutex_lock(&pi_lock);
  /* GHOST monitor_thread 1 */ pi_state.handoffs += 2;
  pthread_mutex_unlock(&pi_lock);
  return 0;
}

int main(void) {
  pthread_t waiter, owner, monitor;
  pthread_mutex_init(&pi_lock, 0);
  pi_state.owner_prio = 10;
  pi_state.waiter_prio = 5;
  pi_state.handoffs = 0;
  pthread_create(&owner, 0, owner_thread, 0);
  pthread_create(&waiter, 0, waiter_thread, 0);
  pthread_create(&monitor, 0, monitor_thread, 0);
  pthread_join(waiter, 0);
  pthread_join(monitor, 0);
  pthread_join(owner, 0);
  if (pi_state.owner_prio != 14 || pi_state.waiter_prio != 11 || pi_state.handoffs != 3) {
    reach_error();
    abort();
  }
  return 0;
}
