// CRAM
// Reference lifecycle with acquire/release phases and an unasserted maintenance worker.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t ref_lock;
struct Pool { int free_slots; int borrowed; int epoch; } pool;

void *frontend(void *arg) {
  int grab = 0;
  for (int i = 0; i < 3; i++)
    grab += 1;
  pthread_mutex_lock(&ref_lock);
  /* GHOST frontend 1 */ pool.free_slots -= grab;
  pthread_mutex_unlock(&ref_lock);
  pthread_mutex_lock(&ref_lock);
  /* GHOST frontend 2 */ pool.borrowed += grab;
  pthread_mutex_unlock(&ref_lock);
  return 0;
}

void *backend(void *arg) {
  int done = 0;
  for (int i = 0; i < 2; i++)
    done += 1;
  pthread_mutex_lock(&ref_lock);
  /* GHOST backend 1 */ pool.borrowed -= done;
  pthread_mutex_unlock(&ref_lock);
  pthread_mutex_lock(&ref_lock);
  /* GHOST backend 2 */ pool.free_slots += done;
  pthread_mutex_unlock(&ref_lock);
  return 0;
}

void *sweeper(void *arg) {
  int mask = 0;
  for (int bit = 0; bit < 2; bit++)
    mask |= 1 << bit;
  pthread_mutex_lock(&ref_lock);
  /* GHOST sweeper 1 */ pool.epoch ^= mask;
  pthread_mutex_unlock(&ref_lock);
  return 0;
}

int main(void) {
  pthread_t a, b, c;
  pthread_mutex_init(&ref_lock, 0);
  pool.free_slots = 12;
  pool.borrowed = 5;
  pool.epoch = 4;
  pthread_create(&a, 0, frontend, 0);
  pthread_create(&b, 0, backend, 0);
  pthread_create(&c, 0, sweeper, 0);
  pthread_join(a, 0);
  pthread_join(b, 0);
  pthread_join(c, 0);
  if (pool.free_slots + pool.borrowed != 17 || pool.free_slots != 11 || pool.borrowed != 6 || pool.epoch != 7) {
    reach_error();
    abort();
  }
  return 0;
}
