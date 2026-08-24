// CRAM
// Snapshot service negotiates feature masks and monotonically changes epochs.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t snap_lock;
struct Snapshot { int memory_features; int storage_features; int epoch; } snapshot;

static int feature_mask(int base) {
  int mask = 0;
  for (int i = 0; i < 3; i++)
    mask |= 1 << (base + i);
  return mask;
}

void *memory_worker(void *arg) {
  pthread_mutex_lock(&snap_lock);
  /* GHOST memory_worker 1 */ snapshot.memory_features |= 7;
  pthread_mutex_unlock(&snap_lock);
  return 0;
}

void *storage_worker(void *arg) {
  pthread_mutex_lock(&snap_lock);
  /* GHOST storage_worker 1 */ snapshot.storage_features |= 28;
  pthread_mutex_unlock(&snap_lock);
  pthread_mutex_lock(&snap_lock);
  /* GHOST storage_worker 2 */ snapshot.epoch += 2;
  pthread_mutex_unlock(&snap_lock);
  return 0;
}

void *epoch_worker(void *arg) {
  int bump = 0;
  for (int i = 0; i < 2; i++)
    bump++;
  pthread_mutex_lock(&snap_lock);
  /* GHOST epoch_worker 1 */ snapshot.epoch += bump;
  pthread_mutex_unlock(&snap_lock);
  return 0;
}

int main(void) {
  pthread_t m, s, e;
  pthread_mutex_init(&snap_lock, 0);
  snapshot.memory_features = 1;
  snapshot.storage_features = 16;
  snapshot.epoch = 10;
  pthread_create(&m, 0, memory_worker, 0);
  pthread_create(&s, 0, storage_worker, 0);
  pthread_create(&e, 0, epoch_worker, 0);
  pthread_join(m, 0);
  pthread_join(s, 0);
  pthread_join(e, 0);
  if (snapshot.memory_features != 7 || snapshot.storage_features != 28 || snapshot.epoch != 14) {
    reach_error();
    abort();
  }
  return 0;
}
