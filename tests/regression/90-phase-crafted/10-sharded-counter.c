// CRAM
// Sharded request accounting spreads updates across three array cells.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t shard_lock;
int shards[3];

void *api_gateway(void *arg) {
  int target = 0;
  for (int i = 0; i < 4; i++)
    target = (target + 1) % 3;
  pthread_mutex_lock(&shard_lock);
  /* GHOST api_gateway 1 */ shards[target] += 9;
  pthread_mutex_unlock(&shard_lock);
  return 0;
}

void *batch_importer(void *arg) {
  for (int i = 0; i < 1; i++) {
    pthread_mutex_lock(&shard_lock);
    /* GHOST batch_importer 1 */ shards[0] += 5;
    pthread_mutex_unlock(&shard_lock);
  }
  pthread_mutex_lock(&shard_lock);
  /* GHOST batch_importer 2 */ shards[2] -= 2;
  pthread_mutex_unlock(&shard_lock);
  return 0;
}

void *health_probe(void *arg) {
  int correction = 0;
  for (int i = 0; i < 4; i++)
    correction += (i == 3);
  pthread_mutex_lock(&shard_lock);
  /* GHOST health_probe 1 */ shards[1] += correction;
  pthread_mutex_unlock(&shard_lock);
  return 0;
}

int main(void) {
  pthread_t a, b, h;
  pthread_mutex_init(&shard_lock, 0);
  shards[0] = 10;
  shards[1] = 20;
  shards[2] = 30;
  pthread_create(&a, 0, api_gateway, 0);
  pthread_create(&b, 0, batch_importer, 0);
  pthread_create(&h, 0, health_probe, 0);
  pthread_join(a, 0);
  pthread_join(b, 0);
  pthread_join(h, 0);
  if (shards[0] + shards[1] + shards[2] != 73 || shards[1] != 30 || shards[2] != 28) {
    reach_error();
    abort();
  }
  return 0;
}
