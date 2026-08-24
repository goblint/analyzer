// CRAM
// Token-bucket throttling: refill, consume, and accounting workers share one bucket.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t bucket_lock;
struct Bucket { int tokens; int consumed; int epoch; } bucket;

void *refill_thread(void *arg) {
  int refill = 0;
  for (int i = 0; i < 4; i++)
    refill += 2;
  pthread_mutex_lock(&bucket_lock);
  /* GHOST refill_thread 1 */ bucket.tokens += refill;
  pthread_mutex_unlock(&bucket_lock);
  return 0;
}

void *consumer_thread(void *arg) {
  int used = 3;
  pthread_mutex_lock(&bucket_lock);
  /* GHOST consumer_thread 1 */ bucket.tokens -= used;
  pthread_mutex_unlock(&bucket_lock);
  pthread_mutex_lock(&bucket_lock);
  /* GHOST consumer_thread 2 */ bucket.consumed += used;
  pthread_mutex_unlock(&bucket_lock);
  return 0;
}

void *epoch_thread(void *arg) {
  pthread_mutex_lock(&bucket_lock);
  /* GHOST epoch_thread 1 */ bucket.epoch += 1;
  pthread_mutex_unlock(&bucket_lock);
  return 0;
}

int main(void) {
  pthread_t refill, consumer, epoch;
  pthread_mutex_init(&bucket_lock, 0);
  bucket.tokens = 10;
  bucket.consumed = 4;
  bucket.epoch = 6;
  pthread_create(&consumer, 0, consumer_thread, 0);
  pthread_create(&refill, 0, refill_thread, 0);
  pthread_join(consumer, 0);
  pthread_create(&epoch, 0, epoch_thread, 0);
  pthread_join(refill, 0);
  pthread_join(epoch, 0);
  if (bucket.tokens != 15 || bucket.consumed != 7 || bucket.epoch != 7) {
    reach_error();
    abort();
  }
  return 0;
}
