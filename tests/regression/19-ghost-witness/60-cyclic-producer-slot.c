// CRAM
#include <pthread.h>

extern int __VERIFIER_nondet_int(void);
void reach_error(void) { }

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int full = 0;
int payload = 0;

void *producer(void *arg) {
  while (__VERIFIER_nondet_int()) {
    pthread_mutex_lock(&mutex);
    payload = 42;
    full = 1;
    pthread_mutex_unlock(&mutex);

    pthread_mutex_lock(&mutex);
    full = 0;
    payload = 0;
    pthread_mutex_unlock(&mutex);
  }

  return NULL;
}

void *consumer(void *arg) {
  pthread_mutex_lock(&mutex);
  if (full) {
    if (payload != 42)
      reach_error();
  }
  pthread_mutex_unlock(&mutex);

  return NULL;
}

int main(void) {
  pthread_t producer_thread;
  pthread_t consumer_thread;

  pthread_create(&producer_thread, NULL, producer, NULL);
  pthread_create(&consumer_thread, NULL, consumer, NULL);
  pthread_join(producer_thread, NULL);
  pthread_join(consumer_thread, NULL);
  return 0;
}
