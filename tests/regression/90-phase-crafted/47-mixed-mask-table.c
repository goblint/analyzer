// CRAM
// Seqlock-style publication: writer advances sequence, sampler records a stable read.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t seq_lock;
struct SeqData { int seq; int value; int samples; } data;

void *writer_thread(void *arg) {
  pthread_mutex_lock(&seq_lock);
  /* GHOST writer_thread 1 */ data.seq += 1;
  pthread_mutex_unlock(&seq_lock);
  pthread_mutex_lock(&seq_lock);
  /* GHOST writer_thread 2 */ data.value += 7;
  pthread_mutex_unlock(&seq_lock);
  pthread_mutex_lock(&seq_lock);
  /* GHOST writer_thread 3 */ data.seq += 1;
  pthread_mutex_unlock(&seq_lock);
  return 0;
}

void *sampler_thread(void *arg) {
  int observations = 0;
  for (int i = 0; i < 2; i++)
    observations++;
  pthread_mutex_lock(&seq_lock);
  /* GHOST sampler_thread 1 */ data.samples += observations;
  pthread_mutex_unlock(&seq_lock);
  return 0;
}

int main(void) {
  pthread_t writer, sampler;
  pthread_mutex_init(&seq_lock, 0);
  data.seq = 0;
  data.value = 5;
  data.samples = 0;
  pthread_create(&sampler, 0, sampler_thread, 0);
  pthread_create(&writer, 0, writer_thread, 0);
  pthread_join(writer, 0);
  pthread_join(sampler, 0);
  if (data.seq != 2 || data.value != 12 || data.samples != 2) {
    reach_error();
    abort();
  }
  return 0;
}
