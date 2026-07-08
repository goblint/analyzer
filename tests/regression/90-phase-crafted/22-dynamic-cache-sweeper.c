// CRAM
// dynamic cache sweeper: nondeterministic number of ghost-free background workers, irregular joins.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t data_lock;
pthread_mutex_t noise_lock;
struct State { int a; int b; int tag; } state;
int noise_total;

void *noise_worker(void *arg) {
  int local = 0;
  for (int i = 0; i < 3; i++)
    local += i + 1;
  pthread_mutex_lock(&noise_lock);
  noise_total += local;
  pthread_mutex_unlock(&noise_lock);
  return 0;
}

void *producer(void *arg) {
  int delta = 0;
  for (int i = 0; i < 4; i++)
    delta += 1;
  pthread_mutex_lock(&data_lock);
  /* GHOST producer 1 */ state.a += delta;
  pthread_mutex_unlock(&data_lock);
  pthread_mutex_lock(&data_lock);
  /* GHOST producer 2 */ state.tag ^= 2;
  pthread_mutex_unlock(&data_lock);
  return 0;
}

void *consumer(void *arg) {
  int delta = 0;
  for (int i = 0; i < 3; i++)
    delta += 2;
  pthread_mutex_lock(&data_lock);
  /* GHOST consumer 1 */ state.b += delta;
  pthread_mutex_unlock(&data_lock);
  return 0;
}

int main(void) {
  pthread_t p, c, noise[4];
  pthread_mutex_init(&data_lock, 0);
  pthread_mutex_init(&noise_lock, 0);
  state.a = 52;
  state.b = 62;
  state.tag = 2;
  noise_total = 0;
  int workers = 2 + (22 % 3);
  for (int i = 0; i < workers; i++)
    pthread_create(&noise[i], 0, noise_worker, 0);
  pthread_create(&p, 0, producer, 0);
  pthread_create(&c, 0, consumer, 0);
  for (int i = workers - 1; i >= 0; i--)
    pthread_join(noise[i], 0);
  pthread_join(p, 0);
  pthread_join(c, 0);
  if (state.a != 56 || state.b != 68 || state.tag != 0) {
    reach_error();
    abort();
  }
  return 0;
}
