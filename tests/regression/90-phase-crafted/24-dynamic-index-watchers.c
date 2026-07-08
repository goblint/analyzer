// CRAM
// dynamic index watchers: nondeterministic number of ghost-free background workers, irregular joins.
#include <pthread.h>

extern int __VERIFIER_nondet_int(void);
extern void abort(void);
void reach_error(void) { }

pthread_mutex_t data_lock;
pthread_mutex_t noise_lock;
struct State { int a; int b; int tag; } state;
int noise_total;
int workers;

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
  for (int i = 0; i < 2; i++)
    delta += 1;
  pthread_mutex_lock(&data_lock);
  /* GHOST producer 1 */ state.a += delta;
  pthread_mutex_unlock(&data_lock);
  pthread_mutex_lock(&data_lock);
  /* GHOST producer 2 */ state.tag ^= 4;
  pthread_mutex_unlock(&data_lock);
  return 0;
}

void *consumer(void *arg) {
  int delta = 0;
  for (int i = 0; i < 2; i++)
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
  state.a = 54;
  state.b = 64;
  state.tag = 4;
  noise_total = 0;
  workers = __VERIFIER_nondet_int();
  if (workers < 1)
    workers = 1;
  if (workers > 4)
    workers = 4;
  pthread_create(&noise[0], 0, noise_worker, 0);
  pthread_create(&p, 0, producer, 0);
  if (workers > 1)
    pthread_create(&noise[1], 0, noise_worker, 0);
  pthread_create(&c, 0, consumer, 0);
  if (workers > 2)
    pthread_create(&noise[2], 0, noise_worker, 0);
  if (workers > 3)
    pthread_create(&noise[3], 0, noise_worker, 0);
  pthread_join(p, 0);
  if (workers > 3)
    pthread_join(noise[3], 0);
  if (workers > 1)
    pthread_join(noise[1], 0);
  pthread_join(noise[0], 0);
  if (workers > 2)
    pthread_join(noise[2], 0);
  pthread_join(c, 0);
  if (state.a != 56 || state.b != 68 || state.tag != 0) {
    reach_error();
    abort();
  }
  return 0;
}
