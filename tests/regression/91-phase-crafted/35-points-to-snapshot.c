// CRAM
// points-to snapshot: nondeterministic loop bounds and pointer-valued expected facts.
#include <pthread.h>

extern int __VERIFIER_nondet_int(void);
extern void abort(void);
void reach_error(void) { }

struct Cell { int value; } cells[3];
struct Cell *read_slot;
struct Cell *write_slot;
struct Values { int read_value; int write_value; } values;
int expected[2];
int read_steps;
int write_steps;
pthread_mutex_t point_lock;

void *reader_thread(void *arg) {
  int d = 0;
  for (int i = 0; i < read_steps; i++)
    d += 1;
  pthread_mutex_lock(&point_lock);
  read_slot = &cells[0];
  pthread_mutex_unlock(&point_lock);
  pthread_mutex_lock(&point_lock);
  /* GHOST reader_thread 1 */ values.read_value += d;
  pthread_mutex_unlock(&point_lock);
  return 0;
}

void *writer_thread(void *arg) {
  int d = 0;
  for (int i = 0; i < write_steps; i++)
    d += 2;
  pthread_mutex_lock(&point_lock);
  write_slot = &cells[1];
  pthread_mutex_unlock(&point_lock);
  pthread_mutex_lock(&point_lock);
  /* GHOST writer_thread 1 */ values.write_value += d;
  pthread_mutex_unlock(&point_lock);
  pthread_mutex_lock(&point_lock);
  /* GHOST writer_thread 2 */ cells[2].value ^= 4;
  pthread_mutex_unlock(&point_lock);
  return 0;
}

int main(void) {
  pthread_t r, w;
  pthread_mutex_init(&point_lock, 0);
  expected[0] = 45;
  expected[1] = 55;
  cells[0].value = expected[0];
  cells[1].value = expected[1];
  cells[2].value = 2;
  values.read_value = expected[0];
  values.write_value = expected[1];
  read_steps = 1;
  write_steps = 5;
  read_slot = &cells[0];
  write_slot = &cells[1];
  int *read_expected = &expected[0];
  int *write_expected = &expected[1];
  pthread_create(&w, 0, writer_thread, 0);
  pthread_create(&r, 0, reader_thread, 0);
  pthread_join(r, 0);
  pthread_join(w, 0);
  if (read_slot != &cells[0] || write_slot != &cells[1] || values.read_value != *read_expected + read_steps || values.write_value != *write_expected + 2 * write_steps) {
    reach_error();
    abort();
  }
  return 0;
}
