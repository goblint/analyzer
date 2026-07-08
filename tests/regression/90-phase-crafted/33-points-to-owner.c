// CRAM
// points to owner: final assertion combines phase-sensitive values with points-to facts.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

struct Cell { int value; } cells[3];
struct Cell *read_slot;
struct Cell *write_slot;
struct Values { int read_value; int write_value; } values;
pthread_mutex_t point_lock;

void *reader_thread(void *arg) {
  int d = 0;
  for (int i = 0; i < 4; i++)
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
  for (int i = 0; i < 3; i++)
    d += 2;
  pthread_mutex_lock(&point_lock);
  write_slot = &cells[1];
  pthread_mutex_unlock(&point_lock);
  pthread_mutex_lock(&point_lock);
  /* GHOST writer_thread 1 */ values.write_value += d;
  pthread_mutex_unlock(&point_lock);
  pthread_mutex_lock(&point_lock);
  /* GHOST writer_thread 2 */ cells[2].value ^= 2;
  pthread_mutex_unlock(&point_lock);
  return 0;
}

int main(void) {
  pthread_t r, w;
  pthread_mutex_init(&point_lock, 0);
  cells[0].value = 43;
  cells[1].value = 53;
  cells[2].value = 0;
  values.read_value = 43;
  values.write_value = 53;
  read_slot = &cells[0];
  write_slot = &cells[1];
  pthread_create(&r, 0, reader_thread, 0);
  pthread_create(&w, 0, writer_thread, 0);
  pthread_join(r, 0);
  pthread_join(w, 0);
  if (read_slot != &cells[0] || write_slot != &cells[1] || values.read_value != 47 || values.write_value != 59) {
    reach_error();
    abort();
  }
  return 0;
}
