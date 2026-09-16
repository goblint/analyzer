// CRAM
// pointer lock journal: workers lock and unlock mutexes through pointer variables.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t primary_lock;
pthread_mutex_t secondary_lock;
struct Record { int x; int y; int stamp; } record;

void *left_worker(void *arg) {
  pthread_mutex_t *lockp = &primary_lock;
  int d = 0;
  for (int i = 0; i < 6; i++)
    d += 1;
  pthread_mutex_lock(lockp);
  /* GHOST left_worker 1 */ record.x += d;
  pthread_mutex_unlock(lockp);
  lockp = &secondary_lock;
  pthread_mutex_lock(lockp);
  /* GHOST left_worker 2 */ record.stamp |= 2;
  pthread_mutex_unlock(lockp);
  return 0;
}

void *right_worker(void *arg) {
  pthread_mutex_t *lockp = &primary_lock;
  int d = 0;
  for (int i = 0; i < 2; i++)
    d += 2;
  pthread_mutex_lock(lockp);
  /* GHOST right_worker 1 */ record.y += d;
  pthread_mutex_unlock(lockp);
  return 0;
}

int main(void) {
  pthread_t l, r;
  pthread_mutex_init(&primary_lock, 0);
  pthread_mutex_init(&secondary_lock, 0);
  record.x = 39;
  record.y = 49;
  record.stamp = 0;
  pthread_create(&l, 0, left_worker, 0);
  pthread_create(&r, 0, right_worker, 0);
  pthread_join(l, 0);
  pthread_join(r, 0);
  if (!(record.x == 45 && record.y == 53 && record.stamp != 0)) {
    reach_error();
    abort();
  }
  return 0;
}
