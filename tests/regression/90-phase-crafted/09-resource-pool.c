// CRAM
// Resource pool has three phases for a borrower and one for a cleaner.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t pool_lock;
struct Resources { int free; int leased; int dirty; } resources;

void *borrower(void *arg) {
  int n = 1;
  for (int i = 1; i <= 3; i++)
    n += i % 2;
  pthread_mutex_lock(&pool_lock);
  /* GHOST borrower 1 */ resources.free -= n;
  pthread_mutex_unlock(&pool_lock);
  pthread_mutex_lock(&pool_lock);
  /* GHOST borrower 2 */ resources.leased += n;
  pthread_mutex_unlock(&pool_lock);
  pthread_mutex_lock(&pool_lock);
  /* GHOST borrower 3 */ resources.dirty += 1;
  pthread_mutex_unlock(&pool_lock);
  return 0;
}

void *cleaner(void *arg) {
  int cleaned = 0;
  while (cleaned < 2)
    cleaned++;
  pthread_mutex_lock(&pool_lock);
  /* GHOST cleaner 1 */ resources.dirty -= cleaned;
  pthread_mutex_unlock(&pool_lock);
  return 0;
}

void *returner(void *arg) {
  pthread_mutex_lock(&pool_lock);
  /* GHOST returner 1 */ resources.free += 4;
  pthread_mutex_unlock(&pool_lock);
  pthread_mutex_lock(&pool_lock);
  /* GHOST returner 2 */ resources.leased -= 4;
  pthread_mutex_unlock(&pool_lock);
  return 0;
}

int main(void) {
  pthread_t b, c, r;
  pthread_mutex_init(&pool_lock, 0);
  resources.free = 10;
  resources.leased = 6;
  resources.dirty = 5;
  pthread_create(&b, 0, borrower, 0);
  pthread_create(&c, 0, cleaner, 0);
  pthread_create(&r, 0, returner, 0);
  pthread_join(b, 0);
  pthread_join(c, 0);
  pthread_join(r, 0);
  if (resources.free != 11 || resources.leased != 5 || resources.dirty != 4) {
    reach_error();
    abort();
  }
  return 0;
}
