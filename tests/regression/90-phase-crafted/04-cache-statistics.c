// CRAM
// Cache maintenance mixes looped histogram updates and bit flags.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t cache_lock;
struct Cache { int hot; int cold; int flags; } cache;

void *reader(void *arg) {
  int hot = 0;
  for (int bucket = 0; bucket < 4; bucket++)
    hot += bucket + 1;
  pthread_mutex_lock(&cache_lock);
  /* GHOST reader 1 */ cache.hot += hot;
  pthread_mutex_unlock(&cache_lock);
  pthread_mutex_lock(&cache_lock);
  /* GHOST reader 2 */ cache.flags |= 2;
  pthread_mutex_unlock(&cache_lock);
  return 0;
}

void *evictor(void *arg) {
  int cold = 0;
  for (int clock = 5; clock > 0; clock--)
    if (clock & 1)
      cold++;
  pthread_mutex_lock(&cache_lock);
  /* GHOST evictor 1 */ cache.cold += cold;
  pthread_mutex_unlock(&cache_lock);
  pthread_mutex_lock(&cache_lock);
  /* GHOST evictor 2 */ cache.hot -= 1;
  pthread_mutex_unlock(&cache_lock);
  pthread_mutex_lock(&cache_lock);
  /* GHOST evictor 3 */ cache.flags |= 4;
  pthread_mutex_unlock(&cache_lock);
  return 0;
}

int main(void) {
  pthread_t r, e;
  pthread_mutex_init(&cache_lock, 0);
  cache.hot = 40;
  cache.cold = 8;
  cache.flags = 1;
  pthread_create(&r, 0, reader, 0);
  pthread_create(&e, 0, evictor, 0);
  pthread_join(r, 0);
  pthread_join(e, 0);
  if (!(cache.hot == 49 && cache.cold == 11 && (cache.flags & 7) == 7)) {
    reach_error();
    abort();
  }
  return 0;
}
