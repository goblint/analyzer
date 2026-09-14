// CRAM
// Index maintenance: broad sequential scaffolding, few shared phase distinctions.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

struct IndexPlan { int levels; int fanout; int stale_pages; int segments; };
struct IndexState { int live_pages; int segments; int checksum; } index_state;
pthread_mutex_t index_lock;

static int estimate_pages(struct IndexPlan p) {
  int pages = 1;
  for (int level = 0; level < p.levels; level++)
    pages *= p.fanout;
  return pages - p.stale_pages;
}

static int choose_segments(struct IndexPlan p) {
  int s = 0;
  for (int i = 0; i < p.segments; i++)
    s += (i % 2) + 1;
  return s;
}

static int local_checksum(struct IndexPlan p) {
  int x = p.levels;
  x = x * 31 + p.fanout;
  x = x * 31 + p.stale_pages;
  x = x * 31 + p.segments;
  return x & 15;
}

void *compactor(void *arg) {
  struct IndexPlan p = {3, 2, 1, 2};
  int pages = 1;
  for (int level = 0; level < p.levels; level++)
    pages *= p.fanout;
  pages -= p.stale_pages;
  pthread_mutex_lock(&index_lock);
  /* GHOST compactor 1 */ index_state.live_pages += pages;
  pthread_mutex_unlock(&index_lock);
  return 0;
}

void *segment_builder(void *arg) {
  struct IndexPlan p = {2, 3, 0, 3};
  int segs = 0;
  for (int i = 0; i < p.segments; i++)
    segs += (i % 2) + 1;
  int sig = p.levels;
  sig = sig * 31 + p.fanout;
  sig = sig * 31 + p.stale_pages;
  sig = sig * 31 + p.segments;
  sig = sig & 15;
  pthread_mutex_lock(&index_lock);
  /* GHOST segment_builder 1 */ index_state.segments += segs;
  pthread_mutex_unlock(&index_lock);
  pthread_mutex_lock(&index_lock);
  /* GHOST segment_builder 2 */ index_state.checksum ^= sig;
  pthread_mutex_unlock(&index_lock);
  return 0;
}

int main(void) {
  pthread_t c, s;
  pthread_mutex_init(&index_lock, 0);
  index_state.live_pages = 40;
  index_state.segments = 3;
  index_state.checksum = 10;
  pthread_create(&c, 0, compactor, 0);
  pthread_create(&s, 0, segment_builder, 0);
  pthread_join(c, 0);
  pthread_join(s, 0);
  if (index_state.live_pages != 47 || index_state.segments != 7 || index_state.checksum != 14) {
    reach_error();
    abort();
  }
  return 0;
}
