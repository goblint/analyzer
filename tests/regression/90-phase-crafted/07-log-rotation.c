// CRAM
// Log rotation uses checksums and slot movement instead of monotonically counting everything.
#include <pthread.h>

extern void abort(void);
void reach_error(void) { }

pthread_mutex_t log_lock;
struct LogState { int active; int archived; int generation; } log_state;

static int checksum(const int *buf, int n) {
  int sig = 0;
  for (int i = 0; i < n; i++)
    sig = (sig << 1) ^ buf[i];
  return sig;
}

void *compressor(void *arg) {
  int pages[] = {3, 1, 4};
  int sig = checksum(pages, 3);
  pthread_mutex_lock(&log_lock);
  /* GHOST compressor 1 */ log_state.archived ^= sig;
  pthread_mutex_unlock(&log_lock);
  pthread_mutex_lock(&log_lock);
  /* GHOST compressor 2 */ log_state.generation += 1;
  pthread_mutex_unlock(&log_lock);
  return 0;
}

void *rotator(void *arg) {
  int carried = 0;
  for (int i = 0; i < 3; i++)
    carried += i + 1;
  pthread_mutex_lock(&log_lock);
  /* GHOST rotator 1 */ log_state.active -= carried;
  pthread_mutex_unlock(&log_lock);
  return 0;
}

int main(void) {
  pthread_t c, r;
  pthread_mutex_init(&log_lock, 0);
  log_state.active = 30;
  log_state.archived = 9;
  log_state.generation = 2;
  pthread_create(&c, 0, compressor, 0);
  pthread_create(&r, 0, rotator, 0);
  pthread_join(c, 0);
  pthread_join(r, 0);
  if ((log_state.active ^ log_state.archived) != 27 || log_state.generation != 3) {
    reach_error();
    abort();
  }
  return 0;
}
