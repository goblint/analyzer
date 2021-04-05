// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

// global priv succeeds
// global-history fails due to [1,+inf] widen ([1,+inf] join [0,+inf]) -> [-inf,+inf]
// sensitive to eval and widen order!

int g = 0;
int limit; // unknown

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg )
{
  // just for going to multithreaded mode
  return NULL;
}

int put() {
  pthread_mutex_lock(&A);
  while (g >= limit) { // problematic widen

  }
  assert(g >= 0);
  g++;
  pthread_mutex_unlock(&A);
}

int main(int argc , char **argv )
{
  pthread_t tid;
  pthread_create(& tid, NULL, & worker, NULL);

  int r;
  limit = r; // only problematic if limit unknown

  while (1) {
    // only problematic if not inlined
    put();
  }
  return 0;
}
