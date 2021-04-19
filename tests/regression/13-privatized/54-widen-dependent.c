// PARAM: --enable ana.int.interval --enable exp.priv-distr-init
#include <pthread.h>
#include <assert.h>

// protection priv succeeds
// write fails due to [1,1] widen [0,1] -> [-inf,1]
// sensitive to eval and widen order!

int g = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg )
{
  pthread_mutex_lock(&A);
  while (g <= 0) {

  }
  assert(g > 0); // TODO
  g--;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;
  pthread_create(& tid, NULL, & worker, NULL);

  pthread_mutex_lock(&A);
  while (g >= 10) {

  }
  assert(g >= 0); // TODO
  g++;
  pthread_mutex_unlock(&A);
  return 0;
}
