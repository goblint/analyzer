#include <pthread.h>
#include <assert.h>

int g = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

int pqueue_init()
{
  g = 0;
  pthread_mutex_init(&A, NULL);
  return (0);
}

int pqueue_put()
{
  pthread_mutex_lock(&A);
  g++; // overflow gives < 0 values!
  pthread_mutex_unlock(&A);
  return (1);
}

int pqueue_get()
{
  int got = 0;
  pthread_mutex_lock(&A);
  while (g <= 0) {
    // g should not be just 0, unsoundness in old
    __goblint_check(g == 0); // UNKNOWN (no interval, with overflow)
    // this assert should not refine!
  }
  // g should not be Error int, unsoundness in global
  __goblint_check(g != 0);
  if (g > 0) {
    g--;
    got = 1;
    pthread_mutex_unlock(&A);
  } else {
    pthread_mutex_unlock(&A);
  }
  return (got);
}

void *worker(void *arg )
{
  while (1) {
    pqueue_get();
  }
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;

  pqueue_init();
  pthread_create(& tid, NULL, & worker, NULL);

  for (int i = 1; i < argc; i++) {
    pqueue_put();
  }
  return 0;
}
