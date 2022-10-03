// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

int g = 0;

void *worker(void *arg )
{
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;
  pthread_create(& tid, NULL, & worker, NULL);

  while (g >= 10) {

  }
  __goblint_check(1); // reachable
  g++;
  __goblint_check(1); // reachable
  return 0;
}
