#include <pthread.h>
#include <assert.h>

int g = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg )
{
  while (1) {
    pthread_mutex_lock(&A);
    g = 1000;
    assert(g != 0);
    if (g > 0) {
      g--;
    }
    pthread_mutex_unlock(&A);
    // extra mutex makes mine-W more precise than mine-lazy
    pthread_mutex_lock(&B);
    pthread_mutex_unlock(&B);
  }
  return NULL;
}

int main(int argc , char **argv )
{
  pthread_t tid;
  pthread_create(& tid, NULL, & worker, NULL);
  return 0;
}
