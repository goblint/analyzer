// PARAM: --set ana.activated[+] "'region'"
// Regression: pthread argument properly tracked as escaped in region analysis.
// The region analysis must treat a local variable passed via pthread argument
// as escaped (global), enabling proper race detection.
// The NORACE variant (same mutex) shows no false positives.
// See also 09/34 which is the RACE variant with different mutexes.
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *p = (int *) arg;
  pthread_mutex_lock(&mutex);
  (*p)++; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  int i;
  pthread_create(&id, NULL, t_fun, (void *) &i);
  pthread_mutex_lock(&mutex);
  i++; // NORACE
  pthread_mutex_unlock(&mutex);
  pthread_join(id, NULL);
  return 0;
}
