// PARAM: --set solver "'new'" --set ana.int.interval true
#include<pthread.h>
#include<assert.h>

int glob = 0;
pthread_mutex_t mtx = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mtx);
  glob++;
  pthread_mutex_unlock(&mtx);
  return NULL;
}

int main() {
  int i = 3;
  pthread_t id;

  assert(glob == 0);

  // Create the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // Simple assignments to only locals
  assert(i == 3);
  i = 9;
  assert(i == 9);

  glob = 10;

  i = glob;
  assert(i >= 0);
  assert(i > 100); // UNKNOWN

  return 0;
}
