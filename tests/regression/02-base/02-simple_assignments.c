#include<pthread.h>
#include<assert.h>

void *t_fun(void *arg) {
  return NULL;
}

int glob1 = 5;
int glob2 = 7;

int main() {
  int i = 3;
  pthread_t id;
  pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER; 

  // Create the thread
  pthread_create(&id, NULL, t_fun, NULL);

  // Simple assignments to only locals
  assert(i == 3);
  i = 9;
  assert(i == 9);

  // simple assignments to globals
  pthread_mutex_lock(&m);
  glob1 = 5;
  assert(glob1 == 5);
  glob2 = 5;
  assert(glob2 == 5); // UNKNOWN
  pthread_mutex_unlock(&m);

  return 0;
}
