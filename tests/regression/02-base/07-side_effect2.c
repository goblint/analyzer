#include<pthread.h>
#include<assert.h>

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER; 
int glob1 = 5;
int glob2 = 7;
int glob3 = 7;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  glob3 = 8;
  assert(glob3 == 8); // UNKNOWN
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  pthread_t id;
  glob3 = 7;
  assert(glob3 == 7);
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&m);

  glob1 = 5;
  assert(glob1 == 5);

  glob2 = 5;
  assert(glob2 == 5); // UNKNOWN

  glob3 = 7;
  assert(glob3 == 7); // UNKNOWN

  pthread_mutex_unlock(&m);
  return 0;
}
