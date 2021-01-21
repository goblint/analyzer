#include<pthread.h>
#include<assert.h>

struct lock {
  pthread_mutex_t mutex;
};

int glob1 = 5;
struct lock lock1 = {.mutex = PTHREAD_MUTEX_INITIALIZER};
struct lock lock2 = {.mutex = PTHREAD_MUTEX_INITIALIZER};

void *t_fun(void *arg) {
  int t;
  pthread_mutex_lock(&lock1.mutex);
  t = glob1;
  assert(t == 5); // UNKNOWN
  glob1 = -10;
  assert(glob1 == -10); // UNKNOWN
  glob1 = t;
  pthread_mutex_unlock(&lock1.mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(glob1 == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(&lock2.mutex);
  glob1++;
  assert(glob1 == 6); // UNKNOWN
  glob1--;
  pthread_mutex_unlock(&lock2.mutex);
  pthread_join (id, NULL);
  return 0;
}
