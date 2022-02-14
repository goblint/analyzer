//PARAM: --enable ana.int.interval --enable ana.int.enums --ana.base.privatization "write" -v

#include<pthread.h>

// Test case that shows how avoiding reading integral globals can reduce the number of solver evaluations.
// Avoiding to evaluate integral globals when setting them reduced the number of necessary evaluations from 62 to 20 in this test case.

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int glob = 10;

void* t_fun(void* ptr) {
  pthread_mutex_lock(&mutex);
  glob = 3;
  glob = 4;
  glob = 1;
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void bar() {
  glob = 2;
}

int main() {
  pthread_t t;

  pthread_create(&t, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex);
  bar();
  pthread_mutex_unlock(&mutex);
  pthread_join(t, NULL);
  assert(glob >= 1);
  assert(glob <= 10);
  return 0;
}
