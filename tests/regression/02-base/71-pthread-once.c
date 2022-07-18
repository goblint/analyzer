//PARAM: --disable sem.unknown_function.spawn
#include <pthread.h>
#include <assert.h>

int g;
pthread_once_t once = PTHREAD_ONCE_INIT;

void t_fun() {
  assert(1); // reachable!
  return NULL;
}

int main() {
  pthread_once(&once,t_fun);
  return 0;
}
