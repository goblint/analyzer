#ifndef GOBLINT_NO_PTHREAD_ONCE
#include <pthread.h>

int pthread_once(pthread_once_t *once_control,void (*init_routine)(void)) __attribute__((goblint_stub));
int pthread_once(pthread_once_t *once_control,void (*init_routine)(void)) {
  // Not actually once, just call it
  int top;
  init_routine();
  return top;
}
#endif
