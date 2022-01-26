// copy only necessary types (as abstractly as possible) instead of include to avoid 64bit vs 32bit CIL merge warnings
// #include <pthread.h>

// copied from pthread.h
typedef int pthread_once_t;

int pthread_once(pthread_once_t *once_control,void (*init_routine)(void)) __attribute__((goblint_stub));
int pthread_once(pthread_once_t *once_control,void (*init_routine)(void)) {
  // Not actually once, just call it
  int top;
  init_routine();
  return top;
}
