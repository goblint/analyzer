#include <pthread.h>
#include <stdio.h>

// OS X has no spinlock
#ifndef __APPLE__
int g_spin = 0;
pthread_spinlock_t spin;
#endif

void *t_fun(void *arg) {
#ifndef __APPLE__
  if (!pthread_spin_lock(&spin)) { // ghost_1 = 1
    pthread_spin_unlock(&spin); // ghost_1 = 0
  }

  // if (!pthread_spin_trylock(&spin)) { // ghost_1 = 1
  //   pthread_spin_unlock(&spin); // ghost_1 = 0
  // }
#endif
  return NULL;
}

int main(void) {
#ifndef __APPLE__
  pthread_spin_init(&spin, PTHREAD_PROCESS_PRIVATE);
#endif
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_join (id, NULL);
  return 0;
}

