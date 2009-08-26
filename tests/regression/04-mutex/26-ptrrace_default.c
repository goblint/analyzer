// SKIP: Don't see the point in such elaborate invalidations for unknown
// functions since we warn unsound on this anyway.

#include <pthread.h>

/**
 * foo /migh/ call the argument function
 */
extern void foo(void (*)(void));

int glob;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex2);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

void reset_glob(void) {
  pthread_mutex_lock(&mutex1);
  glob=glob+1; // RACE!
  pthread_mutex_unlock(&mutex1);
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  /* reset_glob(); */
  foo(reset_glob);
  pthread_join (id, NULL);
  return 0;
}
