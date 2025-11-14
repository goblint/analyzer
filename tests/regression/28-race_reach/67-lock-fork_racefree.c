//PARAM: --set lib.activated[+] sv-comp
#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex); // must happen after unlock in main
  pthread_mutex_unlock(&mutex);
  assert_racefree(global); // no race
  return NULL;
}

int main(void) {
  pthread_mutex_lock(&mutex);
  create_threads(t);
  access(global);
  pthread_mutex_unlock(&mutex);
  return 0;
}
