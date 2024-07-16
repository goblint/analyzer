// PARAM:  --set lib.activated[+] sv-comp
#include <pthread.h>
#include "racemacros.h"

int data = 0;
int *p = &data, *q;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  access(*p);
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main() {
  create_threads(t);
  q = p;
  pthread_mutex_lock(&mutex);
  assert_racefree(*q);
  pthread_mutex_unlock(&mutex);
  return 0;
}
