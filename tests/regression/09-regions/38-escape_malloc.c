// PARAM: --set ana.activated[+] region
// Copy of 09/34 with a malloc'ed variable (works without region).
#include <pthread.h>
#include <stdlib.h>

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *p = (int *) arg;
  pthread_mutex_lock(&mutex1);
  (*p)++; // TODO RACE!
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  int *q = (int*) malloc(sizeof(int));
  pthread_create(&id, NULL, t_fun, (void *) q);
  pthread_mutex_lock(&mutex2);
  (*q)++; // TODO RACE!
  pthread_mutex_unlock(&mutex2);
  pthread_join (id, NULL);
  return 0;
}
