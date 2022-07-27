// PARAM: --set ana.activated[+] "'var_eq'"
// Copy of 04/45 with var_eq enabled
#include <pthread.h>
#include <stdio.h>
#include <assert.h>

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *p = (int *) arg;
  pthread_mutex_lock(&mutex1);
  (*p)++;
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  int i = 0;
  pthread_create(&id, NULL, t_fun, (void *) &i);
  pthread_mutex_lock(&mutex2);
  assert(i == 0); // UNKNOWN!
  pthread_mutex_unlock(&mutex2);
  pthread_join (id, NULL);
  return 0;
}
