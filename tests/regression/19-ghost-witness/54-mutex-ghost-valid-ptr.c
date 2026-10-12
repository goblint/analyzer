// CRAM
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <stdio.h>

int g1 = 37, g2 = 42;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1); // ghost_1 = 1
  g1 = 42; g1 = 37;
  pthread_mutex_unlock(&mutex1); // ghost_1 = 0
  pthread_mutex_lock(&mutex2); // ghost_2 = 1
  g2 = 37; g2 = 42;
  pthread_mutex_unlock(&mutex2); // ghost_2 = 0
  return NULL;
}

int main(void) {
  int i = __VERIFIER_nondet_int();
  pthread_t id;
  pthread_mutex_t *m = &mutex1;
  pthread_mutex_lock(m); // ghost_1 = 1
  g1 = 42; g1 = 37;
  pthread_mutex_unlock(m); // ghost_1 = 0

  m = &mutex2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_mutex_lock(m); // ghost_2 = 1
  g2 = 37; g2 = 42;
  pthread_mutex_unlock(m); // ghost_2 = 0
  pthread_join (id, NULL);
  return 0;
}