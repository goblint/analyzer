#include<stdio.h>
#include<pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  access(global);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main() {
  int i = __VERIFIER_nondet_int();
  create_threads(t);

  printf("Do the work? ");
  if (i) pthread_mutex_lock(&mutex1);

  printf("Now we do the work..\n");
  if (i) assert_racefree(global);

  printf("Work is completed...");
  if (i) pthread_mutex_unlock(&mutex1);

  join_threads(t);
  return 0;
}
