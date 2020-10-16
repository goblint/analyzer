extern int __VERIFIER_nondet_int();

#include<stdio.h>
#include<pthread.h>
#include<assert.h>

int glob;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&m);
  glob++; // RACE!
  pthread_mutex_unlock(&m);
  return NULL;
}

int main() {
  int i = __VERIFIER_nondet_int();
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  printf("Do the work? ");
  if (i)
    pthread_mutex_lock(&m);
  printf("Now we do the work..\n");
  i++;
  if (i)
    glob++; // RACE!
  printf("Work is completed...");
  i--;
  if (i)
    pthread_mutex_unlock(&m);

  return 0;
}
