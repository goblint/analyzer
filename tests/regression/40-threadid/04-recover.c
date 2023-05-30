// PARAM: --set ana.activated[+] threadJoins
#include <pthread.h>
#include <stdio.h>

// not marked as a wrapper this time
int my_pthread_create(
  pthread_t *restrict thread,
  const pthread_attr_t *restrict attr,
  void *(*start_routine)(void *),
  void *restrict arg
) {
  return pthread_create(thread, attr, start_routine, arg);
}


int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main() {
  pthread_t id1;
  my_pthread_create(&id1, NULL, t_fun, 0);
  pthread_t id2;
  my_pthread_create(&id2, NULL, t_fun, 0);

  pthread_join(id1, NULL);


  g = 2; // RACE

  return 0;
}
