// PARAM: --set ana.activated[+] threadJoins --set ana.activated[+] threadCreateWrapper --set ana.thread.unique_thread_id_count 1
#include <pthread.h>
#include <stdio.h>

// not marked as a wrapper this time: instead, the two calls are given unique IDs
int my_pthread_create(
  pthread_t *restrict thread,
  const pthread_attr_t *restrict attr,
  void *(*start_routine)(void *),
  void *restrict arg
) {
  return pthread_create(thread, attr, start_routine, arg);
}

// uncomment to remove the wrapper
// #define my_pthread_create pthread_create

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
  my_pthread_create(&id1, NULL, t_fun, NULL);
  pthread_t id2;
  my_pthread_create(&id2, NULL, t_fun, NULL);

  pthread_join(id1, NULL);
  pthread_join(id2, NULL);

  g = 2; // NORACE

  return 0;
}
